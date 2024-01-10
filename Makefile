# build everything
build: packages

# run all tests
test: test-rust test-js

# run other checks
check: prettier
	cargo fmt --check
	cargo clippy

# delete build artifacts, but not dependencies or downloaded files
clean:
	git clean -Xdf crates packages -e '!node_modules'

# do everything
all: build test check

### Rust

# install additional Rust stuff that we need
rust:
	cargo install --root=.cargo --version=0.2.87 wasm-bindgen-cli

# export TypeScript bindings from Rust types
bindings:
	cargo test export_bindings_

# compile Rust to WebAssembly
wbg: rust
	cargo build --package=rose-web --target=wasm32-unknown-unknown --release
	cargo build --package=rose-web --no-default-features -Z build-std=std,panic_abort -Z build-std-features=panic_immediate_abort --target wasm32-unknown-unknown --profile web
	.cargo/bin/wasm-bindgen --target=web --out-dir=packages/wasm/wbg target/wasm32-unknown-unknown/release/rose_web.wasm
	.cargo/bin/wasm-bindgen --target=web --out-dir=packages/wasm/dist/wbg target/wasm32-unknown-unknown/web/rose_web.wasm

# run Rust tests
test-rust:
	cargo test --quiet

### JavaScript

# fetch JavaScript dependencies
npm:
	npm i

# check Prettier formatting
prettier: npm
	npx prettier --check .

# build `packages/`
packages: core site wasm

# run JavaScript tests
test-js: test-core test-site

## `packages/core`

# build
core: npm wasm
	cp README.md packages/core
	npm run --workspace=rose build

# test
test-core: npm wasm
	npm run --workspace=rose test -- run --no-threads

## `packages/site`

site-deps: npm core

# build
site: site-deps
	npm run --workspace=@rose-lang/site build

# test
test-site: site-deps
	npm run --workspace=@rose-lang/site test -- run --no-threads

## `packages/wasm`

# build
wasm: npm bindings wbg
	npx wasm-opt packages/wasm/dist/wbg/rose_web_bg.wasm -Oz -o packages/wasm/dist/wbg/rose_web_bg.wasm
	npm run --workspace=@rose-lang/wasm build
	node bindings.js
