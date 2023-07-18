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
	git clean -Xdf crates packages -e '!node_modules' -e '!*.png'

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
	cargo test

### JavaScript

# fetch JavaScript dependencies
yarn:
	yarn

# check Prettier formatting
prettier: yarn
	npx prettier --check .

# build `packages/`
packages: core site vscode wasm

# run JavaScript tests
test-js: test-core

## `packages/core`

# build
core: yarn wasm
	yarn workspace rose build

# test
test-core: yarn wasm
	yarn workspace rose test run --no-threads

## `packages/site`

site-deps: yarn core

site: site-deps
	yarn workspace @rose-lang/site build

## `packages/vscode`

# fetch encircled icon
packages/vscode/encircled-rose.png:
	node fetch.js https://github.com/rose-lang/rose-icons/raw/efcc218832d65970a47bed597ee11cecd3d1cc3c/png/encircled-rose.png $@

# fetch plain icon
packages/vscode/plain-rose.png:
	node fetch.js https://github.com/rose-lang/rose-icons/raw/efcc218832d65970a47bed597ee11cecd3d1cc3c/png/plain-rose.png $@

# build
vscode: yarn packages/vscode/encircled-rose.png packages/vscode/plain-rose.png
	yarn workspace rose-vscode build

## `packages/wasm`

# build
wasm: yarn bindings wbg
	yarn workspace @rose-lang/wasm build
	node bindings.js
