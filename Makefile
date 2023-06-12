# build everything
build: packages

# run all tests
test: yarn wasm
	cargo test
	yarn workspace @rose-lang/core test run

# run other checks
check: prettier
	cargo fmt --check
	cargo clippy

# do everything
all: build test check

## Rust

# install additional Rust stuff that we need
rust:
	rustup target add wasm32-unknown-unknown
	cargo install --root=.cargo --version=0.2.84 wasm-bindgen-cli

# export TypeScript bindings from Rust types
bindings:
	cargo test export_bindings_
	./bindings.sh

# compile Rust to WebAssembly
wbg: rust
	cargo build --package=rose-web --target=wasm32-unknown-unknown --release
	.cargo/bin/wasm-bindgen --target=web --out-dir=packages/wasm/wbg target/wasm32-unknown-unknown/release/rose_web.wasm

## JavaScript

# fetch JavaScript dependencies
yarn:
	yarn

# check Prettier formatting
prettier: yarn
	npx prettier --check .

# build `packages/core/`
core: yarn wasm
	yarn workspace @rose-lang/core build

# build `packages/vscode/`
vscode: yarn
	yarn workspace rose package

# build `packages/wasm/`
wasm: yarn bindings wbg
	yarn workspace @rose-lang/wasm build

# build `packages/`
packages: core vscode wasm
