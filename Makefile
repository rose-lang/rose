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

# install `wasm-bindgen` CLI locally; we put this in a separate target from
# `rust` so that in CI we can skip this step by downloading a prebuilt Linux
# binary and using Make's change tracking
.cargo/bin/wasm-bindgen:
	cargo install --root=.cargo --version=0.2.84 wasm-bindgen-cli

# install additional Rust stuff that we need
rust: .cargo/bin/wasm-bindgen
	rustup target add wasm32-unknown-unknown

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
