#!/usr/bin/env bash
set -eo pipefail

# install prebuilt `wasm-bindgen` CLI binary for Linux
VERSION=0.2.84
rustup target add wasm32-unknown-unknown
wget https://github.com/rustwasm/wasm-bindgen/releases/download/$VERSION/wasm-bindgen-$VERSION-x86_64-unknown-linux-musl.tar.gz
tar -xzf wasm-bindgen-$VERSION-x86_64-unknown-linux-musl.tar.gz
mkdir -p .cargo/bin
mv wasm-bindgen-$VERSION-x86_64-unknown-linux-musl/wasm-bindgen .cargo/bin
rm -r wasm-bindgen-$VERSION-x86_64-unknown-linux-musl*
