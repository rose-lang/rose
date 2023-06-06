# Contributing to Rose

## Prerequisites

Make sure to have these tools installed:

- [Git](https://git-scm.com/downloads)

- [Rust](https://www.rust-lang.org/tools/install)

  - the WebAssembly target for Rust:

    ```
    rustup target add wasm32-unknown-unknown
    ```

  - `wasm-bindgen` CLI v0.2.84 installed globally:

    ```
    cargo install --version=0.2.84 wasm-bindgen-cli
    ```

- [Node.js](https://nodejs.org/en/download) v16-v18

  - [Yarn](https://classic.yarnpkg.com/lang/en/docs/install) v1.x

## Setup

Once you've installed all prerequisites, clone this repo.

```
git clone https://github.com/rose-lang/rose
```

Then open a terminal in your clone of it; for instance, if you cloned it via the terminal, run this command:

```
cd rose/
```

Next, install the dependencies from npm:

```
yarn
```

Then, to build all packages, run in the root directory:

```
yarn wasm
```

and then

```
yarn build
```

## Test

To run all tests:

```
yarn test
```
