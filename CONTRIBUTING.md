# Contributing to Rose

## Prerequisites

Make sure to have these tools installed:

- [Git](https://git-scm.com/downloads)
- [Make](https://www.gnu.org/software/make/)
- [Rust](https://www.rust-lang.org/tools/install)
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

Now you should be able to run everything using Make:

```
make all
```

## Build

To just produce all build artifacts:

```
make
```

## Test

To run all tests:

```
make test
```

## Check

To run any additional checks:

```
make check
```
