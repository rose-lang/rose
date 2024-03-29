# Contributing to Rose

## Prerequisites

Make sure to have these tools installed:

- [Git][]
- [Make][]
- [Node][]
- [Rust][]

## Setup

Once you've installed all prerequisites, clone this repo.

```sh
git clone https://github.com/rose-lang/rose
```

Then open a terminal in your clone of it; for instance, if you cloned it via the terminal, run this command:

```sh
cd rose/
```

Now you should be able to run everything using Make:

```sh
make all
```

## Build

To just produce all build artifacts:

```sh
make
```

## Test

To run all tests:

```sh
make test
```

## Check

To run any additional checks:

```sh
make check
```

## Clean

Sometimes old build artifacts can hide errors. To clean your build:

```sh
make clean
```

This doesn't clean everything; it keeps around downloaded files and Rust's
`target` directory. You should be able to run `make all` right after it without
an Internet connection.

## Site

To develop the website locally:

```sh
make site-deps && npm run --workspace=@rose-lang/site dev
```

Or, if you want to host on your local network, e.g. to test on your phone:

```sh
make site-deps && npm run --workspace=@rose-lang/site dev -- --host
```

[git]: https://git-scm.com/downloads
[make]: https://en.wikipedia.org/wiki/Make_(software)
[node]: https://nodejs.org/en/download
[rust]: https://www.rust-lang.org/tools/install
