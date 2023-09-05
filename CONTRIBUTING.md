# Contributing to Rose

## Prerequisites

Make sure to have these tools installed:

- [Git][]
- [Make][]
- [Node][]
- [Rust][]

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

## Clean

Sometimes old build artifacts can hide errors. To clean your build:

```
make clean
```

This doesn't clean everything; it keeps around downloaded files and Rust's
`target` directory. You should be able to run `make all` right after it without
an Internet connection.

## Site

To develop the website locally:

```
make site-deps && npm run --workspace=@rose-lang/site dev
```

Or, if you want to host on your local network, e.g. to test on your phone:

```
make site-deps && npm run --workspace=@rose-lang/site dev -- --host
```

[git]: https://git-scm.com/downloads
[make]: https://en.wikipedia.org/wiki/Make_(software)
[node]: https://nodejs.org/en/download
[rust]: https://www.rust-lang.org/tools/install
