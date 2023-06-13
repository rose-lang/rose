# Contributing to Rose

## Prerequisites

Make sure to have these tools installed:

- [curl][]
- [Git][]
- [Make][]
- [Rust][]
- [Node.js][] v16-v18
  - [Yarn][] v1.x

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

[curl]: https://curl.se/
[git]: https://git-scm.com/downloads
[make]: https://en.wikipedia.org/wiki/Make_(software)
[node.js]: https://nodejs.org/en/download
[rust]: https://www.rust-lang.org/tools/install
[yarn]: https://classic.yarnpkg.com/lang/en/docs/install
