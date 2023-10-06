<div align="center"><img height="256" src="https://github.com/rose-lang/rose-icons/raw/efcc218832d65970a47bed597ee11cecd3d1cc3c/svg/encircled-rose.svg" /></div>
<h1 align="center">Rose</h1>
<p align="center"><em><a href="https://github.com/rose-lang/rose-icons">icon</a> by <a href="https://github.com/aatxe">Aaron Weiss</a> / <a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a></em></p>
<p align="center"><a href="https://www.npmjs.com/package/rose"><img src="https://img.shields.io/npm/v/rose" alt="npm" /></a> <a href="LICENSE"><img src="https://img.shields.io/github/license/rose-lang/rose" alt="license" /></a> <a href="https://github.com/rose-lang/rose/actions/workflows/build.yml"><img src="https://github.com/rose-lang/rose/actions/workflows/build.yml/badge.svg" alt="Build" /></a></p>

Rose is an automatic differentiation engine for the web, inspired by [JAX][].

## Installation

With [npm][]:

```sh
npm i rose
```

With [Yarn][]:

```sh
yarn add rose
```

With [pnpm][]:

```sh
pnpm add rose
```

With [Bun][]:

```sh
bun add rose
```

## Usage

This example computes the output and gradient of a simple function that
multiplies together the two components of a vector:

```js
import { Real, Vec, fn, interp, mul, vjp } from "rose";

const f = fn([Vec(2, Real)], Real, (v) => mul(v[0], v[1]));

const g = fn([Real, Real], Vec(3, Real), (x, y) => {
  const { ret, grad } = vjp(f)([x, y]);
  const v = grad(1);
  return [ret, v[0], v[1]];
});

console.log(interp(g)(2, 3)); // [6, 3, 2]
```

## Contributing

See [`CONTRIBUTING.md`][].

## License

Rose is licensed under the [MIT License][].

[`CONTRIBUTING.md`]: https://github.com/rose-lang/rose/blob/main/CONTRIBUTING.md
[Bun]: https://bun.sh/
[JAX]: http://jax.readthedocs.io/
[MIT License]: https://github.com/rose-lang/rose/blob/main/LICENSE
[npm]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
[pnpm]: https://pnpm.io/installation
[Yarn]: https://classic.yarnpkg.com/lang/en/docs/install/
