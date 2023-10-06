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

This example defines custom derivatives for the builtin JavaScript logarithm and
power functions, then computes the output, gradient, and Hessian for the power
function applied with base 2 and exponent 3:

```js
import { Dual, Real, Vec, add, compile, div, fn, mul, opaque, vjp } from "rose";

const log = opaque([Real], Real, Math.log);
log.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  return { re: log(x), du: div(dx, x) };
});

const pow = opaque([Real, Real], Real, Math.pow);
pow.jvp = fn([Dual, Dual], Dual, ({ re: x, du: dx }, { re: y, du: dy }) => {
  const z = pow(x, y);
  return { re: z, du: mul(add(mul(dx, div(y, x)), mul(dy, log(x))), z) };
});

const Vec2 = Vec(2, Real);
const Mat2 = Vec(2, Vec2);

const f = fn([Vec2], Real, (v) => pow(v[0], v[1]));
const g = fn([Vec2], Vec2, (v) => vjp(f)(v).grad(1));
const h = fn([Vec2], Mat2, (v) => {
  const { grad } = vjp(g)(v);
  return [grad([1, 0]), grad([0, 1])];
});

const funcs = await Promise.all([compile(f), compile(g), compile(h)]);
console.log(funcs.map((func) => func([2, 3])));
```

### With Vite

If you are using [Vite][] then you will need to also install the
[vite-plugin-top-level-await][] package, because Rose internally uses [top-level
`await`][], which Vite does not directly support. You must also include the
following in your Vite config:

```js
import { defineConfig } from "vite";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({
  // the plugin described above
  plugins: [topLevelAwait()],

  // Vite bundles external dependencies by default in development mode, but that
  // process does not include assets; this option disables that particular kind
  // of bundling for Rose so that it can use its internal WebAssembly module
  optimizeDeps: { exclude: ["rose"] },
});
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
[top-level `await`]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await#top_level_await
[vite-plugin-top-level-await]: https://www.npmjs.com/package/vite-plugin-top-level-await
[Vite]: https://vitejs.dev/
[Yarn]: https://classic.yarnpkg.com/lang/en/docs/install/
