import hljs from "highlight.js/lib/core";
import javascript from "highlight.js/lib/languages/javascript";
import "highlight.js/styles/base16/helios.css";

hljs.registerLanguage("javascript", javascript);
hljs.highlightAll();

import("rose").then(
  async ({ Dual, Real, Vec, add, compile, div, fn, mul, opaque, vjp }) => {
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

    const f = fn([Vec2], Real, ([x, y]) => pow(x, y));
    const g = fn([Vec2], Vec2, (v) => vjp(f)(v).grad(1));
    const h = fn([Vec2], Mat2, (v) => {
      const { grad } = vjp(g)(v);
      return [grad([1, 0] as any), grad([0, 1] as any)];
    });

    const funcs = await Promise.all([compile(f), compile(g), compile(h)]);
    console.log(funcs.map((func) => func([2, 3])));
  },
);
