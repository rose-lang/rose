import { describe, expect, test } from "vitest";
import {
  Bool,
  Dual,
  Null,
  Real,
  Vec,
  abs,
  add,
  ceil,
  compile,
  div,
  floor,
  fn,
  gt,
  interp,
  jvp,
  mul,
  mulLin,
  neg,
  not,
  opaque,
  or,
  select,
  sign,
  sqrt,
  sub,
  trunc,
  vec,
  vjp,
} from "./index.js";

describe("invalid", () => {
  test("undefined", () => {
    expect(() => fn([], Real, () => undefined as any)).toThrow("invalid value");
  });

  test("bigint", () => {
    expect(() => fn([], Real, () => 0n as any)).toThrow("invalid value");
  });

  test("string", () => {
    expect(() => fn([], Real, () => "hello" as any)).toThrow("invalid value");
  });

  test("literal return type", () => {
    expect(() => fn([], Real, () => null as any)).toThrow(
      "did not expect null",
    );
  });

  test("add argument type", () => {
    const two = true as any;
    expect(() => fn([], Real, () => add(two, two))).toThrow(
      "did not expect boolean",
    );
  });

  test("invalid index type", () => {
    expect(() =>
      fn([Vec(Real, Null), Real], Null, (v, i) => v[i as any]),
    ).toThrow("index type cannot be used as an index");
  });

  test("symbolic array dimension", () => {
    expect(() => fn([Vec(3, Real)], Vec(2, Real), (v) => v)).toThrow(
      "variable type mismatch",
    );
  });

  test("literal array dimension", () => {
    expect(() => fn([], Vec(2, Real), () => [1, 2, 3])).toThrow(
      "wrong array size",
    );
  });

  test("out of bounds index", () => {
    expect(() => fn([Vec(2, Real)], Real, (v) => v[2])).toThrow("out of range");
  });

  test("access index out of scope", () => {
    const n = 2;
    expect(() =>
      fn([], n, () => {
        let i: number | symbol = 0;
        vec(n, Real, (j) => {
          i = j;
          return 42;
        });
        return i;
      }),
    ).toThrow("variable is out of scope");
  });

  test("wrong struct member names", () => {
    expect(() =>
      fn([{ a: Real, b: Real }], { b: Real, c: Real }, (x) => x as any),
    ).toThrow("variable type mismatch");
  });
});

describe("valid", () => {
  test("return null", () => {
    const f = fn([], Null, () => null);
    const g = interp(f);
    expect(g()).toBe(null);
  });

  test("interp with null", () => {
    const f = fn([Null], Null, (x) => x);
    const g = interp(f);
    expect(g(null)).toBe(null);
  });

  test("not", () => {
    const f = fn([Bool], Bool, (p) => not(p));
    const g = interp(f);
    expect(g(true)).toBe(false);
    expect(g(false)).toBe(true);
  });

  test("2 + 2 = 4", () => {
    const f = fn([Real, Real], Real, (x, y) => add(x, y));
    const g = interp(f);
    expect(g(2, 2)).toBe(4);
  });

  test("basic arithmetic", () => {
    const f = fn([], Real, () => add(2, sub(mul(3, 2), div(2, 1))));
    const g = interp(f);
    expect(g()).toBe(6);
  });

  test("absolute value", () => {
    const f = fn([Real], Real, (x) => abs(x));
    const g = interp(f);
    expect(g(-2)).toBe(2);
    expect(g(-0)).toBe(0);
    expect(g(0)).toBe(0);
    expect(g(2)).toBe(2);
  });

  test("signum", () => {
    const f = fn([Real], Real, (x) => sign(x));
    const g = interp(f);
    expect(g(-2)).toBe(-1);
    expect(g(-0)).toBe(-1);
    expect(g(0)).toBe(1);
    expect(g(2)).toBe(1);
  });

  test("ceiling", () => {
    const f = fn([Real], Real, (x) => ceil(x));
    const g = interp(f);
    expect(g(-1.5)).toBe(-1);
    expect(g(-1)).toBe(-1);
    expect(g(-0.5)).toBe(-0);
    expect(g(-0)).toBe(-0);
    expect(g(0)).toBe(0);
    expect(g(0.5)).toBe(1);
    expect(g(1)).toBe(1);
    expect(g(1.5)).toBe(2);
  });

  test("floor", () => {
    const f = fn([Real], Real, (x) => floor(x));
    const g = interp(f);
    expect(g(-1.5)).toBe(-2);
    expect(g(-1)).toBe(-1);
    expect(g(-0.5)).toBe(-1);
    expect(g(-0)).toBe(-0);
    expect(g(0)).toBe(0);
    expect(g(0.5)).toBe(0);
    expect(g(1)).toBe(1);
    expect(g(1.5)).toBe(1);
  });

  test("truncate", () => {
    const f = fn([Real], Real, (x) => trunc(x));
    const g = interp(f);
    expect(g(-1.5)).toBe(-1);
    expect(g(-1)).toBe(-1);
    expect(g(-0.5)).toBe(-0);
    expect(g(-0)).toBe(-0);
    expect(g(0)).toBe(0);
    expect(g(0.5)).toBe(0);
    expect(g(1)).toBe(1);
    expect(g(1.5)).toBe(1);
  });

  test("square root", () => {
    const f = fn([Real], Real, (x) => sqrt(x));
    const g = interp(f);
    expect(g(Math.PI)).toBe(1.7724538509055159);
  });

  test("select", () => {
    const f = fn([Bool], Real, (x) => select(x, Real, 1, 2));
    const g = interp(f);
    expect(g(false)).toBe(2);
    expect(g(true)).toBe(1);
  });

  test("call", () => {
    const ifCond = fn([Bool, Real, Real], Real, (p, x, y) =>
      select(p, Real, x, y),
    );
    const f = fn([Real], Real, (x) => ifCond(gt(x, 0), x, 0));
    const relu = interp(f);
    expect(relu(-2)).toBe(0);
    expect(relu(-0)).toBe(0);
    expect(relu(0)).toBe(0);
    expect(relu(2)).toBe(2);
  });

  test("empty boolean array", () => {
    const f = fn([], Vec(0, Bool), () => []);
    const g = interp(f);
    expect(g()).toEqual([]);
  });

  test("empty real array", () => {
    const f = fn([], Vec(0, Real), () => []);
    const g = interp(f);
    expect(g()).toEqual([]);
  });

  test("dot product", () => {
    const R3 = Vec(3, Real);
    const dot = fn([R3, R3], Real, (u, v) => {
      const x = mul(u[0], v[0]);
      const y = mul(u[1], v[1]);
      const z = mul(u[2], v[2]);
      return add(add(x, y), z);
    });
    const f = interp(dot);
    expect(f([1, 3, -5], [4, -2, -1])).toBe(3);
  });

  test("cross product", () => {
    const R3 = Vec(3, Real);
    const cross = fn([R3, R3], R3, (u, v) => {
      const x = sub(mul(u[1], v[2]), mul(u[2], v[1]));
      const y = sub(mul(u[2], v[0]), mul(u[0], v[2]));
      const z = sub(mul(u[0], v[1]), mul(u[1], v[0]));
      return [x, y, z];
    });
    const f = interp(cross);
    expect(f([3, -3, 1], [4, 9, 2])).toEqual([-15, -2, 39]);
  });

  test("index array", () => {
    const n = 3;
    const f = fn([Vec(n, n), Vec(n, Real)], Vec(n, Real), (i, v) =>
      vec(n, Real, (j) => v[i[j]]),
    );
    const g = fn([], Vec(n, Real), () => {
      const v = [2, 0, 1];
      return f(v, v);
    });
    const h = interp(g);
    expect(h()).toEqual([1, 2, 0]);
  });

  test("interp with index value", () => {
    const n = 1;
    const f = fn([n, Vec(n, Bool)], Bool, (i, v) => v[i]);
    const g = interp(f);
    expect(g(0, [true])).toBe(true);
  });

  test("matrix multiplication", () => {
    const n = 6;

    const Rn = Vec(n, Real);

    const dot = fn([Rn, Rn], Real, (u, v) => {
      const w = vec(n, Real, (i) => mul(u[i], v[i]));
      let s = w[0];
      s = add(s, w[1]);
      s = add(s, w[2]);
      s = add(s, w[3]);
      s = add(s, w[4]);
      s = add(s, w[5]);
      return s;
    });

    const m = 5;
    const p = 7;

    const Rp = Vec(p, Real);

    const Rmxn = Vec(m, Rn);
    const Rnxp = Vec(n, Rp);
    const Rmxp = Vec(m, Rp);

    const mmul = fn([Rmxn, Rnxp], Rmxp, (a, b) =>
      vec(m, Rp, (i) => {
        const u = a[i];
        return vec(p, Real, (j) => {
          const v = vec(n, Real, (k) => b[k][j]);
          return dot(u, v);
        });
      }),
    );

    const f = interp(mmul);
    expect(
      f(
        [
          [-8, 5, 3, -1, 8, 0],
          [-3, -1, 7, -7, 8, 3],
          [-4, 5, 5, 5, 8, 6],
          [1, -9, 5, 4, 4, 0],
          [9, -3, 1, 3, -5, -5],
        ],
        [
          [-7, 9, 6, -8, 5, 8, -3],
          [3, -6, 8, 0, 7, -4, -1],
          [-4, 9, 9, 1, -8, -4, 0],
          [6, -7, 6, -6, -8, -5, 0],
          [8, 5, 3, 0, 6, 3, -7],
          [-4, 0, -5, -9, 8, -9, -1],
        ],
      ),
    ).toEqual([
      [117, -28, 37, 73, 27, -67, -37],
      [0, 131, 4, 46, 50, -16, -49],
      [93, -16, 85, -47, 31, -127, -55],
      [2, 100, 15, -27, -106, 16, -22],
      [-78, 62, 67, -44, -78, 95, 16],
    ]);
  });

  test("singleton array from index", () => {
    const One = Vec(1, 1);
    const f = fn([], One, () => vec(1, One, (i) => [i])[0]);
    const g = interp(f);
    expect(g()).toEqual([0]);
  });

  test("struct", () => {
    const Pair = { x: Real, y: Real } as const;
    const f = fn([Pair], Real, (p) => sub(p.y, p.x));
    const g = fn([Real, Real], Pair, (x, y) => ({ y, x }));
    const h = interp(fn([Real, Real], Real, (x, y) => f(g(x, y))));
    expect(h(3, 5)).toBe(2);
  });

  test("return struct", () => {
    const f = fn([], { p: Bool, x: Real }, () => ({ p: true, x: 42 }));
    const g = interp(f);
    expect(g()).toEqual({ p: true, x: 42 });
  });

  test("select struct", () => {
    const f = fn([], Real, () => {
      return select(false, { x: Real }, { x: 3 }, { x: 5 }).x;
    });
    const g = interp(f);
    expect(g()).toBe(5);
  });

  test("array of structs", () => {
    const n = 2;
    const Indexed = { i: n, x: Real } as const;
    const f = fn([Vec(n, Real)], Vec(n, Indexed), (v) =>
      vec(n, Indexed, (i) => ({ i, x: v[i] })),
    );
    const g = interp(f);
    expect(g([3, 5])).toEqual([
      { i: 0, x: 3 },
      { i: 1, x: 5 },
    ]);
  });

  test("internal array of structs", () => {
    const n = 2;
    const f = fn([Vec(n, Real)], Vec(n, n), (v) => {
      const u = vec(n, { i: n }, (i) => ({ i }));
      return vec(n, n, (i) => u[i].i);
    });
    const g = interp(f);
    expect(g([3, 5])).toEqual([0, 1]);
  });

  test("interp struct arg", () => {
    const f = fn([{ x: Real }], Real, (p) => p.x);
    const g = interp(f);
    expect(g({ x: 42 })).toBe(42);
  });

  test("opaque unary function", () => {
    const log = opaque([Real], Real, Math.log);
    const f = interp(log);
    expect(f(Math.PI)).toBe(1.1447298858494002);
  });

  test("opaque binary function", () => {
    const pow = opaque([Real, Real], Real, Math.pow);
    const f = interp(pow);
    expect(f(Math.E, Math.PI)).toBe(23.140692632779263);
  });

  test("JVP", () => {
    const f = fn([Real], Real, (x) => mul(x, x));
    const g = jvp(f);
    const h = interp(g);
    expect(h({ re: 3, du: 1 })).toEqual({ re: 9, du: 6 });
  });

  test("JVP with sharing in call graph", () => {
    let f = fn([Real], Real, (x) => x);
    for (let i = 0; i < 20; ++i) {
      f = fn([Real], Real, (x) => add(f(x), f(x)));
    }
    const g = interp(jvp(f));
    expect(g({ re: 2, du: 3 })).toEqual({ re: 2097152, du: 3145728 });
  });

  test("custom JVP", () => {
    const max = fn([Real, Real], Real, (x, y) => select(gt(x, y), Real, x, y));
    const f = fn([Real], Real, (x) => sqrt(x));
    const epsilon = 1e-5;
    f.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      const y = f(x);
      return { re: y, du: mulLin(dx, div(1 / 2, max(epsilon, y))) };
    });
    const g = fn([Real, Real], Real, (x, y) => vjp(f)(x).grad(y));
    expect(interp(g)(0, 1)).toBeCloseTo(50000);
  });

  test("VJP", () => {
    const f = fn([Vec(2, Real)], Real, (v) => mul(v[0], v[1]));
    const g = fn([], Vec(3, Real), () => {
      const { ret: x, grad } = vjp(f)([2, 3]);
      const v = grad(1);
      return [x, v[0], v[1]];
    });
    expect(interp(g)()).toEqual([6, 3, 2]);
  });

  test("VJP with sharing in call graph", () => {
    const iterate = (
      n: number,
      f: (x: Real) => Real,
      g: (x: Real, y: Real) => Real,
    ) => {
      for (let i = 0; i < n; ++i) {
        const f0 = f;
        const g0 = g;
        g = fn([Real, Real], Real, (x, y) => g0(f0(x), f0(y)));
        const g1 = g;
        f = fn([Real], Real, (x) => g1(x, x));
      }
      return f;
    };

    const f = iterate(
      12,
      (x) => sqrt(x),
      (x, y) => mul(x, y),
    );
    const g = vjp(fn([Real], Real, (x) => f(x)));
    const h = fn([Real, Real], Vec(2, Real), (x, y) => {
      const { ret, grad } = g(x);
      return [ret, grad(y)];
    });
    const v = interp(h)(2, 3);
    expect(v[0]).toBeCloseTo(2);
    expect(v[1]).toBeCloseTo(3);
  });

  test("VJP with struct and select", () => {
    const Stuff = { a: Null, b: Bool, c: Real } as const;
    const f = fn([Stuff], Real, ({ b, c }) => select(or(false, b), Real, c, 2));
    const g = fn([Bool, Real], { x: Real, stuff: Stuff }, (b, c) => {
      const { ret: x, grad } = vjp(f)({ a: null, b, c });
      return { x, stuff: grad(3) };
    });
    const h = interp(g);
    expect(h(true, 5)).toEqual({ x: 5, stuff: { a: null, b: true, c: 3 } });
    expect(h(false, 7)).toEqual({ x: 2, stuff: { a: null, b: false, c: 0 } });
  });

  test("VJP with select on null", () => {
    const f = fn([Null], Null, () => select(true, Null, null, null));
    const g = fn([], Null, () => vjp(f)(null).ret);
    const h = interp(g);
    expect(h()).toBe(null);
  });

  test("VJP with select on booleans", () => {
    const f = fn([Bool], Bool, (p) => select(p, Bool, false, true));
    const g = fn([Bool], Bool, (p) => vjp(f)(p).ret);
    const h = interp(g);
    expect(h(true)).toBe(false);
    expect(h(false)).toBe(true);
  });

  test("VJP with select on indices", () => {
    const n = 2;
    const f = fn([Bool], n, (p) => select(p, n, 0, 1));
    const g = fn([Bool], n, (p) => vjp(f)(p).ret);
    const h = interp(g);
    expect(h(true)).toBe(0);
    expect(h(false)).toBe(1);
  });

  test("VJP with vector comprehension", () => {
    const n = 2;
    const f = fn([Vec(n, Real)], Vec(n, Real), (v) =>
      vec(n, Real, (i) => mul(v[i], v[i])),
    );
    const g = fn([Vec(n, Real), Vec(n, Real)], Vec(n, Real), (u, v) =>
      vjp(f)(u).grad(v),
    );
    expect(interp(g)([2, 3], [5, 7])).toEqual([20, 42]);
  });

  test("VJP twice", () => {
    const f = fn([Real], Real, (x) => {
      const y = mul(x, x);
      return mul(x, y);
    });
    const g = fn([Real], Real, (x) => vjp(f)(x).grad(1));
    const h = fn([Real], Real, (x) => vjp(g)(x).grad(1));
    expect(interp(h)(10)).toBe(60);
  });

  test("Hessian", () => {
    const powi = (x: Real, n: number): Real => {
      if (!Number.isInteger(n))
        throw new Error(`exponent is not an integer: ${n}`);
      // https://en.wikipedia.org/wiki/Exponentiation_by_squaring
      if (n < 0) return powi(div(1, x), -n);
      else if (n == 0) return 1;
      else if (n == 1) return x;
      else if (n % 2 == 0) return powi(mul(x, x), n / 2);
      else return mul(x, powi(mul(x, x), (n - 1) / 2));
    };
    const f = fn([Vec(2, Real)], Real, (v) => {
      const x = v[0];
      const y = v[1];
      return sub(sub(powi(x, 3), mul(2, mul(x, y))), powi(y, 6));
    });
    const g = fn([Vec(2, Real)], Vec(2, Real), (v) => vjp(f)(v).grad(1));
    const h = fn([Vec(2, Real)], Vec(2, Vec(2, Real)), (v) => {
      const { grad } = vjp(g)(v);
      return [grad([1, 0] as any), grad([0, 1] as any)];
    });
    expect(interp(h)([1, 2])).toEqual([
      [6, -2],
      [-2, -480],
    ]);
  });

  test("VJP twice with struct", () => {
    const Pair = { x: Real, y: Real } as const;
    const f = fn([Pair], Real, ({ x, y }) => mul(x, y));
    const g = fn([Pair], Pair, (p) => vjp(f)(p).grad(1));
    const h = fn([Pair, Pair], Pair, (p, q) => vjp(g)(p).grad(q));
    expect(interp(h)({ x: 2, y: 3 }, { x: 5, y: 7 })).toEqual({ x: 7, y: 5 });
  });

  test("VJP twice with select", () => {
    const Stuff = { p: Bool, x: Real, y: Real, z: Real } as const;
    const f = fn([Stuff], Real, ({ p, x, y, z }) =>
      mul(z, select(p, Real, x, y)),
    );
    const g = fn([Stuff], Stuff, (p) => vjp(f)(p).grad(1));
    const h = fn([Stuff, Stuff], Stuff, (p, q) => vjp(g)(p).grad(q));
    expect(
      interp(h)(
        { p: true, x: 2, y: 3, z: 5 },
        { p: false, x: 7, y: 11, z: 13 },
      ),
    ).toEqual({ p: true, x: 13, y: 0, z: 7 });
  });

  test("opaque functions with derivatives", () => {
    const grad = (f: any) => fn([Real], Real, (x) => vjp(f)(x).grad(1) as Real);

    const sin = opaque([Real], Real, Math.sin);
    const cos = opaque([Real], Real, Math.cos);

    sin.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      return { re: sin(x), du: mulLin(dx, cos(x)) };
    });
    cos.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      return { re: cos(x), du: mulLin(dx, neg(sin(x))) };
    });

    let f = sin;
    expect(interp(f)(1)).toBeCloseTo(Math.sin(1));
    f = grad(f);
    expect(interp(f)(1)).toBeCloseTo(Math.cos(1));
    f = grad(f);
    expect(interp(f)(1)).toBeCloseTo(-Math.sin(1));

    f = cos;
    expect(interp(f)(1)).toBeCloseTo(Math.cos(1));
    f = grad(f);
    expect(interp(f)(1)).toBeCloseTo(-Math.sin(1));
    f = grad(f);
    expect(interp(f)(1)).toBeCloseTo(-Math.cos(1));
  });

  test("compile", async () => {
    const f1 = fn([Real], Real, (x) => sqrt(x));
    const f2 = fn([Real, Real], Real, (x, y) => mul(x, f1(y)));
    const f3 = fn([Real, Real], Real, (x, y) => mul(f1(x), y));
    const f = fn([Real, Real], Real, (x, y) => sub(f2(x, y), f3(x, y)));
    const g = await compile(f);
    expect(g(2, 3)).toBeCloseTo(-0.7785390719815313);
  });

  test("compile opaque function", async () => {
    const f = opaque([Real], Real, Math.sin);
    const g = await compile(f);
    expect(g(1)).toBeCloseTo(Math.sin(1));
  });

  test("compile calls to multiple opaque functions", async () => {
    const sin = opaque([Real], Real, Math.sin);
    const cos = opaque([Real], Real, Math.cos);
    const f = fn([Real], Real, (x) => sub(sin(x), cos(x)));
    const g = await compile(f);
    expect(g(1)).toBeCloseTo(Math.sin(1) - Math.cos(1));
  });

  test("compile opaque and transparent calls together", async () => {
    const log = opaque([Real], Real, Math.log);
    const f = fn([Real], Real, (x) => add(log(x), sqrt(x)));
    const g = fn([Real], Real, (x) => add(f(x), x));
    const h = await compile(g);
    expect(h(1)).toBeCloseTo(Math.log(1) + Math.sqrt(1) + 1);
  });

  test("compile array", async () => {
    const f = fn([Vec(2, Real)], Real, (v) => mul(v[0], v[1]));
    const g = fn([Real, Real], Real, (x, y) => f([x, y]));
    const h = await compile(g);
    expect(h(2, 3)).toBe(6);
  });
});
