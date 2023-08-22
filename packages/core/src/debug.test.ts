import * as wasm from "@rose-lang/wasm";
import { describe, expect, test } from "vitest";
import {
  Bool,
  Real,
  Vec,
  abs,
  add,
  and,
  div,
  eq,
  fn,
  geq,
  gt,
  iff,
  leq,
  lt,
  mul,
  neg,
  neq,
  not,
  or,
  pprint,
  select,
  sqrt,
  sub,
  vec,
  xor,
} from "./index.js";

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 24, align: 8 },
    Function: { size: 44, align: 4 },
    Instr: { size: 32, align: 8 },
    Ty: { size: 16, align: 4 },
  });
});

describe("pprint", () => {
  test("if", () => {
    const f = fn([Real, Real], Real, (x, y) => {
      const p = lt(x, y);
      const a = mul(x, y);
      const b = sub(y, x);
      const z = select(p, Real, add(a, x), mul(b, y));
      const w = add(z, x);
      return add(y, w);
    });
    const s = pprint(f);
    expect(s).toBe(
      `
T0 = F64
T1 = Bool
(x0: T0, x1: T0) -> T0 {
  x2: T1 = x0 < x1
  x3: T0 = x0 * x1
  x4: T0 = x1 - x0
  x5: T0 = x3 + x0
  x6: T0 = x4 * x1
  x7: T0 = x2 ? x5 : x6
  x8: T0 = x7 + x0
  x9: T0 = x1 + x8
  x9
}
`.trimStart(),
    );
  });

  test("call funcs", () => {
    const g = fn([Real], Real, (y) => add(2, y));
    const h = fn([Real], Real, (z) => mul(2, z));
    const f = fn([Real], Real, (x) => {
      const a = g(x);
      const b = h(x);
      return add(a, b);
    });
    const s = pprint(f);
    expect(s).toBe(
      `
T0 = F64
(x0: T0) -> T0 {
  x1: T0 = f0<>(x0)
  x2: T0 = f1<>(x0)
  x3: T0 = x1 + x2
  x3
}
`.trimStart(),
    );
  });

  test("unary operations", () => {
    const f = fn([Real], Real, (x) => {
      const a = not(true);
      const b = neg(x);
      const c = abs(b);
      const d = sqrt(x);
      return d;
    });
    const s = pprint(f);
    expect(s).toBe(
      `
T0 = F64
T1 = Bool
(x0: T0) -> T0 {
  x1: T1 = true
  x2: T1 = not x1
  x3: T0 = -x0
  x4: T0 = |x3|
  x5: T0 = sqrt(x0)
  x5
}
`.trimStart(),
    );
  });

  test("binary operations", () => {
    const f = fn([Real, Real], Bool, (x, y) => {
      const a = add(x, y);
      const b = sub(x, y);
      const c = mul(x, y);
      const d = div(x, y);
      const e = and(true, false);
      const f = or(true, false);
      const g = iff(true, false);
      const h = xor(true, false);
      const i = neq(x, y);
      const j = lt(x, y);
      const k = leq(x, y);
      const l = eq(x, y);
      const m = gt(x, y);
      return geq(c, d);
    });
    const s = pprint(f);
    expect(s).toBe(
      `
T0 = F64
T1 = Bool
(x0: T0, x1: T0) -> T1 {
  x6: T1 = true
  x7: T1 = false
  x2: T0 = x0 + x1
  x3: T0 = x0 - x1
  x4: T0 = x0 * x1
  x5: T0 = x0 / x1
  x8: T1 = x6 and x7
  x9: T1 = x6 or x7
  x10: T1 = x6 iff x7
  x11: T1 = x6 xor x7
  x12: T1 = x0 != x1
  x13: T1 = x0 < x1
  x14: T1 = x0 <= x1
  x15: T1 = x0 == x1
  x16: T1 = x0 > x1
  x17: T1 = x4 >= x5
  x17
}
`.trimStart(),
    );
  });

  test("for", () => {
    const n = 3;
    const Rn = Vec(n, Real);
    const f = fn([Rn, Rn], Rn, (a, b) => vec(n, Real, (i) => add(a[i], b[i])));
    const s = pprint(f);
    expect(s).toBe(
      `
T0 = 3
T1 = F64
T2 = [T1; T0]
(x0: T2, x1: T2) -> T2 {
  x6: T2 = for x2: T0 {
    x3: T1 = x0[x2]
    x4: T1 = x1[x2]
    x5: T1 = x3 + x4
    x5
  }
  x6
}
`.trimStart(),
    );
  });
});
