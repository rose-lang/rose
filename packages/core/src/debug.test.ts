import * as wasm from "@rose-lang/wasm";
import { describe, expect, test } from "vitest";
import {
  Bool,
  Real,
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
  select,
  sqrt,
  sub,
  xor,
} from "./index.js";

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 32, align: 8 },
    Instr: { size: 40, align: 8 },
    Ty: { size: 16, align: 4 },
  });
});

describe("pprint", () => {
  test("if", () => {
    const f = fn([Real, Real], Real, (x, y) => {
      const p = lt(x, y);
      const a = mul(x, y);
      const b = sub(y, x);
      const z = select(p, add(a, x), mul(b, y));
      const w = add(z, x);
      return add(y, w);
    });
    const s = wasm.pprint(f.f.f);
    expect(s).toBe(
      `
T0 = Bool
T1 = F64
(x0: T1, x1: T1) -> T1 {
  x2: T0 = x0 < x1
  x3: T1 = x0 * x1
  x4: T1 = x1 - x0
  x5: T1 = x3 + x0
  x6: T1 = x4 * x1
  x7: T1 = x2 ? x5 : x6
  x8: T1 = x7 + x0
  x9: T1 = x1 + x8
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
    const s = wasm.pprint(f.f.f);
    expect(s).toBe(
      `
T0 = Bool
T1 = F64
(x0: T1) -> T1 {
  x1: T1 = f0<>(x0)
  x2: T1 = f1<>(x0)
  x3: T1 = x1 + x2
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
    const s = wasm.pprint(f.f.f);
    expect(s).toBe(
      `
T0 = Bool
T1 = F64
(x0: T1) -> T1 {
  x1: T0 = true
  x2: T0 = not x1
  x3: T1 = -x0
  x4: T1 = |x3|
  x5: T1 = sqrt(x0)
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
    const s = wasm.pprint(f.f.f);
    expect(s).toBe(
      `
T0 = Bool
T1 = F64
(x0: T1, x1: T1) -> T0 {
  x2: T1 = x0 + x1
  x3: T1 = x0 - x1
  x4: T1 = x0 * x1
  x5: T1 = x0 / x1
  x6: T0 = true
  x7: T0 = false
  x8: T0 = x6 and x7
  x9: T0 = true
  x10: T0 = false
  x11: T0 = x9 or x10
  x12: T0 = true
  x13: T0 = false
  x14: T0 = x12 iff x13
  x15: T0 = true
  x16: T0 = false
  x17: T0 = x15 xor x16
  x18: T0 = x0 != x1
  x19: T0 = x0 < x1
  x20: T0 = x0 <= x1
  x21: T0 = x0 == x1
  x22: T0 = x0 > x1
  x23: T0 = x4 >= x5
  x23
}
`.trimStart(),
    );
  });
});
