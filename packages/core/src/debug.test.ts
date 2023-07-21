import * as wasm from "@rose-lang/wasm";
import { expect, test } from "vitest";
import {
  Bool,
  Real,
  abs,
  add,
  and,
  cond,
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
  sqrt,
  sub,
  xor,
} from "./index.js";

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 16, align: 8 },
    Instr: { size: 24, align: 8 },
    Ty: { size: 16, align: 4 },
  });
});

test("pprint if", () => {
  const f = fn([Real, Real], Real, (x, y) => {
    const p = lt(x, y);
    const z = cond(
      p,
      () => {
        const a = mul(x, y);
        return add(a, x);
      },
      () => {
        const b = sub(y, x);
        return mul(b, y);
      },
    );
    const w = add(z, x);
    return add(y, w);
  });
  const s = wasm.pprint(f.f.f);
  expect(s).toBe(
    `
T0 = Bool
T1 = F64
T2 = (T1, T1)
T3 = Unit
x0: T2 -> T1 {
  x1: T1 = x0.0
  x2: T1 = x0.1
  x3: T0 = x1 < x2
  x10: T1 = if x3 {
    x4: T3
    x5: T1 = x1 * x2
    x6: T1 = x5 + x1
    x6
  } else {
    x7: T3
    x8: T1 = x2 - x1
    x9: T1 = x8 * x2
    x9
  }
  x11: T1 = x10 + x1
  x12: T1 = x2 + x11
  x12
}
`.trimStart(),
  );
});

test("pprint call funcs", () => {
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
T2 = (T1)
F0 = 0<>
F1 = 1<>
x0: T2 -> T1 {
  x1: T1 = x0.0
  x2: T2 = (x1)
  x3: T1 = F0(x2)
  x4: T2 = (x1)
  x5: T1 = F1(x4)
  x6: T1 = x3 + x5
  x6
}
`.trimStart(),
  );
});

test("pprint unary operations", () => {
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
T2 = (T1)
x0: T2 -> T1 {
  x1: T1 = x0.0
  x2: T0 = true
  x3: T0 = not x2
  x4: T1 = -x1
  x5: T1 = |x4|
  x6: T1 = sqrt(x1)
  x6
}
`.trimStart(),
  );
});

test("pprint binary operations", () => {
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
T2 = (T1, T1)
x0: T2 -> T0 {
  x1: T1 = x0.0
  x2: T1 = x0.1
  x3: T1 = x1 + x2
  x4: T1 = x1 - x2
  x5: T1 = x1 * x2
  x6: T1 = x1 / x2
  x7: T0 = true
  x8: T0 = false
  x9: T0 = x7 and x8
  x10: T0 = true
  x11: T0 = false
  x12: T0 = x10 or x11
  x13: T0 = true
  x14: T0 = false
  x15: T0 = x13 iff x14
  x16: T0 = true
  x17: T0 = false
  x18: T0 = x16 xor x17
  x19: T0 = x1 != x2
  x20: T0 = x1 < x2
  x21: T0 = x1 <= x2
  x22: T0 = x1 == x2
  x23: T0 = x1 > x2
  x24: T0 = x5 >= x6
  x24
}
`.trimStart(),
  );
});
