import * as wasm from "@rose-lang/wasm";
import { expect, test } from "vitest";
import { Real, add, cond, fn, lt, mul, sub } from "./index.js";

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 16, align: 8 },
    Instr: { size: 24, align: 8 },
    Ty: { size: 16, align: 4 },
  });
});

test("pprint", () => {
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
      }
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
`.trimStart()
  );
});
