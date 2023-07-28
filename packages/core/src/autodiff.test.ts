import { expect, test } from "vitest";
import { Real, derivative, fn, interp, mul } from "./index.js";

test("derivative", () => {
  const f = fn([Real], Real, (x) => mul(x, x));
  const g = derivative(f);
  const h = interp(g as any);
  expect(h(3, 1)).toBe(6);
});
