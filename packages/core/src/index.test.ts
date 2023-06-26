import { expect, test } from "vitest";
import { Real, add, div, fn, interp, mul, sub } from "./index.js";

test("2 + 2 = 4", () => {
  const f = fn([Real, Real], (x, y) => add(x, y));
  const g = interp(f);
  expect(g(2, 2)).toBe(4);
});

test("basic arithmetic", () => {
  const f = fn([], () => add(2, sub(mul(3, 2), div(2, 1))));
  const g = interp(f);
  expect(g()).toBe(6);
});
