import { expect, test } from "vitest";
import { Real, add, fn, interp } from "./index.js";

test("add", () => {
  const f = fn([Real, Real], (x, y) => add(x, y));
  const g: (x: number, y: number) => number = interp(f);
  expect(g(3, 5)).toEqual(8);
});
