import { expect, test } from "vitest";
import { Real, fn, interp, sub } from "./index.js";

test("sub", () => {
  const f = fn([Real, Real], (x, y) => sub(x, y));
  const g = interp(f);
  expect(g(5, 3)).toEqual(2);
});
