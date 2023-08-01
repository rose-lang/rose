import { expect, test } from "vitest";
import {
  Bool,
  Real,
  add,
  div,
  fn,
  interp,
  lt,
  mul,
  select,
  sub,
} from "./index.js";

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

test("branch", () => {
  const f = fn([Bool], Real, (x) => select(x, 1, 2));
  const g = interp(f);
  expect(g(false)).toBe(2);
  expect(g(true)).toBe(1);
});

test("call", () => {
  const ifCond = fn([Bool, Real, Real], Real, (p, x, y) => select(p, x, y));
  const f = fn([Real], Real, (x) => ifCond(lt(x, 0), 0, x));
  const relu = interp(f);
  expect(relu(-1)).toBe(0);
  expect(relu(0)).toBe(0);
  expect(relu(1)).toBe(1);
});
