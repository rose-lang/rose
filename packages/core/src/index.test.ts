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
  const g = interp(fn([], Real, () => f(2, 2)));
  expect(g()).toBe(4);
});

test("basic arithmetic", () => {
  const f = fn([], Real, () => add(2, sub(mul(3, 2), div(2, 1))));
  const g = interp(f);
  expect(g()).toBe(6);
});

test("select", () => {
  const f = fn([Bool], Real, (x) => select(x, Real, 1, 2));
  const g = interp(fn([], Real, () => f(false)));
  const h = interp(fn([], Real, () => f(true)));
  expect(g()).toBe(2);
  expect(h()).toBe(1);
});

test("call", () => {
  const ifCond = fn([Bool, Real, Real], Real, (p, x, y) =>
    select(p, Real, x, y),
  );
  const f = fn([Real], Real, (x) => ifCond(lt(x, 0), 0, x));
  const a = interp(fn([], Real, () => f(-1)));
  const b = interp(fn([], Real, () => f(0)));
  const c = interp(fn([], Real, () => f(1)));
  expect(a()).toBe(0);
  expect(b()).toBe(0);
  expect(c()).toBe(1);
});

test("invalid", () => {
  const two = true as any;
  expect(() => fn([], Real, () => add(two, two))).toThrow(
    "variable type mismatch",
  );
});
