import { describe, expect, test } from "vitest";
import {
  Bool,
  Null,
  Real,
  Vec,
  add,
  div,
  fn,
  interp,
  lt,
  mul,
  select,
  sub,
  vec,
} from "./index.js";

describe("invalid", () => {
  test("undefined", () => {
    expect(() => fn([], Real, () => undefined as any)).toThrow(
      "undefined value",
    );
  });

  test("bigint", () => {
    expect(() => fn([], Real, () => 0n as any)).toThrow("bigint not supported");
  });

  test("string", () => {
    expect(() => fn([], Real, () => "hello" as any)).toThrow(
      "string not supported",
    );
  });

  test("literal return type", () => {
    expect(() => fn([], Real, () => null as any)).toThrow(
      "did not expect null",
    );
  });

  test("add argument type", () => {
    const two = true as any;
    expect(() => fn([], Real, () => add(two, two))).toThrow(
      "did not expect boolean",
    );
  });

  test("symbolic array dimension", () => {
    expect(() => fn([Vec(3, Real)], Vec(2, Real), (v) => v)).toThrow(
      "variable type mismatch",
    );
  });

  test("literal array dimension", () => {
    expect(() => fn([], Vec(2, Real), () => vec(Real, [1, 2, 3]))).toThrow(
      "variable type mismatch",
    );
  });

  test("out of bounds index", () => {
    expect(() => fn([Vec(2, Real)], Real, (v) => v[2])).toThrow("out of range");
  });
});

describe("valid", () => {
  test("null", () => {
    const f = fn([], Null, () => null);
    const g = interp(f);
    expect(g()).toBe(null);
  });

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

  test("empty boolean array", () => {
    const f = fn([], Vec(0, Bool), () => vec(Bool, []));
    const g = interp(f);
    expect(g()).toEqual([]);
  });

  test("empty real array", () => {
    const f = fn([], Vec(0, Real), () => vec(Real, []));
    const g = interp(f);
    expect(g()).toEqual([]);
  });

  test("dot product", () => {
    const R3 = Vec(3, Real);
    const dot = fn([R3, R3], Real, (u, v) => {
      const x = mul(u[0], v[0]);
      const y = mul(u[1], v[1]);
      const z = mul(u[2], v[2]);
      return add(add(x, y), z);
    });
    const f = fn([], Real, () =>
      dot(vec(Real, [1, 3, -5]), vec(Real, [4, -2, -1])),
    );
    const g = interp(f);
    expect(g()).toBe(3);
  });

  test("cross product", () => {
    const R3 = Vec(3, Real);
    const cross = fn([R3, R3], R3, (u, v) => {
      const x = sub(mul(u[1], v[2]), mul(u[2], v[1]));
      const y = sub(mul(u[2], v[0]), mul(u[0], v[2]));
      const z = sub(mul(u[0], v[1]), mul(u[1], v[0]));
      return vec(Real, [x, y, z]);
    });
    const f = fn([], R3, () =>
      cross(vec(Real, [3, -3, 1]), vec(Real, [4, 9, 2])),
    );
    const g = interp(f);
    expect(g()).toEqual([-15, -2, 39]);
  });

  test("index array", () => {
    const n = 3;
    const f = fn([Vec(n, n), Vec(n, Real)], Vec(n, Real), (i, v) => {
      return vec(Real, [v[i[0]], v[i[1]], v[i[2]]]);
    });
    const g = fn([], Vec(n, Real), () => {
      const v = [2, 0, 1];
      return f(vec(n, v), vec(Real, v));
    });
    const h = interp(g);
    expect(h()).toEqual([1, 2, 0]);
  });
});
