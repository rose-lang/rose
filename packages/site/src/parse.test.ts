import { add } from "rose";
import { expect, test } from "vitest";
import { Expr, parse } from "./parse.js";

test("add", () => {
  const expected: Expr = {
    kind: "binary",
    f: add,
    lhs: { kind: "var", idx: 0 },
    rhs: { kind: "var", idx: 1 },
  };
  expect(parse("x+y")).toEqual(expected);
});
