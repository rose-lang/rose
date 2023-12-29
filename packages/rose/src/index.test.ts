import { expect, test } from "vitest";
import { add } from "./index.js";

test("add", () => {
  expect(add(2, 2)).toBe(4);
});
