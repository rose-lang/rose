import { double, greet, initialize } from "@rose-lang/wasm";
import { expect, test } from "vitest";

initialize();
test("test Rust Hello World", () => {
  expect(greet("Raven")).toBe("Hello, Raven!");
});

test("test Rust Hello World", () => {
  expect(double(2)).toBe(4);
});
