import { double, greet } from "@rose-lang/wasm";
import { expect, test } from "vitest";

test("test Rust Hello World", () => {
  expect(greet("Raven")).toBe("Hello, Raven!");
});

test("test Rust Hello World", () => {
  expect(double(2)).toBe(4);
});
