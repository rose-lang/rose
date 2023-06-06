import { double, greet, Module } from "@rose-lang/wasm";
import { expect, test } from "vitest";

test("test Rust Hello World", () => {
  expect(greet("Raven")).toBe("Hello, Raven!");
});

test("test different Rust Hello World", () => {
  expect(double(2)).toBe(4);
});

test("test Module type", () => {
  let x = new Module();
  expect(x instanceof Module).toBe(true);
});
