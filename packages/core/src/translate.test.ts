import { double, greet } from "@rose-lang/wasm";
import { expect, test } from "vitest";
import { Module } from "./bindings/Module.js";

test("test Rust Hello World", () => {
  expect(greet("Raven")).toBe("Hello, Raven!");
});

test("test different Rust Hello World", () => {
  expect(double(2)).toBe(4);
});

test("test Module type", () => {
  expect("types" in myMod).toBe(true);
});

let myMod: Module = {
  types: [],
  funcs: [
    {
      generics: 0,
      types: [],
      def: {
        params: ["Real", "Real"],
        ret: ["Real"],
        locals: [],
        funcs: [],
        body: [{ Binary: { op: "AddReal" } }],
      },
    },
  ],
};
