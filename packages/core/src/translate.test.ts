import { module_from_js, module_to_js } from "@rose-lang/wasm";
import { Module } from "@rose-lang/wasm/core/Module";
import { expect, test } from "vitest";

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

const modifiedMod = module_from_js(myMod);
const myMod2 = module_to_js(modifiedMod);
modifiedMod.free();

test("test Module type", () => {
  expect(myMod2).toStrictEqual(myMod);
});
