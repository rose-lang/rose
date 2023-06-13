import { body_from_js, body_to_js } from "@rose-lang/wasm";
import { Instr } from "@rose-lang/wasm/core/Instr";
import { expect, test } from "vitest";

let body: Instr[] = [{ Binary: { op: "AddReal" } }];

const modifiedBody = body_from_js(body);
const body2 = body_to_js(modifiedBody);
modifiedBody.free();

test("test Module type", () => {
  expect(body2).toStrictEqual(body);
});
