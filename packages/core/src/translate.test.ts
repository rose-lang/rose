import * as wasm from "@rose-lang/wasm";
import { Instr } from "@rose-lang/wasm/core/Instr";
import { Type } from "@rose-lang/wasm/core/Type";
import { Val } from "@rose-lang/wasm/interp/Val";
import { expect, test } from "vitest";

const registry = new FinalizationRegistry((free: () => void) => {
  free();
});

interface Func {
  f: wasm.Func;
}

const makeFunc = (params: Type[], locals: Type[], body: Instr[]): Func => {
  const f = wasm.makeFunc(params, locals, body);
  const func: Func = { f };
  registry.register(func, () => f.free());
  return func;
};

const interp = (f: Func, args: Val[]): Val => wasm.interp(f.f, args);

test("2 + 2", () => {
  const f = makeFunc(["Real", "Real"], [], [{ Binary: { op: "AddReal" } }]);
  const x = interp(f, [{ F64: 2 }, { F64: 2 }]);
  expect(x).toStrictEqual({ F64: 4 });
});
