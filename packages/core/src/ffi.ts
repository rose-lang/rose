import * as wasm from "@rose-lang/wasm";
import { Instr } from "@rose-lang/wasm/core/Instr";
import { Type } from "@rose-lang/wasm/core/Type";
import { Val } from "@rose-lang/wasm/interp/Val";

const registry = new FinalizationRegistry((free: () => void) => {
  free();
});

export interface Fn {
  f: wasm.Func;
}

export const makeFunc = (params: Type[], locals: Type[], body: Instr[]): Fn => {
  const f = wasm.makeFunc(params, locals, body);
  const func: Fn = { f };
  registry.register(func, () => f.free());
  return func;
};

export const interp = (f: Fn, args: Val[]): Val => wasm.interp(f.f, args);

export type { Instr, Type, Val };
