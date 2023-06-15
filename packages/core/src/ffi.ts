import * as wasm from "@rose-lang/wasm";
import { Type } from "@rose-lang/wasm/core/Type";
import { Val } from "@rose-lang/wasm/interp/Val";

const registry = new FinalizationRegistry((free: () => void) => {
  free();
});

export interface Fn {
  f: wasm.Func;
}

export const makeFunc = (
  params: Type[],
  locals: Type[],
  body: wasm.Body
): Fn => {
  const f = wasm.makeFunc(params, locals, body);
  const fn: Fn = { f };
  registry.register(fn, () => f.free());
  return fn;
};

export const interp = (f: Fn, args: Val[]): Val => wasm.interp(f.f, args);

export { Body } from "@rose-lang/wasm";
export type { Type, Val };
