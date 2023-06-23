import * as ffi from "./ffi.js";
import { Fn, ToJs } from "./fn.js";
import { Real } from "./real.js";

/**
 * Converts an abstract function into a concrete function using the interpreter.
 */
export const compile = async <const T extends readonly Real[]>(
  f: Fn & ((...args: T) => Real),
  generics: number[]
): Promise<(...args: ToJs<T>) => number> => {
  const bytes = ffi.compile(f.f, generics);
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(bytes)
  );
  return instance.exports[ffi.wasmExportName] as any;
};
