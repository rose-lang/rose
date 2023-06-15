import * as ffi from "./ffi.js";
import { Fn } from "./fn.js";
import { Real } from "./real.js";

type Resolve<T extends readonly Real[]> = {
  [K in keyof T]: number;
};

/**
 * Converts an abstract function into a concrete function using the interpreter.
 */
export const interp =
  <const T extends readonly Real[]>(
    f: Fn & ((...args: T) => Real)
  ): ((...args: Resolve<T>) => number) =>
  // just return a closure
  (...args: Resolve<T>) => {
    // that calls the interpreter
    const x = ffi.interp(
      f.f,
      args.map((x) => ({ F64: x })) // TODO: allow args other than just real
    );
    if ("F64" in x) return x.F64; // TODO: allow return other than just real
    throw Error(`expected F64, got ${Object.keys(x)}`);
  };
