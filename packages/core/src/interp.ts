import { Val, Var } from "./context.js";
import * as ffi from "./ffi.js";
import { Fn } from "./fn.js";

type Concrete = null | boolean | number;

const pack = (x: Concrete): ffi.Val => {
  if (x === null) return "Unit";
  if (typeof x === "boolean") return { Bool: x };
  if (typeof x === "number") return { F64: x };
  throw Error("unreachable");
};

const unpack = (x: ffi.Val): Concrete => {
  if (typeof x === "string") return null;
  if ("Bool" in x) return x.Bool;
  if ("F64" in x) return x.F64;
  throw Error("TODO");
};

type Resolve<T> = Exclude<T, Var>;

type Args<T extends readonly Val[]> = {
  [K in keyof T]: Resolve<T[K]>;
};

/**
 * Converts an abstract function into a concrete function using the interpreter.
 */
export const interp =
  <const A extends readonly Val[], R extends Val>(
    f: Fn & ((...args: A) => R),
  ): ((...args: Args<A>) => Resolve<R>) =>
  // just return a closure that calls the interpreter
  (...args: Args<A>) => {
    // TODO: support generics
    const x = ffi.interp(f.f, [], new Uint32Array(), args.map(pack));
    return unpack(x) as Resolve<R>;
  };
