import * as ffi from "./ffi.js";
import { Fn } from "./fn.js";
import { Real } from "./real.js";

type Resolve<T extends readonly Real[]> = {
  [K in keyof T]: number;
};

export const interp =
  <const T extends readonly Real[]>(
    f: Fn & ((...args: T) => Real)
  ): ((...args: Resolve<T>) => number) =>
  (...args: Resolve<T>) => {
    const x = ffi.interp(
      f.f,
      args.map((x) => ({ F64: x }))
    );
    if ("F64" in x) return x.F64;
    throw Error(`expected F64, got ${Object.keys(x)}`);
  };
