import { Val } from "./context.js";
import * as ffi from "./ffi.js";
import { Fn } from "./fn.js";

export const derivative = <const A extends Val, R extends Val>(
  f: Fn & ((x: A) => R),
): Fn => {
  const g = ffi.derivative(f.f);
  return { params: [f.params[0], f.params[0]], f: g };
};
