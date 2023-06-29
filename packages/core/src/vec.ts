import { Local } from "./context.js";
import * as ffi from "./ffi.js";
import { Int } from "./int.js";

export type Vec<T> = T[] | Local;

/** Emit instructions to push the value of `v` onto the stack. */
export const getVec = <T>(ctx: ffi.Context, v: Vec<T>): void => {
  throw Error("TODO");
};

export const vec = <T>(n: Int, f: (i: Int) => T): Vec<T> => {
  throw Error("TODO");
};
