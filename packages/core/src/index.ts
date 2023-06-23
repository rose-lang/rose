import * as bool from "./bool.js";
import { Bools, Ints, Reals, Vecs } from "./fn.js";
import * as int from "./int.js";
import * as real from "./real.js";
import * as vec from "./vec.js";

export type Bool = bool.Bool;
export const Bool: Bools = { tag: "Bool" };

export type Int = int.Int;
export const Int: Ints = { tag: "Int" };

export type Real = real.Real;
export const Real: Reals = { tag: "Real" };

export type Vec<T> = vec.Vec<T>;
export const Vec = <T>(t: T, n: int.Int): Vecs<T> => ({ tag: "Vec", t, n });

export { and, cond, iff, not, or, xor } from "./bool.js";
export { compile } from "./compile.js";
export { fn } from "./fn.js";
export { int, mod } from "./int.js";
export { interp } from "./interp.js";
export {
  abs,
  add,
  div,
  eq,
  geq,
  gt,
  leq,
  lt,
  max,
  min,
  mul,
  neg,
  neq,
  prod,
  sub,
  sum,
} from "./overload.js";
export { sqrt } from "./real.js";
export { vec } from "./vec.js";
