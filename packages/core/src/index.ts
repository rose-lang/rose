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

export { and, eq, geq, gt, iff, leq, lt, neq, not, or, xor } from "./bool.js";
export { fn } from "./fn.js";
export { mod } from "./int.js";
export { interp } from "./interp.js";
export { abs, add, div, mul, neg, sub } from "./overload.js";
export { max, min, prod, sqrt, sum } from "./real.js";
export { cond, get } from "./val.js";
export { vec } from "./vec.js";
