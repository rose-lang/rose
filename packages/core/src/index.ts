import * as bool from "./bool.js";
import { Bools, Reals } from "./fn.js";
import * as real from "./real.js";

export type Bool = bool.Bool;
export const Bool: Bools = { tag: "Bool" };

export type Real = real.Real;
export const Real: Reals = { tag: "Real" };

export { derivative } from "./autodiff.js";
export { and, cond, iff, not, or, xor } from "./bool.js";
export { fn } from "./fn.js";
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
  mul,
  neg,
  neq,
  sqrt,
  sub,
} from "./real.js";
