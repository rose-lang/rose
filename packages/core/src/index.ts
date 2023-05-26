import * as bool from "./bool.js";
import { Bools, Fn, Ints, Reals, Type, Vecs } from "./fn.js";
import * as int from "./int.js";
import * as real from "./real.js";
import * as vec from "./vec.js";

export type Bool = bool.Bool;
export const Bool = "𝔹" as Bools;

export type Int = int.Int;
export const Int = "ℤ" as Ints;

export type Real = real.Real;
export const Real = "ℝ" as Reals;

export type Vec<T> = vec.Vec<T>;
export const Vec = <T>(t: T, n: int.Int): Vecs<T> => ({ t, n });

interface Generic {
  [n: string]: Int;
}

export const generic = <T>(f: (params: Generic) => T): T =>
  f(new Proxy({}, { get: () => Symbol() }));

type Resolve<T> = T extends Bools
  ? bool.Bool
  : T extends Ints
  ? int.Int
  : T extends Reals
  ? real.Real
  : T extends Vecs<infer U>
  ? vec.Vec<Resolve<U>>
  : unknown;

export const fn = (<T>(types: Type[], f: (...args: any[]) => T): Fn<T> => {
  const params: [symbol, Type][] = types.map((t) => [Symbol(), t]);
  const body = f(...params.map(([x]) => x));
  const g: Fn<T> = { params, body };
  const h = (...args: any[]) => ({
    tag: "val",
    val: { tag: "call", f: g, args },
  });
  h.params = params;
  h.body = body;
  return h;
}) as any as {
  <T, A extends Type>(types: [A], f: (a: Resolve<A>) => T): Fn<T> &
    ((a: Resolve<A>) => T);

  <T, A extends Type, B extends Type>(
    types: [A, B],
    f: (a: Resolve<A>, b: Resolve<B>) => T
  ): Fn<T> & ((a: Resolve<A>, b: Resolve<B>) => T);

  <T, A extends Type, B extends Type, C extends Type>(
    types: [A, B, C],
    f: (a: Resolve<A>, b: Resolve<B>, c: Resolve<C>) => T
  ): Fn<T> & ((a: Resolve<A>, b: Resolve<B>, c: Resolve<C>) => T);

  <T, A extends Type, B extends Type, C extends Type, D extends Type>(
    types: [A, B, C, D],
    f: (a: Resolve<A>, b: Resolve<B>, c: Resolve<C>, d: Resolve<D>) => T
  ): Fn<T> &
    ((a: Resolve<A>, b: Resolve<B>, c: Resolve<C>, d: Resolve<D>) => T);
};

export { and, eq, geq, gt, iff, leq, lt, neq, not, or, xor } from "./bool.js";
export { mod } from "./int.js";
export { abs, add, div, mul, neg, sub } from "./overload.js";
export { max, min, prod, sqrt, sum } from "./real.js";
export { cond, get } from "./val.js";
export { vec } from "./vec.js";
