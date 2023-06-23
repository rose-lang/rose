import { context, setCtx } from "./context.js";
import * as ffi from "./ffi.js";
import { Int } from "./int.js";
import { Real, getReal } from "./real.js";

export type Bools = { tag: "Bool" };
export type Ints = { tag: "Int" };
export type Reals = { tag: "Real" };

export interface Vecs<T> {
  tag: "Vec";
  t: T;
  n: Int;
}

export type Type = Bools | Ints | Reals | Vecs<Type>;

export interface Fn {
  params: Type[];
  f: ffi.Fn;
}

type FromType<T extends readonly Reals[]> = {
  [K in keyof T]: Real;
};

export type ToJs<T extends readonly Real[]> = {
  [K in keyof T]: number;
};

const ffiType = (t: Type): ffi.Type => {
  switch (t.tag) {
    case "Bool":
    case "Int":
    case "Real": {
      return t.tag;
    }
    case "Vec": {
      throw Error("TODO");
    }
  }
};

/** Constructs an abstract function with the given `types` for parameters. */
// TODO: allow args other than `Real`
export const fn = <const T extends readonly Reals[]>(
  types: T,
  f: (...args: FromType<T>) => Real // TODO: allow return other than `Real`
): Fn & ((...args: FromType<T>) => Real) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  const paramTypes = types.map(ffiType);
  let func: ffi.Fn;
  const ctx = new ffi.Context(paramTypes, "Real");
  try {
    setCtx(ctx);
    const params: Real[] = [];
    // reverse because stack is LIFO
    for (let i = types.length - 1; i >= 0; --i)
      params.push({ ctx, id: ctx.set(paramTypes[i]) });
    getReal(ctx, f(...(params.reverse() as FromType<T>)));
    func = ffi.bake(ctx);
  } catch (e) {
    // `ctx` points into Wasm memory, so if we didn't finish the `ffi.bake` call
    // above then we need to be sure to `free` it
    ctx.free();
    throw e;
  } finally {
    setCtx(undefined);
  }
  const g = (...args: FromType<T>): Real => {
    throw Error("TODO");
  };
  g.params = types as unknown as Type[];
  g.f = func;
  return g;
};
