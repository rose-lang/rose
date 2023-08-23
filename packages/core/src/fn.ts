import { Bool } from "./bool.js";
import {
  Val,
  Var,
  context,
  getBlock,
  getCtx,
  getVar,
  setBlock,
  setCtx,
} from "./context.js";
import * as ffi from "./ffi.js";
import { Real } from "./real.js";

export type Bools = { tag: "Bool" };
export type Reals = { tag: "Real" };

export type Type = Bools | Reals;

export interface Fn {
  params: Type[];
  f: ffi.Fn;
}

const call = (f: Fn, args: Val[]): Val => {
  const ctx = getCtx();
  const b = getBlock();
  const vars = new Uint32Array(args.map((arg) => getVar(ctx, b, arg)));
  const generics = new Uint32Array(); // TODO
  const y: Var = { ctx, id: ctx.call(b, f.f.f, generics, vars) };
  return y;
};

type Resolve<T extends Type> = T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : unknown; // TODO: is `unknown` the right default here? what about `never`?

type Args<T extends readonly Type[]> = {
  [K in keyof T]: Resolve<T[K]>;
};

const makeSig = (
  params: readonly Type[],
  ret: Type,
): { types: ffi.Ty[]; params: Uint32Array; ret: number } => {
  // TODO: support non-primitive types
  const types: ffi.Ty[] = ["Bool", "F64"];

  const ty = (t: Type): number => {
    switch (t.tag) {
      case "Bool": {
        return types.indexOf("Bool");
      }
      case "Real": {
        return types.indexOf("F64");
      }
    }
  };

  return {
    types,
    params: new Uint32Array(params.map(ty)),
    ret: ty(ret),
  };
};

/** Constructs an abstract function with the given `types` for parameters. */
export const fn = <const A extends readonly Type[], R extends Type>(
  // TODO: support generics
  params: A,
  ret: R,
  f: (...args: Args<A>) => Resolve<R>,
): Fn & ((...args: Args<A>) => Resolve<R>) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  const sig = makeSig(params, ret);
  let retVar: number;
  const { ctx, main } = ffi.make(0, sig.types, sig.params);
  try {
    setCtx(ctx);
    setBlock(main);
    const x = f(...(params.map((_, id): Var => ({ ctx, id })) as Args<A>));
    retVar = getVar(ctx, main, x);
  } catch (e) {
    // `ctx` and `main` point into Wasm memory, so if we don't reach the
    // `ffi.bake` call below then we need to be sure to `free` them
    main.free();
    ctx.free();
    throw e;
  } finally {
    setBlock(undefined);
    setCtx(undefined);
  }
  const func = ffi.bake(ctx, retVar, main);
  const g = (...args: Args<A>): Resolve<R> =>
    call(g, args as Val[]) as Resolve<R>;
  g.params = params as unknown as Type[];
  g.f = func;
  return g;
};
