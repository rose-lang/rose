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
  const x = ctx.tuple(
    b,
    new Uint32Array(args.map((arg) => getVar(ctx, b, arg)))
  );
  const i = ctx.func(f.f.f, []);
  const y: Var = { ctx, id: ctx.call(b, i, x) };
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

const ffiType = (t: Type): ffi.Type => {
  switch (t.tag) {
    case "Bool": {
      return "Bool";
    }
    case "Real": {
      return "F64";
    }
  }
};

/** Constructs an abstract function with the given `types` for parameters. */
export const fn = <const A extends readonly Type[], R extends Type>(
  // TODO: support generics
  params: A,
  ret: R,
  f: (...args: Args<A>) => Resolve<R>
): Fn & ((...args: Args<A>) => Resolve<R>) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  const paramTypes = params.map(ffiType);
  const retType = ffiType(ret);
  let func: ffi.Fn;
  const { ctx, main, arg, args: ids } = ffi.make(0, paramTypes, retType);
  try {
    setCtx(ctx);
    setBlock(main);
    const x = f(...(ids.map((id): Var => ({ ctx, id })) as Args<A>));
    const y = getVar(ctx, main, x);
    const b = ctx.block(main, arg, y);
    func = ffi.bake(ctx, b);
  } catch (e) {
    // `ctx` and `main` point into Wasm memory, so if we didn't finish the
    // `ffi.bake` call above then we need to be sure to `free` them
    main.free();
    ctx.free();
    throw e;
  } finally {
    setBlock(undefined);
    setCtx(undefined);
  }
  const g = (...args: Args<A>): Resolve<R> =>
    call(g, args as Val[]) as Resolve<R>;
  g.params = params as unknown as Type[];
  g.f = func;
  return g;
};
