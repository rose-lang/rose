import * as ffi from "./ffi.js";

/** Context for the current function under construction. */
export let context: ffi.Context | undefined = undefined;

/** Throw if there's no current context. */
export const getCtx = (): ffi.Context => {
  if (context === undefined) throw Error("no `fn` context found");
  return context;
};

export const setCtx = (ctx: ffi.Context | undefined): void => {
  context = ctx;
};

/** A local variable that remembers the context in which it was created. */
export interface Var {
  ctx: ffi.Context;
  id: number;
}

export let block: ffi.Block | undefined = undefined;

export const getBlock = (): ffi.Block => {
  if (block === undefined) throw Error("no block found");
  return block;
};

export const setBlock = (b: ffi.Block | undefined): void => {
  block = b;
};

export type Val = boolean | number | Var;

export const getVar = (ctx: ffi.Context, b: ffi.Block, x: Val): number => {
  if (typeof x === "boolean") return ctx.bool(b, x);
  if (typeof x === "number") return ctx.f64(b, x);
  if (x.ctx === ctx) return x.id;
  throw Error("value escaped its context");
};
