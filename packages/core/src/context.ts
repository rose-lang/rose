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

/** A local that remembers the context in which it was created. */
export interface Local {
  ctx: ffi.Context;
  id: number;
}

/** Push the value of `x` onto the stack if it's still in the right context. */
export const local = (ctx: ffi.Context, x: Local): void => {
  if (x.ctx !== ctx) throw Error("value escaped its context");
  ctx.get(x.id);
};
