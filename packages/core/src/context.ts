import * as ffi from "./ffi.js";

export let context: ffi.Context | undefined = undefined;

export const getCtx = (): ffi.Context => {
  if (context === undefined) throw Error("no `fn` context found");
  return context;
};

export const setCtx = (ctx: ffi.Context): void => {
  context = ctx;
};

export interface Local {
  ctx: ffi.Context;
  id: number;
}
