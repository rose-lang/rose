import { Local, getCtx } from "./context.js";
import * as ffi from "./ffi.js";

export type Bool = boolean | Local;

const getBool = (ctx: ffi.Context, x: Bool): void => {
  if (typeof x === "boolean") ctx.bool(x);
  else {
    if (x.ctx !== ctx) throw Error("value escaped its context");
    ctx.get(x.id);
  }
};

export const not = (p: Bool): Bool => {
  const ctx = getCtx();
  getBool(ctx, p);
  ctx.not();
  return { ctx, id: ctx.set("Bool") };
};

const logic =
  (op: (ctx: ffi.Context) => void) =>
  (p: Bool, q: Bool): Bool => {
    const ctx = getCtx();
    getBool(ctx, p);
    getBool(ctx, q);
    op(ctx);
    return { ctx, id: ctx.set("Bool") };
  };

export const and = logic((ctx) => ctx.and());
export const or = logic((ctx) => ctx.or());
export const iff = logic((ctx) => ctx.eqBool());
export const xor = logic((ctx) => ctx.neqBool());

export const cond = <T>(cond: Bool, then: () => T, els: () => T): Local => {
  const ctx = getCtx();
  getBool(ctx, cond);
  ctx.cond();
  then();
  ctx.alt();
  els();
  ctx.end();
  return { ctx, id: ctx.set("Real") }; // TODO: support other types
};
