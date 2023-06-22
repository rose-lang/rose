import { Local, getCtx } from "./context.js";
import * as ffi from "./ffi.js";
import { Vec, getVec } from "./vec.js";

export type Real = number | Local;

const getReal = (ctx: ffi.Context, x: Real): void => {
  if (typeof x === "number") ctx.real(x);
  else {
    if (x.ctx !== ctx) throw Error("value escaped its context");
    ctx.get(x.id);
  }
};

const unary =
  (op: (ctx: ffi.Context) => void) =>
  (x: Real): Real => {
    const ctx = getCtx();
    const id = ctx.makeLocal("Real");
    getReal(ctx, x);
    op(ctx);
    ctx.set(id);
    return { ctx, id };
  };

export const neg = unary((ctx) => ctx.negReal());
export const abs = unary((ctx) => ctx.absReal());
export const sqrt = unary((ctx) => ctx.sqrt());

const binary =
  (op: (ctx: ffi.Context) => void) =>
  (a: Real, b: Real): Real => {
    const ctx = getCtx();
    const id = ctx.makeLocal("Real");
    getReal(ctx, a);
    getReal(ctx, b);
    op(ctx);
    ctx.set(id);
    return { ctx, id };
  };

export const add = binary((ctx) => ctx.addReal());
export const sub = binary((ctx) => ctx.subReal());
export const mul = binary((ctx) => ctx.mulReal());
export const div = binary((ctx) => ctx.divReal());

const fold =
  (op: (ctx: ffi.Context) => void) =>
  (v: Vec<Real>): Real => {
    const ctx = getCtx();
    const id = ctx.makeLocal("Real");
    getVec(ctx, v);
    op(ctx);
    ctx.set(id);
    return { ctx, id };
  };

export const sum = unary((ctx) => ctx.sumReal());
export const prod = unary((ctx) => ctx.prodReal());
export const max = unary((ctx) => ctx.maxReal());
export const min = unary((ctx) => ctx.minReal());
