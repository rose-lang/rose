import { Bool } from "./bool.js";
import { Local, getCtx } from "./context.js";
import * as ffi from "./ffi.js";
import { Vec, getVec } from "./vec.js";

export type Real = number | Local;

export const getReal = (ctx: ffi.Context, x: Real): void => {
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
    getReal(ctx, x);
    op(ctx);
    return { ctx, id: ctx.set("Real") };
  };

export const neg = unary((ctx) => ctx.negReal());
export const abs = unary((ctx) => ctx.absReal());
export const sqrt = unary((ctx) => ctx.sqrt());

const binary =
  (op: (ctx: ffi.Context) => void) =>
  (x: Real, y: Real): Real => {
    const ctx = getCtx();
    getReal(ctx, x);
    getReal(ctx, y);
    op(ctx);
    return { ctx, id: ctx.set("Real") };
  };

export const add = binary((ctx) => ctx.addReal());
export const sub = binary((ctx) => ctx.subReal());
export const mul = binary((ctx) => ctx.mulReal());
export const div = binary((ctx) => ctx.divReal());

const fold =
  (op: (ctx: ffi.Context) => void) =>
  (v: Vec<Real>): Real => {
    const ctx = getCtx();
    getVec(ctx, v);
    op(ctx);
    return { ctx, id: ctx.set("Real") };
  };

export const sum = fold((ctx) => ctx.sumReal());
export const prod = fold((ctx) => ctx.prodReal());
export const max = fold((ctx) => ctx.maxReal());
export const min = fold((ctx) => ctx.minReal());

const comp =
  (op: (ctx: ffi.Context) => void) =>
  (x: Real, y: Real): Bool => {
    const ctx = getCtx();
    getReal(ctx, x);
    getReal(ctx, y);
    op(ctx);
    return { ctx, id: ctx.set("Bool") };
  };

export const neq = comp((ctx) => ctx.neqReal());
export const lt = comp((ctx) => ctx.ltReal());
export const leq = comp((ctx) => ctx.leqReal());
export const eq = comp((ctx) => ctx.eqReal());
export const gt = comp((ctx) => ctx.gtReal());
export const geq = comp((ctx) => ctx.geqReal());
