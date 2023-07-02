import { Bool } from "./bool.js";
import { Var, getBlock, getCtx, getVar } from "./context.js";

export type Real = number | Var;

export const neg = (x: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.neg(b, getVar(ctx, b, x)) };
};

export const abs = (x: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.abs(b, getVar(ctx, b, x)) };
};

export const sqrt = (x: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.sqrt(b, getVar(ctx, b, x)) };
};

export const add = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.add(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const sub = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.sub(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const mul = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.mul(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const div = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.div(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const neq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.neq(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const lt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.lt(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const leq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.leq(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const eq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.eq(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const gt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.gt(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};

export const geq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.geq(b, getVar(ctx, b, x), getVar(ctx, b, y)) };
};
