import { Val, Var, getBlock, getCtx, getVar } from "./context.js";

export type Bool = boolean | Var;

export const not = (p: Bool): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.not(b, getVar(ctx, b, p)) };
};

export const and = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.and(b, getVar(ctx, b, p), getVar(ctx, b, q)) };
};

export const or = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.or(b, getVar(ctx, b, p), getVar(ctx, b, q)) };
};

export const iff = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.iff(b, getVar(ctx, b, p), getVar(ctx, b, q)) };
};

export const xor = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  const b = getBlock();
  return { ctx, id: ctx.xor(b, getVar(ctx, b, p), getVar(ctx, b, q)) };
};

export const select = <T extends Val>(cond: Bool, then: T, els: T): T | Var => {
  const ctx = getCtx();
  const b = getBlock();
  const p = getVar(ctx, b, cond);
  const t = getVar(ctx, b, then);
  const e = getVar(ctx, b, els);
  return { ctx, id: ctx.select(b, p, t, e) };
};
