import { Val, Var, getBlock, getCtx, getVar, setBlock } from "./context.js";
import * as ffi from "./ffi.js";

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

export const cond = <T extends Val>(
  cond: Bool,
  then: () => T,
  els: () => T,
): T | Var => {
  const ctx = getCtx();
  const b = getBlock();

  const p = getVar(ctx, b, cond);

  const at = ctx.varUnit(); // `then` and `els` blocks take in `Unit`-type arg
  const bt = new ffi.Block();
  let nt: number; // block ID
  try {
    setBlock(bt);
    const rt = getVar(ctx, bt, then());
    nt = ctx.block(bt, at, rt);
  } catch (e) {
    bt.free();
    throw e;
  } finally {
    setBlock(b);
  }

  const af = ctx.varUnit();
  const bf = new ffi.Block();
  let nf: number;
  try {
    setBlock(bf);
    const rf = getVar(ctx, bf, els());
    nf = ctx.block(bf, af, rf);
  } catch (e) {
    bf.free();
    throw e;
  } finally {
    setBlock(b);
  }

  return { ctx, id: ctx.cond(b, p, nt, nf) };
};
