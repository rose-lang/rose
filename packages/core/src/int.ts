import { Bool } from "./bool.js";
import { Local, getCtx, local } from "./context.js";
import * as ffi from "./ffi.js";
import { Vec, getVec } from "./vec.js";

const bound = 2 ** 32;

class ConstInt {
  private n: number;

  constructor(n: number) {
    if (Number.isInteger(n) && 0 <= n && n < bound) this.n = n;
    else throw Error("not a valid u32");
  }

  val(): number {
    return this.n;
  }
}

export type Int = ConstInt | Local;

/** Throw if `n` is not a valid unsigned 32-bit integer. */
export const int = (n: number): Int => new ConstInt(n);

/** Emit instructions to push the value of `x` onto the stack. */
const getInt = (ctx: ffi.Context, n: Int): void => {
  if (n instanceof ConstInt) ctx.int(n.val());
  else local(ctx, n);
};

const unary =
  (op: (ctx: ffi.Context) => void) =>
  (n: Int): Int => {
    const ctx = getCtx();
    getInt(ctx, n);
    op(ctx);
    return { ctx, id: ctx.set("Int") };
  };

export const neg = unary((ctx) => ctx.negInt());
export const abs = unary((ctx) => ctx.absInt());

const binary =
  (op: (ctx: ffi.Context) => void) =>
  (a: Int, b: Int): Int => {
    const ctx = getCtx();
    getInt(ctx, a);
    getInt(ctx, b);
    op(ctx);
    return { ctx, id: ctx.set("Int") };
  };

export const add = binary((ctx) => ctx.addInt());
export const sub = binary((ctx) => ctx.subInt());
export const mul = binary((ctx) => ctx.mulInt());
export const div = binary((ctx) => ctx.divInt());
export const mod = binary((ctx) => ctx.mod());

const fold =
  (op: (ctx: ffi.Context) => void) =>
  (v: Vec<Int>): Int => {
    const ctx = getCtx();
    getVec(ctx, v);
    op(ctx);
    return { ctx, id: ctx.set("Int") };
  };

export const sum = fold((ctx) => ctx.sumInt());
export const prod = fold((ctx) => ctx.prodInt());
export const max = fold((ctx) => ctx.maxInt());
export const min = fold((ctx) => ctx.minInt());

const comp =
  (op: (ctx: ffi.Context) => void) =>
  (a: Int, b: Int): Bool => {
    const ctx = getCtx();
    getInt(ctx, a);
    getInt(ctx, b);
    op(ctx);
    return { ctx, id: ctx.set("Bool") };
  };

export const neq = comp((ctx) => ctx.neqInt());
export const lt = comp((ctx) => ctx.ltInt());
export const leq = comp((ctx) => ctx.leqInt());
export const eq = comp((ctx) => ctx.eqInt());
export const gt = comp((ctx) => ctx.gtInt());
export const geq = comp((ctx) => ctx.geqInt());
