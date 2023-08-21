import * as wasm from "@rose-lang/wasm";
import { Val as RawVal } from "@rose-lang/wasm/interp/Val";

/**
 * The user-constructed functions we expose in our API hold reference-counted
 * pointers to Rust objects living in WebAssembly memory. To avoid memory leaks,
 * we use this finalization registry to free those objects once they are no
 * longer referenced by any JavaScript objects. This registry takes a
 * `wasm.Func` object, so that can't directly be what we hold onto: instead, the
 * `Fn` type holds a `wasm.Func` internally, so we register that as the target
 * and pass the internal value to be finalized. This does mean that if a user
 * reaches in and pulls out the wrapped object, it could become invalid if they
 * don't hold onto the wrapper, so that inner object must be treated as private.
 */
const funcs = new FinalizationRegistry((f: wasm.Func) => f.free());

const inner = Symbol("inner");

export interface Fn {
  [inner]: wasm.Func;
}

export const pprint = (f: Fn): string => wasm.pprint(f[inner]);

class Var {
  id: number;

  constructor(id: number) {
    this.id = id;
  }
}

export type Null = null | Var;

export type Bool = boolean | Var;

export type Real = number | Var;

export type Nat = number | symbol;

export type Generic = symbol;

type Val = Null | Bool | Real | Nat | Generic | Val[];

interface Context {
  func: wasm.FuncBuilder;
  block: wasm.Block;
  symbols: Map<symbol, number>;
  literals: Map<number, Map<Val, number>>;
}

const valId = (ctx: Context, t: number, x: Val): number => {
  if (x instanceof Var) return ctx.func.expect(t, x.id);

  if (typeof x === "symbol") {
    const id = ctx.symbols.get(x);
    if (id === undefined) throw Error("undefined symbol");
    return ctx.func.expect(t, id);
  }

  let map = ctx.literals.get(t);
  if (map === undefined) {
    map = new Map();
    ctx.literals.set(t, map);
  }
  let id = map.get(x);
  if (id !== undefined) return ctx.func.expect(t, id);

  if (x === null) id = ctx.func.unit(t);
  else if (typeof x === "boolean") id = ctx.func.bool(t, x);
  else if (typeof x === "number") id = ctx.func.num(t, x);
  else {
    const size = ctx.func.size(t);
    const elem = ctx.func.elem(t);
    const xs = new Uint32Array(size);
    for (let i = 0; i < size; ++i) xs[i] = valId(ctx, elem, x[i]);
    id = ctx.func.array(t, xs);
  }

  map.set(x, id);
  return id;
};

/** Context for the current function under construction. */
let context: Context | undefined = undefined;

/** Throw if there's no current context. */
export const getCtx = (): Context => {
  if (context === undefined) throw Error("no `fn` context found");
  return context;
};

export const Null = Symbol("Null");
export const Bool = Symbol("Bool");
export const Real = Symbol("Real");

type Nulls = typeof Null;

type Bools = typeof Bool;

type Reals = typeof Real;

type Nats = number;

class Generics {
  id: number;

  constructor(id: number) {
    this.id = id;
  }
}

class Vecs<K, V> {
  index: K;
  elem: V;

  constructor(index: K, elem: V) {
    this.index = index;
    this.elem = elem;
  }
}

type Type = Nulls | Bools | Reals | Nats | Generics | Vecs<Type, Type>;

const tyId = (ctx: Context, ty: Type): number => {
  if (ty === Null) return ctx.func.tyUnit();
  if (ty === Bool) return ctx.func.tyBool();
  if (ty === Real) return ctx.func.tyF64();
  if (typeof ty === "number") return ctx.func.tyFin(ty);
  if (ty instanceof Generics) return ctx.func.tyGeneric(ty.id);
  return ctx.func.tyArray(tyId(ctx, ty.index), tyId(ctx, ty.elem));
};

export const Vec = <K, V>(index: K, elem: V): Vecs<K, V> =>
  new Vecs(index, elem);

const idVal = (ctx: Context, t: number, id: number): Val => {
  if (ctx.func.isSymbol(t)) {
    const sym = Symbol();
    ctx.symbols.set(sym, id);
    return sym;
  } else if (ctx.func.isArray(t)) return arrayProxy(t, id);
  else return new Var(id);
};

const arrayProxy = (t: number, v: number): Val[] => {
  return new Proxy(
    {},
    {
      get: (target, prop): Val => {
        const ctx = getCtx();
        const index = ctx.func.index(t);
        const elem = ctx.func.elem(t);
        const i = typeof prop === "string" ? parseInt(prop, 10) : prop;
        const x = ctx.block.index(ctx.func, v, valId(ctx, index, i));
        return idVal(ctx, elem, x);
      },
    },
  ) as Val[];
};

const call = (f: Fn, generics: Uint32Array, args: Val[]): Val => {
  const ctx = getCtx();
  const sig = ctx.func.ingest(f[inner], generics);
  const ret = sig[sig.length - 1];
  const vars = new Uint32Array(args.map((arg, i) => valId(ctx, sig[i], arg)));
  const id = ctx.block.call(ctx.func, f[inner], generics, ret, vars);
  return idVal(ctx, ret, id);
};

type FromType<T extends Type> = T extends Nulls
  ? Null
  : T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : T extends Nats
  ? Nat
  : T extends Generics
  ? Generic
  : T extends Vecs<any, infer V extends Type>
  ? FromType<V>[]
  : unknown; // TODO: is `unknown` the right default here? what about `never`?

type Params<T extends readonly Type[]> = {
  [K in keyof T]: FromType<T[K]>;
};

/** Constructs an abstract function with the given `types` for parameters. */
export const fn = <const P extends readonly Type[], R extends Type>(
  params: P,
  ret: R,
  f: (...args: Params<P>) => FromType<R>,
): Fn & ((...args: Params<P>) => FromType<R>) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  let out: number | undefined = undefined;
  const builder = new wasm.FuncBuilder(0); // TODO: support generics
  const body = new wasm.Block();
  try {
    const ctx = {
      func: builder,
      block: body,
      symbols: new Map(),
      literals: new Map(),
    };
    context = ctx;
    const x = f(
      ...(params.map((ty): Val => {
        const t = tyId(ctx, ty);
        return idVal(ctx, t, ctx.func.param(t));
      }) as Params<P>),
    );
    out = valId(ctx, tyId(ctx, ret), x);
  } finally {
    context = undefined;
    if (out === undefined) {
      body.free();
      builder.free();
    }
  }
  const func = builder.finish(out, body);
  const g: any = (...args: Params<P>): FromType<R> =>
    // TODO: support generics
    call(g, new Uint32Array(), args as Val[]) as FromType<R>;
  g[inner] = func;
  return g;
};

type Js = null | boolean | number | Js[];

const translate = (x: RawVal): Js => {
  if (x === "Unit") return null;
  if ("Bool" in x) return x.Bool;
  if ("F64" in x) return x.F64;
  if ("Fin" in x) return x.Fin;
  if ("Ref" in x) throw Error("Ref not supported");
  if ("Array" in x) return x.Array.map(translate);
  else throw Error("Tuple not supported");
};

type ToJs<T extends Val> = T extends (infer V extends Val)[]
  ? ToJs<V>[]
  : Exclude<T, Var | symbol>;

// TODO: support interpreting functions with parameters and generics
export const interp =
  <R extends Val>(f: Fn & (() => R)): (() => ToJs<R>) =>
  () =>
    translate(wasm.interp(f[inner])) as ToJs<R>;

const boolId = (ctx: Context, x: Bool): number =>
  valId(ctx, ctx.func.tyBool(), x);

export const not = (p: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.not(ctx.func, boolId(ctx, p)));
};

export const and = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.and(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

export const or = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.or(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

export const iff = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.iff(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

export const xor = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.xor(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

export const select = <T extends Type>(
  cond: Bool,
  ty: T,
  then: FromType<T>,
  els: FromType<T>,
): FromType<T> => {
  const ctx = getCtx();
  const t = tyId(ctx, ty);
  const p = boolId(ctx, cond);
  const a = valId(ctx, t, then);
  const b = valId(ctx, t, els);
  return idVal(ctx, t, ctx.block.select(ctx.func, p, t, a, b)) as FromType<T>;
};

const realId = (ctx: Context, x: Real): number =>
  valId(ctx, ctx.func.tyF64(), x);

export const neg = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.neg(ctx.func, realId(ctx, x)));
};

export const abs = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.abs(ctx.func, realId(ctx, x)));
};

export const sqrt = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.sqrt(ctx.func, realId(ctx, x)));
};

export const add = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.add(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const sub = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.sub(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const mul = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.mul(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const div = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.div(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const neq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.neq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const lt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.lt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const leq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.leq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const eq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.eq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const gt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.gt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

export const geq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.geq(ctx.func, realId(ctx, x), realId(ctx, y)));
};
