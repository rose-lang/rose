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

/**
 * Property key for the internal `wasm.Func` object held by a `Fn`.
 *
 * This can of course be reached via `Object.getOwnPropertySymbols`, but that
 * requires an intentional effort to circumvent the encapsulation; such usage is
 * discouraged.
 */
export const inner = Symbol("inner");

/** An abstract function. */
export interface Fn {
  [inner]: wasm.Func;
}

/**
 * An abstract variable.
 *
 * The `Context` is implicit; don't use a `Var` outside of its original context.
 *
 * This class is used for primitive types that can't be indices; index types are
 * instead represented by `Symbol`, and non-primitive types are represented by
 * `Proxy`.
 */
class Var {
  id: number;

  constructor(id: number) {
    this.id = id;
  }
}

/** An abstract null value. */
export type Null = null | Var;

/** An abstract boolean. */
export type Bool = boolean | Var;

/** An abstract 64-bit floating point number. */
export type Real = number | Var;

/** An abstract natural number, which can be used to index into a vector. */
type Nat = number | symbol;

/** An abstract vector, which can be indexed by a natural number. */
export interface Vec<T> {
  [K: Nat]: T;
}

/** A concrete or abstract vector. */
type ArrayOrVec<T> = T[] | Vec<T>;

/** An abstract value that is not a concrete vector. */
type Symbolic = Null | Bool | Real | Nat | Vec<Symbolic>;

/** Any abstract value. */
type Value = Symbolic | Value[];

/** The context for an abstract function under construction. */
interface Context {
  /** Handle to Rust-side information about the function itself. */
  func: wasm.FuncBuilder;

  /** Handle to Rust-side information about the current block being built. */
  block: wasm.Block;

  /** Variable IDs for symbols, which are used to represent abstract indices. */
  symbols: Map<symbol, number>;

  /**
   * Variable IDs for literals (primitives/aggregates), indexed by type ID.
   *
   * For instance, the number `42` may represent a floating-point number, or it
   * may represent a natural number that is being used as an index. Similarly,
   * an array of numbers could be a vector of floating-point numbers or a vector
   * of indices. We need to know the type first before we can interpret it.
   */
  literals: Map<number, Map<Value, number>>;
}

/**
 * Property key for the variable ID of a `Proxy` vector.
 *
 * Such `Proxy` vectors interpret access through any other property key as a
 * request to emit an indexing instruction into the current block.
 */
const variable = Symbol("variable");

/**
 * Return the variable ID for the given abstract value with the given type.
 *
 * Throws if the value does not match the type.
 */
const valId = (ctx: Context, t: number, x: Value): number => {
  // various possible values that don't actually fall under `Value`; these are
  // disallowed by TypeScript, but TypeScript can be circumvented e.g. by `any`
  if (x === undefined) throw Error("undefined value");
  if (typeof x === "bigint") throw Error("bigint not supported");
  if (typeof x === "string") throw Error("string not supported");

  // a `Var` could be misused or even stray from a different context, so we
  // check its type via `expect`
  if (x instanceof Var) return ctx.func.expect(t, x.id);

  if (typeof x === "symbol") {
    const id = ctx.symbols.get(x);
    if (id === undefined) throw Error("undefined symbol");
    return ctx.func.expect(t, id);
  }

  let id: number | undefined;
  if (typeof x === "object" && x !== null) {
    // this means `x` is either an actual `Array` or a `Proxy` vector
    id = (x as any)[variable];
    if (id !== undefined) return ctx.func.expect(t, id); // `Proxy` vector
  }

  let map = ctx.literals.get(t);
  if (map === undefined) {
    // the map is keyed by type, so we don't always have an entry for every
    // possible type; if we're missing one, just initialize it as an empty map
    map = new Map();
    ctx.literals.set(t, map);
  }
  id = map.get(x);
  // see if we already have an entry for this type and value; if so then we're
  // done! no need to call `expect` because we checked it when inserting before
  if (id !== undefined) return id;

  if (x === null) id = ctx.func.unit(t);
  else if (typeof x === "boolean") id = ctx.func.bool(t, x);
  else if (typeof x === "number") id = ctx.func.num(t, x);
  else if (Array.isArray(x)) {
    const size = ctx.func.size(t);
    const elem = ctx.func.elem(t);
    if (x.length !== size) throw Error("wrong array size");
    const xs = new Uint32Array(size);
    // this is an example of why it's good to check for `undefined` above; an
    // array can still have holes even if its length is correct, and that's
    // something TypeScript doesn't check regardless
    for (let i = 0; i < size; ++i) xs[i] = valId(ctx, elem, x[i]);
    id = ctx.func.array(t, xs);
  } else throw Error("unknown kind of value");

  map.set(x, id);
  return id;
};

/** Context for the current function under construction. */
let context: Context | undefined = undefined;

/** Return the current context if there is one; throw otherwise. */
const getCtx = (): Context => {
  if (context === undefined) throw Error("no `fn` context found");
  return context;
};

/** The null type. */
export const Null = Symbol("Null");

/** The boolean type. */
export const Bool = Symbol("Bool");

/** The 64-bit floating-point type. */
export const Real = Symbol("Real");

/** Representation of the null type. */
type Nulls = typeof Null;

/** Representation of the boolean type. */
type Bools = typeof Bool;

/** Representation of the 64-bit floating point type. */
type Reals = typeof Real;

/** Representation of a bounded index type (it's just the upper bound). */
type Nats = number;

/** Representation of a vector type. */
class Vecs<K, V> {
  index: K;
  elem: V;

  constructor(index: K, elem: V) {
    this.index = index;
    this.elem = elem;
  }
}

/** JavaScript-side representation of a type. */
type Type = Nulls | Bools | Reals | Nats | Vecs<Type, Type>;

/** Return the type ID for `ty` in `ctx`, creating the type if needed. */
const tyId = (ctx: Context, ty: Type): number => {
  if (ty === Null) return ctx.func.tyUnit();
  if (ty === Bool) return ctx.func.tyBool();
  if (ty === Real) return ctx.func.tyF64();
  if (typeof ty === "number") return ctx.func.tyFin(ty);
  return ctx.func.tyArray(tyId(ctx, ty.index), tyId(ctx, ty.elem));
};

/** The type of vectors from the index type `K` to the element type `V`. */
export const Vec = <K, V>(index: K, elem: V): Vecs<K, V> =>
  new Vecs(index, elem);

/**
 * Return a symbolic JS representation of the variable with the given `id`.
 *
 * This depends on the type `t` because if the variable is of an index type then
 * it must be represented by a `Symbol`, and if it is of a vector type then it
 * must be represented by a `Proxy`.
 */
const idVal = (ctx: Context, t: number, id: number): Symbolic => {
  if (ctx.func.isSymbol(t)) {
    const sym = Symbol();
    ctx.symbols.set(sym, id);
    return sym;
  } else if (ctx.func.isProxy(t)) return arrayProxy(t, id);
  else return new Var(id);
};

/**
 * Return a `Proxy` vector of type `t` for the variable ID `v`.
 *
 * The original variable ID `v` can be accessed via the `variable` symbol
 * property key. Any string access will be parsed as a literal integer index.
 * Any other symbol access will use the `Context`'s `symbols` map.
 */
const arrayProxy = (t: number, v: number): Vec<Symbolic> => {
  return new Proxy(
    {},
    {
      get: (target, prop): Symbolic => {
        if (prop === variable) return v;
        const ctx = getCtx();
        const index = ctx.func.index(t);
        const elem = ctx.func.elem(t);
        const i = typeof prop === "string" ? parseInt(prop, 10) : prop;
        const x = ctx.block.index(ctx.func, v, valId(ctx, index, i));
        return idVal(ctx, elem, x);
      },
    },
  );
};

/** Insert a call instruction to `f` in the current context. */
const call = (f: Fn, generics: Uint32Array, args: Value[]): Symbolic => {
  const ctx = getCtx();
  // we first need to pull in the signature types from `f` so we know how to
  // interpret the abstract values for its arguments
  const sig = ctx.func.ingest(f[inner], generics);
  const ret = sig[sig.length - 1]; // the last type is the return type
  // TODO: we should probably check that `args` is the right length
  const vars = new Uint32Array(args.map((arg, i) => valId(ctx, sig[i], arg)));
  const id = ctx.block.call(ctx.func, f[inner], generics, ret, vars);
  return idVal(ctx, ret, id);
};

/**
 * Map from a type of a type to the type of the symbolic values it represents.
 *
 * The result should be `Symbolic`, so this should be used in any situation
 * where the abstract value is being synthesized (e.g. the parameters of the
 * body of `fn` or `vec`, or the returned value from a call or `select`).
 */
type ToSymbolic<T extends Type> = T extends Nulls
  ? Null
  : T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : T extends Nats
  ? Nat
  : T extends Vecs<any, infer V extends Type>
  ? Vec<ToSymbolic<V>>
  : unknown; // TODO: is `unknown` the right default here? what about `never`?

/**
 * Map from a type of a type to the type of the abstract values it represents.
 *
 * The result should be `Value`, so this should be used in any situation where
 * the abstract value is provided by the user (e.g. the returned value in the
 * body of `fn` or `vec`, or the inputs to a call or `select).
 */
type ToValue<T extends Type> = T extends Nulls
  ? Null
  : T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : T extends Nats
  ? Nat
  : T extends Vecs<any, infer V extends Type>
  ? Vec<ToValue<V>> | ToValue<V>[]
  : unknown; // TODO: is `unknown` the right default here? what about `never`?

/** Map from a parameter type array to a symbolic parameter value type array. */
type SymbolicParams<T extends readonly Type[]> = {
  [K in keyof T]: ToSymbolic<T[K]>;
};

/** Map from a parameter type array to an abstract parameter value type array. */
type ValueParams<T extends readonly Type[]> = {
  [K in keyof T]: ToValue<T[K]>;
};

/** Constructs an abstract function with the given `types` for parameters. */
export const fn = <const P extends readonly Type[], R extends Type>(
  params: P,
  ret: R,
  f: (...args: SymbolicParams<P>) => ToValue<R>,
): Fn & ((...args: ValueParams<P>) => ToSymbolic<R>) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  let out: number | undefined = undefined; // function return variable ID
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
      ...(params.map((ty): Symbolic => {
        const t = tyId(ctx, ty);
        // it's important that `map` runs eagerly an in order, because the
        // ordering of these calls to `param` must match the order of `params`
        return idVal(ctx, t, ctx.func.param(t));
      }) as SymbolicParams<P>),
    );
    // TODO: we have to wait until after running the function body to typecheck
    // the returned value, but we can check whether the return type itself makes
    // sense before running the function body; maybe we should do that?
    out = valId(ctx, tyId(ctx, ret), x as Value);
  } finally {
    context = undefined;
    if (out === undefined) {
      // if we didn't reach the assignment statement defining `out` then there
      // must have been an error, so we won't reach the below call to
      // `builder.finish`; thus we must free these handles to avoid memory leaks
      body.free();
      builder.free();
    }
  }
  const func = builder.finish(out, body);
  const g: any = (...args: SymbolicParams<P>): ToSymbolic<R> =>
    // TODO: support generics
    call(g, new Uint32Array(), args as Value[]) as ToSymbolic<R>;
  funcs.register(g, func);
  g[inner] = func;
  return g;
};

/** A concrete value. */
type Js = null | boolean | number | Js[];

/** Translate a concrete value from the raw format given by the interpreter. */
const translate = (x: RawVal): Js => {
  if (x === "Unit") return null;
  if ("Bool" in x) return x.Bool;
  if ("F64" in x) return x.F64;
  if ("Fin" in x) return x.Fin;
  if ("Ref" in x) throw Error("Ref not supported");
  if ("Array" in x) return x.Array.map(translate);
  else throw Error("Tuple not supported");
};

/** Map from an abstract value type to its corresponding concrete value type. */
type ToJs<T extends Value> = T extends ArrayOrVec<infer V extends Value>
  ? ToJs<V>[]
  : Exclude<T, Var | symbol>;

/** Concretize the nullary abstract function `f` using the interpreter. */
export const interp =
  <R extends Value>(f: Fn & (() => R)): (() => ToJs<R>) =>
  // TODO: support interpreting functions with parameters and generics
  () =>
    translate(wasm.interp(f[inner])) as ToJs<R>;

/** Return the variable ID for the abstract boolean `x`. */
const boolId = (ctx: Context, x: Bool): number =>
  valId(ctx, ctx.func.tyBool(), x);

/** Return the negation of the abstract boolean `p`. */
export const not = (p: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.not(ctx.func, boolId(ctx, p)));
};

/** Return the conjunction of the abstract booleans `p` and `q`. */
export const and = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.and(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the disjunction of the abstract booleans `p` and `q`. */
export const or = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.or(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the biconditional of the abstract booleans `p` and `q`. */
export const iff = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.iff(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the exclusive disjunction of the abstract booleans `p` and `q`. */
export const xor = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.xor(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return an abstract value selecting between `then` and `els` via `cond`. */
export const select = <T extends Type>(
  cond: Bool,
  ty: T,
  then: ToValue<T>,
  els: ToValue<T>,
): ToSymbolic<T> => {
  const ctx = getCtx();
  const t = tyId(ctx, ty);
  const p = boolId(ctx, cond);
  const a = valId(ctx, t, then as Value);
  const b = valId(ctx, t, els as Value);
  return idVal(ctx, t, ctx.block.select(ctx.func, p, t, a, b)) as ToSymbolic<T>;
};

/** Return the variable ID for the abstract floating point number `x`. */
const realId = (ctx: Context, x: Real): number =>
  valId(ctx, ctx.func.tyF64(), x);

/** Return the negative of the abstract number `x`. */
export const neg = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.neg(ctx.func, realId(ctx, x)));
};

/** Return the absolute value of the abstract number `x`. */
export const abs = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.abs(ctx.func, realId(ctx, x)));
};

/** Return the square root of the abstract number `x`. */
export const sqrt = (x: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.sqrt(ctx.func, realId(ctx, x)));
};

/** Return the abstract number `x` plus the abstract number `y`. */
export const add = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.add(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` minus the abstract number `y`. */
export const sub = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.sub(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` times the abstract number `y`. */
export const mul = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.mul(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` divided by the abstract number `y`. */
export const div = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return new Var(ctx.block.div(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is not equal to `y`. */
export const neq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.neq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is less than `y`. */
export const lt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.lt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is less than or equal to `y`. */
export const leq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.leq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is equal to `y`. */
export const eq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.eq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is greater than `y`. */
export const gt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.gt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is greater than or equal to `y`. */
export const geq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return new Var(ctx.block.geq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract vector by computing each element via `f`. */
export const vec = <I extends Type, T extends Type>(
  index: I,
  elem: T,
  f: (i: ToSymbolic<I>) => ToValue<T>,
): Vec<ToSymbolic<T>> => {
  const ctx = getCtx();
  const i = tyId(ctx, index);
  const e = tyId(ctx, elem);
  const t = ctx.func.tyArray(i, e);
  const arg = ctx.func.bind(i);
  const block = ctx.block;
  let out: number | undefined = undefined;
  const body = new wasm.Block();
  try {
    ctx.block = body;
    out = valId(ctx, e, f(idVal(ctx, i, arg) as ToSymbolic<I>) as Value);
  } finally {
    if (out === undefined) body.free();
    ctx.block = block;
  }
  const id = block.vec(ctx.func, t, arg, body, out);
  return idVal(ctx, t, id) as Vec<ToSymbolic<T>>;
};
