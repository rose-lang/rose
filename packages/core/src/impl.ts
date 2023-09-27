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

/** Property key for a `Fn`'s string array for resolving struct key names. */
const strings = Symbol("strings");

interface FnBase {
  [inner]: wasm.Func;
  [strings]: string[];
}

/** An abstract function. */
export interface Fn extends FnBase {
  jvp?: Fn;
}

/** Adds `f` to the registry, mutates it into a full `Fn`, then returns it. */
const makeFn = (f: FnBase): Fn => {
  funcs.register(f, f[inner]);
  Object.defineProperty(f, "jvp", {
    set(g: Fn) {
      f[inner].setJvp(g[inner]);
    },
  });
  return f as Fn;
};

/** Property key for a variable ID. */
const variable = Symbol("variable");

/**
 * An abstract variable.
 *
 * The `Context` is implicit; don't use a `Var` outside of its original context.
 *
 * This class is used for primitive types that can't be indices; index types are
 * instead represented by `Symbol`, and non-primitive types are represented by
 * `Proxy`.
 */
interface Var {
  [variable]: number;
}

/**
 * Construct an abstract variable with the given `id`.
 *
 * This should not be used for any values that instead needs to be represented
 * as a `Symbol` or a `Proxy`.
 */
const newVar = (id: number): Var => ({ [variable]: id });

/** An abstract null value. */
export type Null = null | Var;

/** An abstract boolean. */
export type Bool = boolean | Var;

/** An abstract 64-bit floating point number. */
export type Real = number | Var;

/** An abstract 64-bit floating point tangent number. */
export type Tan = number | Var;

/** An abstract natural number, which can be used to index into a vector. */
type Nat = number | symbol;

/** An abstract vector, which can be indexed by a natural number. */
export interface Vec<T> {
  [K: Nat]: T;
}

/** The context for an abstract function under construction. */
interface Context {
  /** Handle to Rust-side information about the function itself. */
  func: wasm.FuncBuilder;

  /** Handle to Rust-side information about the current block being built. */
  block: wasm.Block;

  /**
   * Variable IDs for abstract values, indexed by type ID.
   *
   * For instance, the number `42` may represent a floating-point number, or it
   * may represent a natural number that is being used as an index. Similarly,
   * an array of numbers could be a vector of floating-point numbers or a vector
   * of indices. We need to know the type first before we can interpret it.
   */
  variables: Map<number, Map<unknown, number>>;

  /** The actual string represented each string ID. */
  strings: string[];

  /** Reverse lookup from strings to IDs; also used for deduplication. */
  stringIds: Map<string, number>;
}

/** Struct field name for the nonlinear part of a dual number. */
const re = "re";

/** Struct field name for the linear part of a dual number. */
const du = "du";

/** String ID for `"re"`. */
const reId = 0;

/** String ID for `"du"`. */
const duId = 1;

/** Return a fresh initial string cache for constructing a new function. */
const initStrings = (): {
  strings: string[];
  stringIds: Map<string, number>;
} => ({
  strings: [re, du], // order matches `reId` and `duId`
  stringIds: new Map([
    [re, reId],
    [du, duId],
  ]),
});

/** Return the variable ID map for type ID `t`, creating it if necessary. */
const typeMap = (ctx: Context, t: number): Map<unknown, number> => {
  let map = ctx.variables.get(t);
  if (map === undefined) {
    // the map is keyed by type, so we don't always have an entry for every
    // possible type; if we're missing one, just initialize it as an empty map
    map = new Map();
    ctx.variables.set(t, map);
  }
  return map;
};

/**
 * Return the variable ID for the given abstract value with the given type.
 *
 * Throws if the value does not match the type.
 */
const valId = (ctx: Context, t: number, x: unknown): number => {
  const map = typeMap(ctx, t);
  let id = map.get(x);
  if (id !== undefined) return ctx.func.expect(t, id); // check if out of scope

  if (typeof x === "boolean") id = ctx.func.bool(t, x);
  else if (typeof x === "number") id = ctx.func.num(t, x);
  else if (typeof x === "object") {
    if (x === null) id = ctx.func.unit(t);
    else {
      id = (x as any)[variable];
      // if `x` is a `Var` or some sort of `Proxy` that we constructed then the
      // `variable` property will be present and it will be a number, but
      // otherwise it shouldn't be present; we just get it directly and check
      // its type instead of using JavaScript's `in` operator, because
      // TypeScript doesn't really like that here, and also because it's
      // possible that someone manually reached in and extracted the `variable`
      // symbol from somewhere else and put it somewhere it wasn't supposed to
      // be; that's one of the reasons we still call `expect` here to
      // double-check the type (the other reasons being that a `Var` could be
      // accidentally used outside its original `Context`, or inside its
      // original context but after it has already dropped out of scope)
      if (typeof id === "number") id = ctx.func.expect(t, id);
      else {
        if (Array.isArray(x)) {
          // arrays
          const size = ctx.func.size(t);
          const elem = ctx.func.elem(t);
          if (x.length !== size) throw Error("wrong array size");
          const xs = new Uint32Array(size);
          for (let i = 0; i < size; ++i) xs[i] = valId(ctx, elem, x[i]);
          id = ctx.func.array(t, xs);
        } else {
          // structs
          const keys = ctx.func.keys(t);
          const mems = ctx.func.members(t);
          const entries = Object.entries(x).sort(([a], [b]) => compare(a, b));
          if (
            !(
              entries.length === keys.length &&
              // they're sorted
              entries.every(([k], i) => k === ctx.strings[keys[i]])
            )
          )
            throw Error("wrong struct keys");
          const ys = new Uint32Array(
            entries.map(([, y], i) => valId(ctx, mems[i], y)),
          );
          id = ctx.func.obj(t, ys);
        }
      }
    }
  } else throw Error(`invalid value: ${x}`);

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

/** The 64-bit floating-point tangent type. */
export const Tan = Symbol("Tan");

/** Representation of the null type. */
type Nulls = typeof Null;

/** Representation of the boolean type. */
type Bools = typeof Bool;

/** Representation of the 64-bit floating point type. */
type Reals = typeof Real;

/** Representation of the 64-bit floating point tangent type. */
type Tans = typeof Tan;

/** Representation of a bounded index type (it's just the upper bound). */
type Nats = number;

/** Property key for the index type ID of a vector type. */
const ind = Symbol("index");

/** Property key for the element type ID of a vector type. */
const elm = Symbol("elem");

/** Representation of a vector type. */
interface Vecs<K, V> {
  [ind]: K;
  [elm]: V;
}

/** The type of vectors from the index type `K` to the element type `V`. */
export const Vec = <K, V>(index: K, elem: V): Vecs<K, V> => {
  return { [ind]: index, [elm]: elem };
};

/** The 128-bit floating-point dual number type. */
export const Dual = { re: Real, du: Tan } as const;

// TODO: make this locale-independent
const compare = (a: string, b: string): number => a.localeCompare(b);

/** Return an array of the IDs for the given `strs` in `ctx`. */
const intern = (ctx: Context, strs: string[]): Uint32Array =>
  new Uint32Array(
    strs.map((s) => {
      let i = ctx.stringIds.get(s);
      if (i === undefined) {
        i = ctx.strings.length;
        ctx.strings.push(s);
        ctx.stringIds.set(s, i);
      }
      return i;
    }),
  );

/** Return the type ID for `ty` in `ctx`, creating the type if needed. */
const tyId = (ctx: Context, ty: unknown): number => {
  if (ty === Null) return ctx.func.tyUnit();
  else if (ty === Bool) return ctx.func.tyBool();
  else if (ty === Real) return ctx.func.tyF64();
  else if (ty === Tan) return ctx.func.tyT64();
  else if (typeof ty === "number") return ctx.func.tyFin(ty);
  else if (typeof ty === "object" && ty !== null) {
    if (ind in ty && elm in ty)
      // arrays
      return ctx.func.tyArray(tyId(ctx, ty[ind]), tyId(ctx, ty[elm]));
    else {
      // structs
      const entries = Object.entries(ty).sort(([a], [b]) => compare(a, b));
      const strs = intern(
        ctx,
        entries.map(([s]) => s),
      );
      const tys = new Uint32Array(entries.map(([, t]) => tyId(ctx, t)));
      return ctx.func.tyStruct(strs, tys);
    }
  } else throw Error("invalid type");
};

/**
 * Return a symbolic JS representation of the variable with the given `id`.
 *
 * This depends on the type `t` because if the variable is of an index type then
 * it must be represented by a `Symbol`, and if it is of a vector type then it
 * must be represented by a `Proxy`.
 */
const idVal = (ctx: Context, t: number, id: number): unknown => {
  if (ctx.func.isSymbol(t)) {
    const sym = Symbol();
    typeMap(ctx, t).set(sym, id);
    return sym;
  } else if (ctx.func.isArray(t)) return arrayProxy(ctx, t, id);
  else if (ctx.func.isStruct(t)) return structProxy(ctx, t, id);
  else return newVar(id);
};

/**
 * Return a `Proxy` vector of type `t` for the variable ID `v`.
 *
 * The original variable ID `v` can be accessed via the `variable` symbol
 * property key. Any string access will be parsed as a literal integer index.
 * Any other symbol access will use the `Context`'s `symbols` map.
 */
const arrayProxy = (ctx: Context, t: number, v: number): Vec<unknown> => {
  const index = ctx.func.index(t);
  const elem = ctx.func.elem(t);
  return new Proxy(
    {},
    {
      get: (target, prop) => {
        if (getCtx() !== ctx) throw Error("array escaped its context");
        if (prop === variable) return v;
        const i = typeof prop === "string" ? parseInt(prop, 10) : prop;
        const x = ctx.block.index(ctx.func, elem, v, valId(ctx, index, i));
        return idVal(ctx, elem, x);
      },
    },
  );
};

/**
 * Return a `Proxy` struct of type `t` for the variable ID `x`.
 *
 * The original variable ID `x` can be accessed via the `variable` symbol
 * property key. Any other symbol access will throw an `Error`. Any string
 * access will emit a member instruction if the key is valid, throw otherwise.
 */
const structProxy = (
  ctx: Context,
  t: number,
  x: number,
): { [K: string]: unknown } => {
  const keys = ctx.func.keys(t);
  const mems = ctx.func.members(t);
  const map = new Map<string, number>();
  for (let i = 0; i < keys.length; ++i) map.set(ctx.strings[keys[i]], i);
  return new Proxy(
    {},
    {
      get: (target, prop) => {
        if (getCtx() !== ctx) throw Error("struct escaped its context");
        if (prop === variable) return x;
        if (typeof prop === "symbol") throw Error("unexpected symbol");
        const i = map.get(prop);
        if (i === undefined) throw Error("unexpected key");
        const t = mems[i];
        const y = ctx.block.member(ctx.func, t, x, i);
        return idVal(ctx, t, y);
      },
    },
  );
};

/** Insert a call instruction to `f` in the current context. */
const call = (f: Fn, generics: Uint32Array, args: unknown[]): unknown => {
  const ctx = getCtx();
  const strs = intern(ctx, f[strings]);
  // we first need to pull in the signature types from `f` so we know how to
  // interpret the abstract values for its arguments
  const sig = ctx.func.ingest(f[inner], strs, generics);
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
type ToSymbolic<T> = T extends Nulls
  ? Null
  : T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : T extends Tans
  ? Tan
  : T extends Nats
  ? Nat
  : T extends Vecs<any, infer V>
  ? Vec<ToSymbolic<V>>
  : { [K in keyof T]: ToSymbolic<T[K]> };

/**
 * Map from a type of a type to the type of the abstract values it represents.
 *
 * The result should be `Value`, so this should be used in any situation where
 * the abstract value is provided by the user (e.g. the returned value in the
 * body of `fn` or `vec`, or the inputs to a call or `select).
 */
type ToValue<T> = T extends Nulls
  ? Null
  : T extends Bools
  ? Bool
  : T extends Reals
  ? Real
  : T extends Tans
  ? Tan
  : T extends Nats
  ? Nat
  : T extends Vecs<any, infer V>
  ? Vec<ToValue<V>> | ToValue<V>[]
  : { [K in keyof T]: ToValue<T[K]> };

/** Map from a parameter type array to a symbolic parameter value type array. */
type SymbolicParams<T> = {
  [K in keyof T]: ToSymbolic<T[K]>;
};

/** Map from a parameter type array to an abstract parameter value type array. */
type ValueParams<T> = {
  [K in keyof T]: ToValue<T[K]>;
};

/** Construct an abstract function by abstractly interpreting `f` once. */
export const fn = <const P extends readonly any[], const R>(
  params: P,
  ret: R,
  f: (...args: SymbolicParams<P>) => ToValue<R>,
): Fn & ((...args: ValueParams<P>) => ToSymbolic<R>) => {
  // TODO: support closures
  if (context !== undefined)
    throw Error("can't define a function while defining another function");
  let out: number | undefined = undefined; // function return variable ID
  let { strings: strs, stringIds } = initStrings();
  const builder = new wasm.FuncBuilder(0); // TODO: support generics
  const body = new wasm.Block();
  try {
    const ctx = {
      func: builder,
      block: body,
      variables: new Map(),
      strings: strs,
      stringIds,
    };
    context = ctx;
    const args = params.map((ty) => {
      const t = tyId(ctx, ty);
      // it's important that `map` runs eagerly an in order, because the
      // ordering of these calls to `param` must match the order of `params`
      return idVal(ctx, t, ctx.func.param(t));
    }) as SymbolicParams<P>;
    const ty = tyId(ctx, ret);
    const x = f(...args);
    out = valId(ctx, ty, x);
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
  const g: any = (...args: any): any =>
    // TODO: support generics
    call(g, new Uint32Array(), args);
  g[inner] = func;
  g[strings] = strs;
  return makeFn(g) as any;
};

/** Construct an opaque function whose implementation runs `f`. */
export const opaque = <const P extends readonly Reals[], const R extends Reals>(
  params: P,
  ret: R,
  f: (...args: JsArgs<SymbolicParams<P>>) => ToJs<ToValue<R>>,
): Fn & ((...args: ValueParams<P>) => ToSymbolic<R>) => {
  // TODO: support more complicated signatures for opaque functions
  const func = new wasm.Func(params.length, f);
  const g: any = (...args: any): any =>
    // TODO: support generics
    call(g, new Uint32Array(), args);
  g[inner] = func;
  g[strings] = initStrings().strings; // TODO: allow structs in opaque functions
  return makeFn(g) as any;
};

/** A concrete value. */
type Js = null | boolean | number | Js[] | { [K: string]: Js };

/** Translate from the interpreteer's raw format to a concrete value. */
const pack = (f: Fn, t: number, x: unknown): RawVal => {
  const func = f[inner];
  if (typeof x === "boolean") return { Bool: x };
  else if (typeof x === "number")
    return func.isFin(t) ? { Fin: x } : { F64: x };
  else if (typeof x === "object") {
    if (x === null) return "Unit";
    else if (Array.isArray(x))
      return { Array: x.map((y) => pack(f, func.elem(t), y)) };
    else {
      const keys = func.keys(t);
      const mems = func.mems(t);
      const vals: RawVal[] = [];
      for (let i = 0; i < keys.length; ++i) {
        vals.push(pack(f, mems[i], (x as any)[f[strings][keys[i]]]));
      }
      return { Tuple: vals };
    }
  } else throw Error(`invalid value: ${x}`);
};

/** Translate a concrete value from the interpreter's raw format. */
const unpack = (f: Fn, t: number, x: RawVal): Js => {
  const func = f[inner];
  if (x === "Unit") return null;
  if ("Bool" in x) return x.Bool;
  if ("F64" in x) return x.F64;
  if ("Fin" in x) return x.Fin;
  if ("Ref" in x) throw Error("Ref not supported");
  if ("Array" in x)
    return x.Array.map((y: RawVal) => unpack(f, func.elem(t), y));
  else {
    const keys = func.keys(t);
    const mems = func.mems(t);
    return Object.fromEntries(
      x.Tuple.map((y: RawVal, i: number) => [
        f[strings][keys[i]],
        unpack(f, mems[i], y),
      ]),
    );
  }
};

/** Map from an abstract value type to its corresponding concrete value type. */
// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
type ToJs<T> = [T] extends [Null]
  ? null
  : [T] extends [Bool]
  ? boolean
  : [T] extends [Real]
  ? number
  : [T] extends [Nat]
  ? number
  : { [K in keyof T]: ToJs<T[K]> };

/** Map from an abstract value type array to a concrete argument type array. */
type JsArgs<T> = {
  [K in keyof T]: ToJs<T[K]>;
};

/** Concretize the abstract function `f` using the interpreter. */
export const interp =
  <const A extends readonly any[], const R>(
    f: Fn & ((...args: A) => R),
  ): ((...args: JsArgs<A>) => ToJs<R>) =>
  // TODO: support interpreting functions with generics
  (...args) => {
    const func = f[inner];
    const params = func.paramTypes();
    const vals = args.map((x, i) => pack(f, params[i], x));
    return unpack(f, func.retType(), func.interp(vals)) as ToJs<R>;
  };

// https://github.com/rose-lang/rose/issues/116

// TODO: use something more like an enum
interface Layout {
  size: number;
  align: number;
}

/** Round up `size` to the nearest multiple of `align`.  */
const aligned = ({ size, align }: Layout): number =>
  (size + align - 1) & ~(align - 1);

/** An aligned `ArrayBuffer` view, or `undefined` for zero-sized types. */
type View = undefined | Uint8Array | Uint16Array | Uint32Array | Float64Array;

const getView = (buffer: ArrayBuffer, layout: Layout, offset: number): View => {
  // this code assumes that the layout is uniquely determined by its `size`
  const { size } = layout;
  if (size === 0) return undefined;
  else if (size === 1) return new Uint8Array(buffer, offset);
  else if (size === 2) return new Uint16Array(buffer, offset);
  else if (size === 4) return new Uint32Array(buffer, offset);
  else if (size === 8) return new Float64Array(buffer, offset);
  else throw Error("unknown layout");
};

/** Memory representation for a type. */
interface Meta {
  /** Layout of an individual value of this type in memory. */
  layout: Layout;

  /**
   * Return the Wasm representation of the JS value `x`.
   *
   * The given byte offset is only used for pointer types.
   */
  encode: (x: unknown, offset: number) => number;

  /** Total memory cost of an object of this type, including sub-allocations. */
  cost: number;

  /** Return a JS value represented by the Wasm value `x`. */
  decode: (x: number) => unknown;
}

/**
 * Return enough information to encode and decode Wasm values with type ID `t`.
 *
 * The given function `f` must have already been compiled to WebAssembly,
 * yielding the given `buffer` of sufficient size. The `metas` array should hold
 * encoding/decoding information for all types with IDs less than `t`, or
 * `undefined` for reference types and non-struct tuple types since those cannot
 * appear in user-facing function signatures.
 */
const getMeta = (
  f: Fn,
  buffer: ArrayBuffer,
  metas: (Meta | undefined)[],
  t: number,
): Meta | undefined => {
  const func = f[inner];
  if (func.isUnit(t)) {
    return {
      layout: { size: 0, align: 1 },
      encode: () => 0,
      cost: 0,
      decode: () => null,
    };
  } else if (func.isBool(t)) {
    return {
      layout: { size: 1, align: 1 },
      encode: (x) => (x ? 1 : 0),
      cost: 0,
      decode: Boolean,
    };
  } else if (func.isF64(t)) {
    return {
      layout: { size: 8, align: 8 },
      encode: (x) => x as number,
      cost: 0,
      decode: (x) => x,
    };
  } else if (func.isFin(t)) {
    const size = func.size(t);
    const layout =
      size <= 1
        ? { size: 0, align: 1 }
        : size <= 256
        ? { size: 1, align: 1 }
        : size <= 65536
        ? { size: 2, align: 2 }
        : { size: 4, align: 4 };
    return { layout, encode: (x) => x as number, cost: 0, decode: (x) => x };
  } else if (func.isArray(t)) {
    const meta = metas[func.elem(t)];
    if (meta === undefined) return undefined;
    const { layout, encode, cost, decode } = meta;
    const n = func.size(func.index(t));
    const elem = aligned(layout);
    const total = aligned({ size: n * elem, align: 8 });
    const view = getView(buffer, layout, 0);
    return {
      layout: { size: 4, align: 4 },
      encode:
        view === undefined
          ? (x, offset) => offset
          : (x, offset) => {
              let child = offset + total;
              for (let i = 0; i < n; ++i) {
                view[offset / elem + i] = encode((x as unknown[])[i], child);
                child += cost;
              }
              return offset;
            },
      cost: total + n * cost,
      decode:
        view === undefined
          ? () => {
              const arr: unknown[] = [];
              // this code assumes that all values of all zero-sized types can
              // be represented by zero
              for (let i = 0; i < n; ++i) arr.push(decode(0));
              return arr;
            }
          : (x) => {
              const arr: unknown[] = [];
              for (let i = 0; i < n; ++i) arr.push(decode(view[x / elem + i]));
              return arr;
            },
    };
  } else if (func.isStruct(t)) {
    const keys = func.keys(t);
    const members = func.mems(t);
    const n = keys.length;
    const mems: { key: string; meta: Meta; view?: View; child?: number }[] = [];
    for (let i = 0; i < n; ++i) {
      const meta = metas[members[i]];
      if (meta === undefined) return undefined;
      mems.push({ key: f[strings][keys[i]], meta });
    }
    mems.sort((a, b) => a.meta.layout.align - b.meta.layout.align);
    let cost = 0;
    let offset = 0;
    for (const mem of mems) {
      const { meta } = mem;
      mem.child = cost;
      cost += meta.cost;
      const { layout } = meta;
      const { size, align } = layout;
      offset = aligned({ size: offset, align });
      mem.view = getView(buffer, layout, offset);
      offset += size;
    }
    const total = aligned({ size: offset, align: 8 });
    return {
      layout: { size: 4, align: 4 },
      encode: (x, offset) => {
        for (const { key, meta, view, child } of mems) {
          // instead of mutating each element of `mems` above to add more data
          // and then still having an `if` statement in here, it would be nicer
          // to just map over `mems` above to produce an array of closures that
          // can be called directly, with the condition on `view === undefined`
          // being handled once rather than in every call to `encode` here
          if (view !== undefined) {
            view[offset / aligned(meta.layout)] = meta.encode(
              (x as any)[key],
              offset + total + child!,
            );
          }
        }
        return offset;
      },
      cost: total + cost,
      decode: (x) => {
        const obj: any = {};
        for (const { key, meta, view } of mems) {
          if (view === undefined) {
            // this code assumes that all values of all zero-sized types can be
            // represented by zero
            obj[key] = meta.decode(0);
          } else {
            obj[key] = meta.decode(view[x / aligned(meta.layout)]);
          }
        }
        return obj;
      },
    };
  } else return undefined;
};

/** Concretize the abstract function `f` using the compiler. */
export const compile = async <const A extends readonly any[], const R>(
  f: Fn & ((...args: A) => R),
): Promise<(...args: JsArgs<A>) => ToJs<R>> => {
  const func = f[inner];
  const res = func.compile();
  const bytes = res.bytes()!;
  const imports = res.imports()!;
  res.free();
  const instance = await WebAssembly.instantiate(
    await WebAssembly.compile(bytes),
    { "": Object.fromEntries(imports.map((g, i) => [i.toString(), g])) },
  );
  const { f: g, m } = instance.exports;
  const metas: (Meta | undefined)[] = [];
  const n = func.numTypes();
  for (let t = 0; t < n; ++t)
    metas.push(getMeta(f, (m as WebAssembly.Memory).buffer, metas, t));
  let total = 0;
  const params = Array.from(func.paramTypes()).map((t) => {
    const { encode, cost } = metas[t]!;
    const offset = total;
    total += cost;
    return { encode, offset };
  });
  const { decode } = metas[func.retType()]!;
  return (...args): any => {
    const vals = params.map(({ encode, offset }, i) => encode(args[i], offset));
    return decode((g as any)(...vals, total));
  };
};

// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
type ToJvp<T> = [T] extends [Null]
  ? Null
  : [T] extends [Bool]
  ? Bool
  : [T] extends [Real]
  ? { re: Real; du: Real }
  : [T] extends [Nat]
  ? Nat
  : { [K in keyof T]: ToJvp<T[K]> };

type JvpArgs<T> = {
  [K in keyof T]: ToJvp<T[K]>;
};

/** Construct a function that computes the Jacobian-vector product of `f`. */
export const jvp = <const A extends readonly any[], const R>(
  f: Fn & ((...args: A) => R),
): Fn & ((...args: JvpArgs<A>) => ToJvp<R>) => {
  const strs = [...f[strings]];
  const func = f[inner].jvp(reId, duId);
  const g: any = (...args: any): any =>
    // TODO: support generics
    call(g, new Uint32Array(), args);
  g[inner] = func;
  g[strings] = strs;
  return makeFn(g) as any;
};

/** Construct a closure that computes the Jacobian-vector product of `f`. */
export const vjp = <const A, const R>(
  f: Fn & ((arg: A) => R),
): ((arg: A) => { ret: R; grad: (cot: R) => A }) => {
  const g = jvp(f);
  const tp = g[inner].transpose();
  const fwdFunc = tp.fwd()!;
  const bwdFunc = tp.bwd()!;
  tp.free();
  const fwd = makeFn({ [inner]: fwdFunc, [strings]: [...f[strings]] });
  const bwd = makeFn({ [inner]: bwdFunc, [strings]: [...f[strings]] });
  return (arg: A) => {
    const ctx = getCtx();
    const strs = intern(ctx, fwd[strings]);
    const generics = new Uint32Array(); // TODO: support generics
    const [tArg, tBundle] = ctx.func.ingest(fwd[inner], strs, generics);
    const [tRet, tInter] = ctx.func.members(tBundle);
    const argId = valId(ctx, tArg, arg);
    const bundleId = ctx.block.call(
      ctx.func,
      fwd[inner],
      generics,
      tBundle,
      new Uint32Array([argId]),
    );
    const primalId = ctx.block.member(ctx.func, tRet, bundleId, 0);
    const interId = ctx.block.member(ctx.func, tInter, bundleId, 1);
    const grad = (cot: R) => {
      if (getCtx() !== ctx) throw Error("VJP closure escaped its context");
      const cotId = valId(ctx, tRet, cot);
      const tRef = ctx.func.tyRef(tArg);
      const accId = ctx.block.accum(ctx.func, tRef, argId);
      ctx.block.call(
        ctx.func,
        bwd[inner],
        new Uint32Array([]), // TODO: support generics
        ctx.func.tyUnit(),
        new Uint32Array([accId, cotId, interId]),
      );
      return idVal(ctx, tArg, ctx.block.resolve(ctx.func, tArg, accId)) as A;
    };
    return { ret: idVal(ctx, tRet, primalId) as R, grad };
  };
};

/** Return the variable ID for the abstract boolean `x`. */
const boolId = (ctx: Context, x: Bool): number =>
  valId(ctx, ctx.func.tyBool(), x);

/** Return the negation of the abstract boolean `p`. */
export const not = (p: Bool): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.not(ctx.func, boolId(ctx, p)));
};

/** Return the conjunction of the abstract booleans `p` and `q`. */
export const and = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.and(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the disjunction of the abstract booleans `p` and `q`. */
export const or = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.or(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the biconditional of the abstract booleans `p` and `q`. */
export const iff = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.iff(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return the exclusive disjunction of the abstract booleans `p` and `q`. */
export const xor = (p: Bool, q: Bool): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.xor(ctx.func, boolId(ctx, p), boolId(ctx, q)));
};

/** Return an abstract value selecting between `then` and `els` via `cond`. */
export const select = <const T>(
  cond: Bool,
  ty: T,
  then: ToValue<T>,
  els: ToValue<T>,
): ToSymbolic<T> => {
  const ctx = getCtx();
  const t = tyId(ctx, ty);
  const p = boolId(ctx, cond);
  const a = valId(ctx, t, then);
  const b = valId(ctx, t, els);
  return idVal(ctx, t, ctx.block.select(ctx.func, p, t, a, b)) as ToSymbolic<T>;
};

/** Return the variable ID for the abstract floating point number `x`. */
const realId = (ctx: Context, x: Real): number =>
  valId(ctx, ctx.func.tyF64(), x);

/** Return the negative of the abstract number `x`. */
export const neg = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.neg(ctx.func, realId(ctx, x)));
};

/** Return the absolute value of the abstract number `x`. */
export const abs = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.abs(ctx.func, realId(ctx, x)));
};

/** Return the signum of the abstract number `x`. */
export const sign = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.sign(ctx.func, realId(ctx, x)));
};

/** Return the ceiling of the abstract number `x`. */
export const ceil = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.ceil(ctx.func, realId(ctx, x)));
};

/** Return the floor of the abstract number `x`. */
export const floor = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.floor(ctx.func, realId(ctx, x)));
};

/** Return the truncation of the abstract number `x`. */
export const trunc = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.trunc(ctx.func, realId(ctx, x)));
};

/** Return the square root of the abstract number `x`. */
export const sqrt = (x: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.sqrt(ctx.func, realId(ctx, x)));
};

/** Return the abstract number `x` plus the abstract number `y`. */
export const add = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.add(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` minus the abstract number `y`. */
export const sub = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.sub(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` times the abstract number `y`. */
export const mul = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.mul(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return the abstract number `x` divided by the abstract number `y`. */
export const div = (x: Real, y: Real): Real => {
  const ctx = getCtx();
  return newVar(ctx.block.div(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is not equal to `y`. */
export const neq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.neq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is less than `y`. */
export const lt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.lt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is less than or equal to `y`. */
export const leq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.leq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is equal to `y`. */
export const eq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.eq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is greater than `y`. */
export const gt = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.gt(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract boolean for if `x` is greater than or equal to `y`. */
export const geq = (x: Real, y: Real): Bool => {
  const ctx = getCtx();
  return newVar(ctx.block.geq(ctx.func, realId(ctx, x), realId(ctx, y)));
};

/** Return an abstract vector by computing each element via `f`. */
export const vec = <const I, const T>(
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
    out = valId(ctx, e, f(idVal(ctx, i, arg) as ToSymbolic<I>));
  } finally {
    if (out === undefined) body.free();
    ctx.block = block;
  }
  const id = block.vec(ctx.func, t, arg, body, out);
  return idVal(ctx, t, id) as Vec<ToSymbolic<T>>;
};

/** Return the variable ID for the abstract floating point tangent `x`. */
const tanId = (ctx: Context, x: Tan): number => valId(ctx, ctx.func.tyT64(), x);

/** Return the negative of the abstract tangent `x`. */
export const negLin = (x: Tan): Tan => {
  const ctx = getCtx();
  return newVar(ctx.block.neg(ctx.func, tanId(ctx, x)));
};

/** Return the abstract tangent `x` plus the abstract tangent `y`. */
export const addLin = (x: Tan, y: Tan): Tan => {
  const ctx = getCtx();
  return newVar(ctx.block.add(ctx.func, tanId(ctx, x), tanId(ctx, y)));
};

/** Return the abstract tangent `x` minus the abstract tangent `y`. */
export const subLin = (x: Tan, y: Tan): Tan => {
  const ctx = getCtx();
  return newVar(ctx.block.sub(ctx.func, tanId(ctx, x), tanId(ctx, y)));
};

/** Return the abstract tangent `x` times the abstract number `y`. */
export const mulLin = (x: Tan, y: Real): Tan => {
  const ctx = getCtx();
  return newVar(ctx.block.mul(ctx.func, tanId(ctx, x), realId(ctx, y)));
};

/** Return the abstract tangent `x` divided by the abstract number `y`. */
export const divLin = (x: Tan, y: Real): Tan => {
  const ctx = getCtx();
  return newVar(ctx.block.div(ctx.func, tanId(ctx, x), realId(ctx, y)));
};
