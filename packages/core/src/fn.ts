import * as ffi from "./ffi.js";
import { Int } from "./int.js";
import * as real from "./real.js";
import { Real } from "./real.js";
import { Val } from "./val.js";

export type Bools = { tag: "Bool" };
export type Ints = { tag: "Int" };
export type Reals = { tag: "Real" };

export interface Vecs<T> {
  tag: "Vec";
  t: T;
  n: Int;
}

export type Type = Bools | Ints | Reals | Vecs<Type>;

export interface Fn {
  params: Type[];
  f: ffi.Fn;
}

type Resolve<T extends readonly Reals[]> = {
  [K in keyof T]: Real;
};

/**
 * Builder class to generate the body of a function by recursively walking its
 * computation graph. Uses a very simple strategy: every node gets its own local
 * in case it turns out to be referenced multiple times. This obviously creates
 * a bunch of unnecessary locals; if we want then we can do a pass to eliminate
 * those, but it's nicer to do that in Rust and just keep this JavaScript code
 * simple.
 *
 * This strategy means that when we first see a given node, we need to check for
 * an existing local for that node. The methods that see nodes for the first
 * time are called "entrypoints," in contrast to other methods that just get
 * called once the broader type has been narrowed down. We use these
 * type-constrained non-entrypoints because TypeScript (and ESLint) work nicely
 * with `switch` statements at the function top-level, but not so nicely with
 * _nested_ `switch` statements.
 */
class Body {
  /**
   * Map from nodes in the computation graph to local indices. Locals are
   * contiguous, so the next unused one is `this.locals.size`.
   */
  locals: Map<unknown, number>;
  instrs: ffi.Body;

  /**
   * Start building a function body which begins by popping all `params` into
   * locals.
   */
  constructor(params: Val<never>[]) {
    this.instrs = new ffi.Body();
    try {
      // reverse because stack is LIFO
      for (let i = params.length - 1; i >= 0; --i) this.instrs.set(i);
      this.locals = new Map(params.map((param, id) => [param, id]));
    } catch (e) {
      // unlikely that the code in the `try` block above would throw, but we
      // want to be defensive against memory leaks
      this.instrs.free();
      throw e;
    }
  }

  /**
   * Make a new local for the computation graph node `x`, and emit code to pop
   * the top-of-stack into that local and then push it back onto the stack.
   */
  local(x: unknown): number {
    const id = this.locals.size;
    this.locals.set(x, id);
    this.instrs.set(id);
    this.instrs.get(id);
    return id;
  }

  // non-entrypoints

  val<T>(x: Val<T>): number {
    const { val } = x;
    if (typeof val === "symbol") throw Error("all symbols should be mapped");
    switch (val.tag) {
      case "call": {
        throw Error("TODO");
      }
      case "index": {
        throw Error("TODO");
      }
      case "if": {
        throw Error("TODO");
      }
    }
  }

  realBinary(x: real.Binary): number {
    this.real(x.left);
    this.real(x.right);
    switch (x.op) {
      case "+": {
        this.instrs.addReal();
        return this.local(x);
      }
      case "-": {
        this.instrs.subReal();
        return this.local(x);
      }
      case "*": {
        this.instrs.mulReal();
        return this.local(x);
      }
      case "/": {
        this.instrs.divReal();
        return this.local(x);
      }
    }
  }

  // entrypoints

  real(x: Real): number {
    // check for existing local
    const id = this.locals.get(x);
    if (id !== undefined) {
      this.instrs.get(id);
      return id;
    }

    if (typeof x === "number") {
      this.instrs.real(x);
      return this.local(x);
    }
    switch (x.tag) {
      case "val": {
        return this.val(x);
      }
      case "unary": {
        throw Error("TODO");
      }
      case "binary": {
        return this.realBinary(x);
      }
      case "fold": {
        throw Error("TODO");
      }
    }
  }
}

const ffiType = (t: Type): ffi.Type => {
  switch (t.tag) {
    case "Bool":
    case "Int":
    case "Real": {
      return t.tag;
    }
    case "Vec": {
      throw Error("TODO");
    }
  }
};

/**
 * Constructs an abstract function with the given `types` for parameters.
 */
// TODO: allow args other than `Real`
export const fn = <const T extends readonly Reals[]>(
  types: T,
  f: (...args: Resolve<T>) => Real // TODO: allow return other than `Real`
): Fn & ((...args: Resolve<T>) => Real) => {
  //
  const g = (...args: Resolve<T>): Real => ({
    tag: "val",
    // bit of a circular reference: every `Call` constructed by `g` points back
    // to `g` itself
    val: { tag: "call", f: g, args: args as unknown[] },
  });
  g.params = types as unknown as Type[];
  const params: Val<never>[] = types.map(() => ({ tag: "val", val: Symbol() }));
  const body = new Body(params);
  try {
    // emit IR by recursively walking computation graph
    body.real(f(...(params as Resolve<T>)));
  } catch (e) {
    // `body` points into Wasm memory, so if we don't reach the `ffi.makeFunc`
    // call below then we need to be sure to `free` it
    body.instrs.free();
    throw e;
  }
  g.f = ffi.makeFunc(
    types.map(ffiType),
    Array(body.locals.size).fill("Real"),
    body.instrs
  );
  return g;
};
