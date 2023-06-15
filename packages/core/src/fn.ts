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

class Body {
  locals: Map<unknown, number>;
  instrs: ffi.Body;

  constructor(params: symbol[]) {
    this.instrs = new ffi.Body();
    try {
      for (let i = params.length - 1; i >= 0; --i) this.instrs.set(i);
      this.locals = new Map(params.map((param, id) => [param, id]));
    } catch (e) {
      this.instrs.free();
      throw e;
    }
  }

  local(x: unknown): number {
    const id = this.locals.size;
    this.locals.set(x, id);
    this.instrs.set(id);
    this.instrs.get(id);
    return id;
  }

  val<T>(x: Val<T>["val"]): number {
    const id = this.locals.get(x);
    if (id !== undefined) {
      this.instrs.get(id);
      return id;
    }
    if (typeof x === "symbol") throw Error("all symbols should be mapped");
    switch (x.tag) {
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

  real(x: Real): number {
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
        return this.val(x.val);
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

export const fn = <const T extends readonly Reals[]>(
  types: T,
  f: (...args: Resolve<T>) => Real
): Fn & ((...args: Resolve<T>) => Real) => {
  const g = (...args: Resolve<T>): Real => ({
    tag: "val",
    val: { tag: "call", f: g, args: args as unknown[] },
  });
  g.params = types as unknown as Type[];
  const params = types.map(() => Symbol());
  const body = new Body(params);
  try {
    body.real(f(...(params.map((val) => ({ tag: "val", val })) as Resolve<T>)));
  } catch (e) {
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
