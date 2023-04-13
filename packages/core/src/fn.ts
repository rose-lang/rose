import { Int } from "./int";

declare const opaqueBool: unique symbol;
export type Bools = string & { tag: typeof opaqueBool };

declare const opaqueInt: unique symbol;
export type Ints = string & { tag: typeof opaqueInt };

declare const opaqueReal: unique symbol;
export type Reals = string & { tag: typeof opaqueReal };

export interface Vecs<T> {
  t: T;
  n: Int;
}

export type Type = Bools | Ints | Reals | Vecs<Type>;

export interface Fn<T> {
  params: [symbol, Type][];
  body: T;
}
