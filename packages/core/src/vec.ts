import { Int } from "./int.js";
import { Val } from "./val.js";

export interface Cons<T> {
  tag: "cons";
  dim: Int;
  index: symbol;
  elem: T;
}

export type Vec<T> = T[] | Val<Vec<T>> | Cons<T>;

export const vec = <T>(n: Int, f: (i: Int) => T): Vec<T> => {
  const name = Symbol();
  const i: Int = { tag: "index", bound: n, name };
  return { tag: "cons", dim: n, index: name, elem: f(i) };
};
