import { Bool } from "./bool.js";
import { Fn } from "./fn.js";
import { Int } from "./int.js";
import { Vec } from "./vec.js";

interface Call<T> {
  tag: "call";
  f: Fn<T>;
  args: unknown[];
}

interface If<T> {
  tag: "if";
  cond: Bool;
  then: T;
  els: T;
}

interface Index<T> {
  tag: "index";
  vec: Vec<T>;
  index: Int;
}

export interface Val<T> {
  tag: "val";
  val: symbol | Call<T> | Index<T> | If<T>;
}

export const cond = <T>(cond: Bool, then: T, els: T): Val<T> => ({
  tag: "val",
  val: { tag: "if", cond, then, els },
});

export const get = <T>(v: Vec<T>, i: Int): Val<T> => ({
  tag: "val",
  val: { tag: "index", vec: v, index: i },
});
