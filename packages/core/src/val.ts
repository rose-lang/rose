import { Bool } from "./bool";
import { Fn } from "./fn";
import { Int } from "./int";
import { Vec } from "./vec";

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

export const get = <T>(v: Vec<T>, i: Int): Val<T> => ({
  tag: "val",
  val: { tag: "index", vec: v, index: i },
});
