import { Val } from "./val.js";

export interface Unary {
  tag: "unary";
  op: "-" | "abs";
  arg: Int;
}

export interface Binary {
  tag: "binary";
  op: "*" | "+" | "-" | "/" | "mod";
  left: Int;
  right: Int;
}

export interface Index {
  tag: "index";
  bound: Int;
  name: symbol;
}

export type Int = number | Val<Int> | Unary | Binary | Index;

export const mod = (a: Int, b: Int): Int => ({
  tag: "binary",
  op: "mod",
  left: a,
  right: b,
});
