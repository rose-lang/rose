import { Int } from "./int.js";
import { Real } from "./real.js";

export const neg: {
  (a: Int): Int;
  (a: Real): Real;
} = (a: any) => ({ tag: "unary", op: "-", arg: a });

export const abs: {
  (a: Int): Int;
  (a: Real): Real;
} = (a: any) => ({ tag: "unary", op: "abs", arg: a });

export const add: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any) => ({ tag: "binary", op: "+", left: a, right: b });

export const sub: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any) => ({ tag: "binary", op: "-", left: a, right: b });

export const mul: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any) => ({ tag: "binary", op: "*", left: a, right: b });

export const div: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any) => ({ tag: "binary", op: "/", left: a, right: b });
