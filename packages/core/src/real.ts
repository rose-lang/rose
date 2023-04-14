import { Val } from "./val";
import { Vec } from "./vec";

interface Unary {
  tag: "unary";
  op: "-" | "abs" | "sqrt";
  arg: Real;
}

interface Binary {
  tag: "binary";
  op: "*" | "+" | "-" | "/";
  left: Real;
  right: Real;
}

interface Fold {
  tag: "fold";
  op: "*" | "+" | "max" | "min";
  vec: Vec<Real>;
}

export type Real = number | Val<Real> | Unary | Binary | Fold;

export const sqrt = (a: Real): Real => ({ tag: "unary", op: "sqrt", arg: a });

export const sum = (v: Vec<Real>): Real => ({ tag: "fold", op: "+", vec: v });

export const prod = (v: Vec<Real>): Real => ({ tag: "fold", op: "*", vec: v });

export const max = (v: Vec<Real>): Real => ({ tag: "fold", op: "max", vec: v });

export const min = (v: Vec<Real>): Real => ({ tag: "fold", op: "min", vec: v });
