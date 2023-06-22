import { Bool } from "./bool.js";
import { Int } from "./int.js";
import * as real from "./real.js";
import { Real } from "./real.js";
import { Vec } from "./vec.js";

// TODO: actually support `Int` rather than just assuming `Real`

export const neg: {
  (a: Int): Int;
  (a: Real): Real;
} = (a: any): any => real.neg(a);

export const abs: {
  (a: Int): Int;
  (a: Real): Real;
} = (a: any): any => real.abs(a);

export const add: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any): any => real.add(a, b);

export const sub: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any): any => real.sub(a, b);

export const mul: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any): any => real.mul(a, b);

export const div: {
  (a: Int, b: Int): Int;
  (a: Real, b: Real): Real;
} = (a: any, b: any): any => real.div(a, b);

export const sum: {
  (v: Vec<Int>): Int;
  (v: Vec<Real>): Real;
} = (v: any): any => real.sum(v);

export const prod: {
  (v: Vec<Int>): Int;
  (v: Vec<Real>): Real;
} = (v: any): any => real.prod(v);

export const max: {
  (v: Vec<Int>): Int;
  (v: Vec<Real>): Real;
} = (v: any): any => real.max(v);

export const min: {
  (v: Vec<Int>): Int;
  (v: Vec<Real>): Real;
} = (v: any): any => real.min(v);

export const neq: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.neq(a, b);

export const lt: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.lt(a, b);

export const leq: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.leq(a, b);

export const eq: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.eq(a, b);

export const gt: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.gt(a, b);

export const geq: {
  (a: Int, b: Int): Bool;
  (a: Real, b: Real): Bool;
} = (a: any, b: any): any => real.geq(a, b);
