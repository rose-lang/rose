import { Real } from "./real";
import { Val } from "./val";

interface Not {
  tag: "not";
  arg: Bool;
}

interface Logic {
  tag: "logic";
  op: "!=" | "==" | "and" | "or";
  left: Bool;
  right: Bool;
}

interface Comp {
  tag: "comp";
  op: "!=" | "<" | "<=" | "==" | ">" | ">=";
  left: Real;
  right: Real;
}

export type Bool = boolean | Val<Bool> | Not | Logic | Comp;

export const not = (p: Bool): Bool => ({ tag: "not", arg: p });

export const and = (p: Bool, q: Bool): Bool => ({
  tag: "logic",
  op: "and",
  left: p,
  right: q,
});

export const or = (p: Bool, q: Bool): Bool => ({
  tag: "logic",
  op: "or",
  left: p,
  right: q,
});

export const iff = (p: Bool, q: Bool): Bool => ({
  tag: "logic",
  op: "==",
  left: p,
  right: q,
});

export const xor = (p: Bool, q: Bool): Bool => ({
  tag: "logic",
  op: "!=",
  left: p,
  right: q,
});

export const eq = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: "==",
  left: a,
  right: b,
});

export const neq = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: "!=",
  left: a,
  right: b,
});

export const lt = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: "<",
  left: a,
  right: b,
});

export const leq = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: "<=",
  left: a,
  right: b,
});

export const gt = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: ">",
  left: a,
  right: b,
});

export const geq = (a: Real, b: Real): Bool => ({
  tag: "comp",
  op: ">=",
  left: a,
  right: b,
});
