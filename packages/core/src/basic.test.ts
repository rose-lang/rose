import { expect, test } from "vitest";

test("test that 2 + 2 = 4", () => {
  expect(interpret(add(2, 2))).toBe(4);
});

test("test computation graph", () => {
  expect(interpret(add(2, sub(mul(3, 2), div(2, 1))))).toBe(6);
});

interface Node {
  op: "*" | "+" | "-" | "/";
  left: Num;
  right: Num;
}

export type Num = number | Node;

export function interpret(node: Num): number {
  if (typeof node === "number") {
    return node;
  }

  const left = interpret(node.left);
  const right = interpret(node.right);
  if (node.op == "+") {
    return left + right;
  } else if (node.op == "-") {
    return left - right;
  } else if (node.op == "*") {
    return left * right;
  } else if (node.op == "/") {
    return left / right;
  }

  throw new Error(`Unsupported operation: ${node.op}`);
}

export function add(a: Num, b: Num): Node {
  const res: Node = {
    op: "+",
    left: a,
    right: b,
  };
  return res;
}

export function sub(a: Num, b: Num): Node {
  const res: Node = {
    op: "-",
    left: a,
    right: b,
  };
  return res;
}

export function mul(a: Num, b: Num): Node {
  const res: Node = {
    op: "*",
    left: a,
    right: b,
  };
  return res;
}

export function div(a: Num, b: Num): Node {
  const res: Node = {
    op: "/",
    left: a,
    right: b,
  };
  return res;
}
