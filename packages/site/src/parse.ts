import {
  Real,
  abs,
  add,
  ceil,
  div,
  floor,
  mul,
  neg,
  sign,
  sqrt,
  sub,
  trunc,
} from "rose";
import {
  acos,
  acosh,
  asin,
  asinh,
  atan,
  atanh,
  cbrt,
  cos,
  cosh,
  exp,
  expm1,
  log,
  log10,
  log1p,
  log2,
  pow,
  sin,
  sinh,
  tan,
  tanh,
} from "./math.js";

const unaries = {
  abs,
  acos,
  acosh,
  asin,
  asinh,
  atan,
  atanh,
  cbrt,
  ceil,
  cos,
  cosh,
  exp,
  expm1,
  floor,
  log,
  log10,
  log1p,
  log2,
  sign,
  sin,
  sinh,
  sqrt,
  tan,
  tanh,
  trunc,
};

const unary = (name: string): ((x: Real) => Real) => {
  if (name in unaries) return unaries[name as keyof typeof unaries];
  throw Error(`unknown unary function: ${name}`);
};

type Token =
  | { kind: "end" }
  | { kind: "const"; val: number }
  | { kind: "name"; val: string }
  | { kind: "+" }
  | { kind: "-" }
  | { kind: "*" }
  | { kind: "/" }
  | { kind: "^" }
  | { kind: "(" }
  | { kind: ")" };

class Lexer {
  s: string;

  constructor(s: string) {
    this.s = s;
  }

  token(): Token {
    this.s = this.s.trimStart();
    if (this.s.length === 0) return { kind: "end" };
    {
      const m = this.s.match(/^[0-9]+(?:\.[0-9]+)?\b/);
      if (m) {
        this.s = this.s.slice(m[0].length);
        return { kind: "const", val: Number(m[0]) };
      }
    }
    {
      const m = this.s.match(/^[A-Z_a-z][0-9A-Z_a-z]*/);
      if (m) {
        this.s = this.s.slice(m[0].length);
        return { kind: "name", val: m[0] };
      }
    }
    {
      const m = this.s.match(/^[+\-*/^()]/);
      if (m) {
        this.s = this.s.slice(m[0].length);
        return { kind: m[0] as any };
      }
    }
    throw Error(`can't tokenize: ${this.s}`);
  }
}

function* lex(s: string) {
  const lexer = new Lexer(s);
  while (true) {
    const tok = lexer.token();
    yield tok;
    if (tok.kind === "end") break;
  }
}

export type Expr =
  | { kind: "const"; val: number }
  | { kind: "var"; idx: number }
  | { kind: "unary"; f: (x: Real) => Real; arg: Expr }
  | { kind: "binary"; f: (x: Real, y: Real) => Real; lhs: Expr; rhs: Expr };

class Parser {
  tokens: Token[];

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  peek(): Token {
    return this.tokens[this.tokens.length - 1];
  }

  pop(): Token {
    const tok = this.tokens.pop();
    if (!tok) throw Error("unexpected end of input");
    return tok;
  }

  parseAtom(): Expr {
    const tok = this.pop();
    switch (tok.kind) {
      case "const":
        return { kind: "const", val: tok.val };
      case "name": {
        if (tok.val === "x") return { kind: "var", idx: 0 };
        if (tok.val === "y") return { kind: "var", idx: 1 };
        const f = unary(tok.val);
        const arg = this.parseAtom();
        return { kind: "unary", f, arg };
      }
      case "(": {
        const x = this.parseExpr();
        const tok = this.pop();
        if (tok.kind !== ")") throw Error("expected )");
        return x;
      }
      default:
        throw Error(`unexpected token: ${tok.kind}`);
    }
  }

  parseFactor(): Expr {
    if (this.peek().kind === "-") {
      this.pop();
      return { kind: "unary", f: neg, arg: this.parseFactor() };
    }
    const x = this.parseAtom();
    if (this.peek().kind === "^") {
      this.pop();
      return { kind: "binary", f: pow, lhs: x, rhs: this.parseFactor() };
    }
    return x;
  }

  parseTerm(): Expr {
    let x = this.parseFactor();
    let tok = this.peek();
    while (tok.kind === "*" || tok.kind === "/") {
      this.pop();
      const f = { "*": mul, "/": div }[tok.kind];
      x = { kind: "binary", f, lhs: x, rhs: this.parseFactor() };
      tok = this.peek();
    }
    return x;
  }

  parseExpr(): Expr {
    let x = this.parseTerm();
    let tok = this.peek();
    while (tok.kind === "+" || tok.kind === "-") {
      this.pop();
      const f = { "+": add, "-": sub }[tok.kind];
      x = { kind: "binary", f, lhs: x, rhs: this.parseTerm() };
      tok = this.peek();
    }
    return x;
  }
}

export const parse = (s: string): Expr => {
  const parser = new Parser([...lex(s)].reverse());
  const expr = parser.parseExpr();
  if (parser.pop().kind !== "end") throw Error("expected end of input");
  if (parser.tokens.length !== 0) throw Error("unexpected tokens after end");
  return expr;
};
