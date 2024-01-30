const unaries = {
  abs: Math.abs,
  acos: Math.acos,
  acosh: Math.acosh,
  asin: Math.asin,
  asinh: Math.asinh,
  atan: Math.atan,
  atanh: Math.atanh,
  cbrt: Math.cbrt,
  ceil: Math.ceil,
  cos: Math.cos,
  cosh: Math.cosh,
  exp: Math.exp,
  expm1: Math.expm1,
  floor: Math.floor,
  log: Math.log,
  log10: Math.log10,
  log1p: Math.log1p,
  log2: Math.log2,
  sign: Math.sign,
  sin: Math.sin,
  sinh: Math.sinh,
  sqrt: Math.sqrt,
  tan: Math.tan,
  tanh: Math.tanh,
  trunc: Math.trunc,
};

const unary = (name: string): ((x: number) => number) => {
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
  | { kind: "unary"; f: (x: number) => number; arg: Expr }
  | {
      kind: "binary";
      f: (x: number, y: number) => number;
      lhs: Expr;
      rhs: Expr;
    };

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
      return { kind: "unary", f: (a) => -a, arg: this.parseFactor() };
    }
    const x = this.parseAtom();
    if (this.peek().kind === "^") {
      this.pop();
      return { kind: "binary", f: Math.pow, lhs: x, rhs: this.parseFactor() };
    }
    return x;
  }

  parseTerm(): Expr {
    let x = this.parseFactor();
    let tok = this.peek();
    while (tok.kind === "*" || tok.kind === "/") {
      this.pop();
      const f = {
        "*": (a: number, b: number) => a * b,
        "/": (a: number, b: number) => a / b,
      }[tok.kind];
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
      const f = {
        "+": (a: number, b: number) => a + b,
        "-": (a: number, b: number) => a - b,
      }[tok.kind];
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
