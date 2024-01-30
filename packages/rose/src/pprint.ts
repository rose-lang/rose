import { Token } from "moo";
import {
  Bind,
  BindKind,
  Def,
  Expr,
  ExprKind,
  Module,
  Param,
  TokenId,
  Type,
  TypeKind,
} from "./parse.js";

export class Printer {
  tokens: Token[];
  strings: string[];

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.strings = [];
  }

  flush(): string {
    const s = this.strings.join("");
    this.strings = [s];
    return s;
  }

  push(s: string) {
    this.strings.push(s);
  }

  get(n: TokenId): string {
    return this.tokens[n].text;
  }

  bind(bind: Bind) {
    switch (bind.kind) {
      case BindKind.Unit:
        this.push("()");
        break;
      case BindKind.Name:
        this.push(this.get(bind.name));
        break;
      case BindKind.Pair:
        this.param(bind.left);
        this.push(", ");
        this.param(bind.right);
        break;
    }
  }

  type(type: Type) {
    switch (type.kind) {
      case TypeKind.Unit:
        this.push("()");
        break;
      case TypeKind.Name:
        this.push(this.get(type.name));
        break;
      case TypeKind.Pair:
        this.type(type.left);
        this.push(", ");
        this.type(type.right);
        break;
    }
  }

  param({ bind, type }: Param) {
    this.bind(bind);
    if (type !== undefined) {
      this.push(": ");
      this.type(type);
    }
  }

  expr(expr: Expr) {
    switch (expr.kind) {
      case ExprKind.Name:
        this.push(this.get(expr.name));
        break;
      case ExprKind.Unit:
        this.push("()");
        break;
      case ExprKind.Add:
        this.expr(expr.left);
        this.push(" + ");
        this.expr(expr.right);
        break;
    }
  }

  def({ pub, name, param, type, body }: Def) {
    if (pub !== undefined) this.push("pub ");
    this.push("def ");
    this.push(this.get(name));
    this.push("(");
    if (!(param.bind.kind === BindKind.Unit && param.type === undefined))
      this.param(param);
    this.push(")");
    if (type !== undefined) {
      this.push(": ");
      this.type(type);
    }
    this.push(" = ");
    this.expr(body);
    this.push("\n");
  }

  module(mod: Module) {
    for (const def of mod.def) this.def(def);
  }
}
