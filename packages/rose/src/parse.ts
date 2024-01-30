import { Lexer, Token } from "moo";
import { unreachable, unwrap } from "./util.js";

/** Index of a token in the original token array. */
export type TokenId = number;

export interface Module {
  def: Def[];
}

export interface Def {
  pub: TokenId | undefined;
  name: TokenId;
  param: Param;
  type: Type | undefined;
  body: Expr;
}

export enum TypeKind {
  Unit,
  Name,
  Pair,
}

export type Type =
  | { kind: TypeKind.Unit }
  | { kind: TypeKind.Name; name: TokenId }
  | { kind: TypeKind.Pair; left: Type; right: Type };

export interface Param {
  bind: Bind;
  type: Type | undefined;
}

export enum BindKind {
  Unit,
  Name,
  Pair,
}

export type Bind =
  | { kind: BindKind.Unit }
  | { kind: BindKind.Name; name: TokenId }
  | { kind: BindKind.Pair; left: Param; right: Param };

export enum ExprKind {
  Name,
  Unit,
  Add,
}

export type Expr =
  | { kind: ExprKind.Name; name: TokenId }
  | { kind: ExprKind.Unit }
  | { kind: ExprKind.Add; left: Expr; right: Expr };

export type ErrorData =
  // token trees
  | { kind: "Unmatched"; left: TokenId }
  | { kind: "Mismatched"; left: TokenId; right: TokenId }
  | { kind: "Extra"; right: TokenId }

  // top-level
  | { kind: "ToplevelMissing"; pub: TokenId }
  | { kind: "ToplevelWrong"; wrong: TokenId }

  // `def`
  | { kind: "DefNameMissing"; def: TokenId }
  | { kind: "DefNameWrong"; wrong: TokenId }
  | { kind: "DefParamMissing"; name: TokenId }
  | { kind: "DefParamBracketWrong"; name: TokenId }
  | { kind: "DefTypeMissing"; colon: TokenId }
  | { kind: "DefEqualMissing"; name: TokenId }
  | { kind: "DefEqualWrong"; wrong: TokenId }
  | { kind: "DefBodyMissing"; equal: TokenId }

  // type
  | { kind: "TypeNameWrong"; wrong: TokenId }
  | { kind: "TypeBracketWrong"; wrong: TokenId }
  | { kind: "TypeExtra"; extra: TokenId }
  | { kind: "TypePairRightMissing"; comma: TokenId }

  // param
  | { kind: "ParamTypeMissing"; colon: TokenId }
  | { kind: "ParamPairRightMissing"; comma: TokenId }

  // bind
  | { kind: "BindNameWrong"; wrong: TokenId }
  | { kind: "BindBracketWrong"; wrong: TokenId }
  | { kind: "BindExtra"; extra: TokenId }
  | { kind: "BindPairRightMissing"; right: TokenId }

  // expr
  | { kind: "ExprNameWrong"; wrong: TokenId }
  | { kind: "ExprBracketWrong"; wrong: TokenId }
  | { kind: "ExprExtra"; extra: TokenId }
  | { kind: "ExprAddRightMissing"; op: TokenId };

export class ParseError extends Error {
  data: ErrorData;

  constructor(data: ErrorData) {
    super("parse error");
    this.data = data;
  }
}

export enum NodeKind {
  Leaf,
  Paren,
  Bracket,
  Brace,
}

type Bracket = NodeKind.Paren | NodeKind.Bracket | NodeKind.Brace;

const close = (bracket: Bracket): string => {
  switch (bracket) {
    case NodeKind.Paren:
      return ")";
    case NodeKind.Bracket:
      return "]";
    case NodeKind.Brace:
      return "}";
  }
};

export type Tree =
  | { kind: NodeKind.Leaf; id: TokenId }
  | {
      kind: Bracket;
      left: TokenId;
      right: TokenId;
      children: Tree[];
    };

export const forest = (tokens: Token[]): Tree[] => {
  let i = 0;

  const recurse = (): Tree[] => {
    const trees: Tree[] = [];

    const child = (kind: Bracket) => {
      const left = i++;
      const children = recurse();
      if (i >= tokens.length) throw new ParseError({ kind: "Unmatched", left });
      const right = i++;
      if (tokens[right].text !== close(kind))
        throw new ParseError({ kind: "Mismatched", left, right });
      trees.push({ kind, left, right, children });
    };

    while (i < tokens.length) {
      switch (tokens[i].text) {
        case "(":
          child(NodeKind.Paren);
          break;
        case "[":
          child(NodeKind.Bracket);
          break;
        case "{":
          child(NodeKind.Brace);
          break;
        case ")":
        case "]":
        case "}":
          return trees;
        default:
          trees.push({ kind: NodeKind.Leaf, id: i++ });
      }
    }

    return trees;
  };

  const trees = recurse();
  if (i < tokens.length) throw new ParseError({ kind: "Extra", right: i });
  return trees;
};

export const filter = (tokens: Token[], trees: Tree[]): Tree[] => {
  const filtered: Tree[] = [];
  for (const tree of trees) {
    if (tree.kind === NodeKind.Leaf) {
      switch (tokens[tree.id].type) {
        case "space":
        case "comment":
          break;
        default:
          filtered.push(tree);
      }
    } else {
      const { kind, left, right, children } = tree;
      filtered.push({ kind, left, right, children: filter(tokens, children) });
    }
  }
  return filtered;
};

/** Index of a `Tree`. */
export type TreeId = number;

export class Parser {
  tokens: Token[];
  trees: Tree[];
  i: TreeId;

  constructor(tokens: Token[], trees: Tree[]) {
    this.tokens = tokens;
    this.trees = trees;
    this.i = 0;
  }

  peek({ missing }: { missing: () => ErrorData }): Tree {
    if (this.i < this.trees.length) return this.trees[this.i];
    throw new ParseError(missing());
  }

  pop({ missing }: { missing: () => ErrorData }): Tree {
    const tree = this.peek({ missing });
    ++this.i;
    return tree;
  }

  leaf({
    pred,
    missing,
    wrong,
  }: {
    pred: (token: Token) => boolean;
    missing: () => ErrorData;
    wrong: (id: TokenId) => ErrorData;
  }): TokenId {
    const tree = this.pop({ missing });
    if (tree.kind !== NodeKind.Leaf) throw new ParseError(wrong(tree.left));
    const { id } = tree;
    if (!pred(this.tokens[id])) throw new ParseError(wrong(id));
    return id;
  }

  maybe(text: string): TokenId | undefined {
    if (this.i >= this.trees.length) return undefined;
    const tree = this.trees[this.i];
    if (tree.kind !== NodeKind.Leaf) return undefined;
    const { id } = tree;
    if (this.tokens[id].text === text) {
      ++this.i;
      return id;
    }
  }

  end({ extra }: { extra: (id: TokenId) => ErrorData }): undefined {
    if (this.i < this.trees.length) {
      const tree = this.peek({ missing: unreachable });
      throw new ParseError(
        extra(tree.kind === NodeKind.Leaf ? tree.id : tree.left),
      );
    }
  }

  pub(): TokenId | undefined {
    return this.maybe("pub");
  }

  comma(): TokenId | undefined {
    return this.maybe(",");
  }

  colon(): TokenId | undefined {
    return this.maybe(":");
  }

  plus(): TokenId | undefined {
    return this.maybe("+");
  }

  bindAtom({ missing }: { missing: () => ErrorData }): Bind {
    const tree = this.pop({ missing });
    if (tree.kind === NodeKind.Leaf) {
      const { id } = tree;
      if (this.tokens[id].type !== "id")
        throw new ParseError({ kind: "BindNameWrong", wrong: id });
      return { kind: BindKind.Name, name: id };
    }
    if (tree.kind !== NodeKind.Paren)
      throw new ParseError({ kind: "BindBracketWrong", wrong: tree.left });
    if (tree.children.length === 0) return { kind: BindKind.Unit };
    const parser = new Parser(this.tokens, tree.children);
    const { bind, type } = parser.param({ missing: unreachable });
    parser.end({ extra: (id) => ({ kind: "BindExtra", extra: id }) });
    if (type !== undefined)
      throw new ParseError({ kind: "BindPairRightMissing", right: tree.right });
    return bind;
  }

  bindElem({ missing }: { missing: () => ErrorData }): Bind {
    return this.bindAtom({ missing });
  }

  typeAtom({ missing }: { missing: () => ErrorData }): Type {
    const tree = this.pop({ missing });
    if (tree.kind === NodeKind.Leaf) {
      const { id } = tree;
      if (this.tokens[id].type !== "id")
        throw new ParseError({ kind: "TypeNameWrong", wrong: id });
      return { kind: TypeKind.Name, name: id };
    }
    if (tree.kind !== NodeKind.Paren)
      throw new ParseError({ kind: "TypeBracketWrong", wrong: tree.left });
    if (tree.children.length === 0) return { kind: TypeKind.Unit };
    const parser = new Parser(this.tokens, tree.children);
    const type = parser.type({ missing: unreachable });
    parser.end({ extra: (id) => ({ kind: "TypeExtra", extra: id }) });
    return type;
  }

  typeElem({ missing }: { missing: () => ErrorData }): Type {
    return this.typeAtom({ missing });
  }

  type({ missing }: { missing: () => ErrorData }): Type {
    const types = [this.typeElem({ missing })];
    let sep = this.comma();
    while (sep !== undefined) {
      const comma = sep;
      types.push(
        this.typeElem({
          missing: () => ({ kind: "TypePairRightMissing", comma }),
        }),
      );
      sep = this.comma();
    }
    return types.reduceRight((right, left) => {
      return { kind: TypeKind.Pair, left, right };
    });
  }

  paramElem({ missing }: { missing: () => ErrorData }): Param {
    const bind = this.bindElem({ missing });
    const colon = this.colon();
    const type =
      colon === undefined
        ? undefined
        : this.typeElem({
            missing: () => ({ kind: "ParamTypeMissing", colon }),
          });
    return { bind, type };
  }

  param({ missing }: { missing: () => ErrorData }): Param {
    const params = [this.paramElem({ missing })];
    let sep = this.comma();
    while (sep !== undefined) {
      const comma = sep;
      params.push(
        this.paramElem({
          missing: () => ({ kind: "ParamPairRightMissing", comma }),
        }),
      );
      sep = this.comma();
    }
    return params.reduceRight((right, left) => {
      return { bind: { kind: BindKind.Pair, left, right }, type: undefined };
    });
  }

  exprAtom({ missing }: { missing: () => ErrorData }): Expr {
    const tree = this.pop({ missing });
    if (tree.kind === NodeKind.Leaf) {
      const { id } = tree;
      if (this.tokens[id].type !== "id")
        throw new ParseError({ kind: "ExprNameWrong", wrong: id });
      return { kind: ExprKind.Name, name: id };
    }
    if (tree.kind !== NodeKind.Paren)
      throw new ParseError({ kind: "ExprBracketWrong", wrong: tree.left });
    if (tree.children.length === 0) return { kind: ExprKind.Unit };
    const parser = new Parser(this.tokens, tree.children);
    const expr = parser.expr({ missing: unreachable });
    parser.end({ extra: (id) => ({ kind: "ExprExtra", extra: id }) });
    return expr;
  }

  exprTerm({ missing }: { missing: () => ErrorData }): Expr {
    return this.exprAtom({ missing });
  }

  expr({ missing }: { missing: () => ErrorData }): Expr {
    let expr = this.exprTerm({ missing });
    let plus = this.plus();
    while (plus !== undefined) {
      const op = plus;
      const right = this.exprTerm({
        missing: () => ({ kind: "ExprAddRightMissing", op }),
      });
      expr = { kind: ExprKind.Add, left: expr, right };
      plus = this.plus();
    }
    return expr;
  }

  def(): Def {
    const pub = this.pub();

    const def = this.leaf({
      pred: ({ text }) => text === "def",
      missing: () => ({ kind: "ToplevelMissing", pub: unwrap(pub) }),
      wrong: (id) => ({ kind: "ToplevelWrong", wrong: id }),
    });
    const name = this.leaf({
      pred: ({ type }) => type === "id",
      missing: () => ({ kind: "DefNameMissing", def }),
      wrong: (id) => ({ kind: "DefNameWrong", wrong: id }),
    });

    const tree = this.pop({
      missing: () => ({ kind: "DefParamMissing", name }),
    });
    if (tree.kind !== NodeKind.Paren)
      throw new ParseError({ kind: "DefParamBracketWrong", name });
    let param: Param = { bind: { kind: BindKind.Unit }, type: undefined };
    if (tree.children.length > 0) {
      const parser = new Parser(this.tokens, tree.children);
      param = parser.param({ missing: unreachable });
    }

    const colon = this.colon();
    const type =
      colon === undefined
        ? undefined
        : this.type({ missing: () => ({ kind: "DefTypeMissing", colon }) });

    const equal = this.leaf({
      pred: ({ text }) => text === "=",
      missing: () => ({ kind: "DefEqualMissing", name: name }),
      wrong: (id) => ({ kind: "DefEqualWrong", wrong: id }),
    });
    const body = this.expr({
      missing: () => ({ kind: "DefBodyMissing", equal }),
    });

    return { pub, name, param, type, body };
  }

  module(): Module {
    const def: Def[] = [];
    while (this.i < this.trees.length) def.push(this.def());
    return { def };
  }
}

export const parse = (lex: Lexer, source: string): Module => {
  lex.reset(source);
  const tokens = [...lex];
  const trees = filter(tokens, forest(tokens));
  return new Parser(tokens, trees).module();
};
