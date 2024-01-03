import { Token } from "moo";

/** Index of a token in the original token array. */
export type TokenId = number;

export type ErrorData =
  // token trees
  | { kind: "Unmatched"; left: TokenId }
  | { kind: "Mismatched"; left: TokenId; right: TokenId }
  | { kind: "Extra"; right: TokenId }

  // `use`
  | { kind: "UseListMissing"; use: TokenId }
  | { kind: "UseListBraces"; use: TokenId; wrong: TokenId }
  | { kind: "UseFromMissing"; use: TokenId; brace: TokenId }
  | { kind: "UseFromWrong"; use: TokenId; wrong: TokenId }
  | { kind: "UseModuleMissing"; use: TokenId; from: TokenId }
  | { kind: "UseModuleWrong"; use: TokenId; wrong: TokenId }

  // `use` items
  | { kind: "UseItemWrong"; before: TokenId; wrong: TokenId }
  | { kind: "UseCommaMissing"; before: TokenId; after: TokenId }

  // infix
  | { kind: "InfixListMissing"; infix: TokenId }
  | { kind: "InfixListBrackets"; infix: TokenId; wrong: TokenId }
  | { kind: "InfixOpMissing"; infix: TokenId }
  | { kind: "InfixOpWrong"; infix: TokenId; wrong: TokenId }

  // infix precedence
  | { kind: "InfixPrecWrong"; before: TokenId; wrong: TokenId }
  | { kind: "InfixCommaMissing"; before: TokenId; after: TokenId };

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

/** Index of a tree in the outermost array returned by `forest`. */
export type TreeId = number;

const toplevelObj = {
  class: undefined,
  def: undefined,
  infix: undefined,
  infixl: undefined,
  infixr: undefined,
  instance: undefined,
  type: undefined,
  use: undefined,
  val: undefined,
};

export type ToplevelKind = keyof typeof toplevelObj;

const toplevelKinds = new Set(Object.keys(toplevelObj));

const isToplevelKind = (text: string): text is ToplevelKind =>
  toplevelKinds.has(text);

export const toplevel = (
  tokens: Token[],
  trees: Tree[],
): Map<ToplevelKind, TreeId[]> => {
  const toplevels = new Map<ToplevelKind, TreeId[]>();
  for (let i = 0; i < trees.length; ++i) {
    const tree = trees[i];
    const { kind } = tree;
    if (kind === NodeKind.Leaf) {
      const { text } = tokens[tree.id];
      if (isToplevelKind(text)) {
        let ids = toplevels.get(text);
        if (ids === undefined) {
          ids = [];
          toplevels.set(text, ids);
        }
        ids.push(i);
      }
    }
  }
  return toplevels;
};

const leaf = (
  pred: (token: Token) => boolean,
  wrong: (id: TokenId) => ErrorData,
  tokens: Token[],
  tree: Tree,
): TokenId => {
  if (tree.kind !== NodeKind.Leaf) throw new ParseError(wrong(tree.left));
  const { id } = tree;
  if (!pred(tokens[id])) throw new ParseError(wrong(id));
  return id;
};

const parseLeafList =
  (
    pred: (token: Token) => boolean,
    item: (before: TokenId) => (id: TokenId) => ErrorData,
    comma: (before: TokenId) => (id: TokenId) => ErrorData,
  ) =>
  (tokens: Token[], trees: Tree[], left: TokenId): TokenId[] => {
    const names: TokenId[] = [];
    let before = left;
    let i = 0;
    while (i < trees.length) {
      before = leaf(pred, item(before), tokens, trees[i]);
      names.push(before);

      const j = i + 1;
      if (j >= trees.length) break;
      before = leaf(
        ({ text }) => text === ",",
        comma(before),
        tokens,
        trees[j],
      );
      i = j + 1;
    }
    return names;
  };

export interface Use {
  pub: TokenId | undefined;
  names: TokenId[];
  module: TokenId;
}

const parseUseList = parseLeafList(
  ({ type }) => type === "op" || type === "id",
  (before) => (wrong) => ({ kind: "UseItemWrong", before, wrong }),
  (before) => (after) => ({ kind: "UseCommaMissing", before, after }),
);

export const parseUse = (tokens: Token[], trees: Tree[], id: TreeId): Use => {
  const first = trees[id];
  if (first.kind !== NodeKind.Leaf) throw Error("impossible");
  const use = first.id;

  let pub = undefined;
  const before = trees[id - 1];
  if (before?.kind === NodeKind.Leaf && tokens[before.id].text === "pub")
    pub = before.id;

  const list = trees[id + 1];
  if (list === undefined) throw new ParseError({ kind: "UseListMissing", use });
  switch (list.kind) {
    case NodeKind.Leaf:
      throw new ParseError({ kind: "UseListBraces", use, wrong: list.id });
    case NodeKind.Paren:
    case NodeKind.Bracket:
      throw new ParseError({ kind: "UseListBraces", use, wrong: list.left });
  }
  const names = parseUseList(tokens, list.children, list.left);

  const third = trees[id + 2];
  if (third === undefined)
    throw new ParseError({ kind: "UseFromMissing", use, brace: list.right });
  const from = leaf(
    ({ text }) => text === "from",
    (wrong) => ({ kind: "UseFromWrong", use, wrong }),
    tokens,
    third,
  );

  const str = trees[id + 3];
  if (str === undefined)
    throw new ParseError({ kind: "UseModuleMissing", use, from });
  const module = leaf(
    ({ type }) => type === "str",
    (wrong) => ({ kind: "UseModuleWrong", use, wrong }),
    tokens,
    str,
  );

  return { pub, names, module };
};

export interface Infix {
  pub: TokenId | undefined;
  assoc: TokenId;
  prec: TokenId[];
  op: TokenId;
}

const parseInfixList = parseLeafList(
  ({ type }) => type === "num",
  (before) => (wrong) => ({ kind: "InfixPrecWrong", before, wrong }),
  (before) => (after) => ({ kind: "InfixCommaMissing", before, after }),
);

export const parseInfix = (
  tokens: Token[],
  trees: Tree[],
  id: TreeId,
): Infix => {
  const first = trees[id];
  if (first.kind !== NodeKind.Leaf) throw Error("impossible");
  const infix = first.id;

  let pub = undefined;
  const before = trees[id - 1];
  if (before?.kind === NodeKind.Leaf && tokens[before.id].text === "pub")
    pub = before.id;

  const list = trees[id + 1];
  if (list === undefined)
    throw new ParseError({ kind: "InfixListMissing", infix });
  switch (list.kind) {
    case NodeKind.Leaf:
      throw new ParseError({
        kind: "InfixListBrackets",
        infix,
        wrong: list.id,
      });
    case NodeKind.Paren:
    case NodeKind.Brace:
      throw new ParseError({
        kind: "InfixListBrackets",
        infix,
        wrong: list.left,
      });
  }
  const prec = parseInfixList(tokens, list.children, list.left);

  let i = id + 2;
  while (i < trees.length) {
    const tree = trees[i];
    if (tree.kind === NodeKind.Leaf && tokens[tree.id].text === ":") break;
    ++i;
  }
  if (i >= trees.length)
    throw new ParseError({ kind: "InfixOpMissing", infix });
  const op = leaf(
    ({ type }) => type === "op",
    (wrong) => ({ kind: "InfixOpWrong", infix, wrong }),
    tokens,
    trees[i - 2],
  );

  return { pub, assoc: infix, prec, op };
};
