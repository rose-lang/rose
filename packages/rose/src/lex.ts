import moo from "moo";

export const lexer = (): moo.Lexer =>
  moo.compile({
    op: /[^\s#\w"\(\)\[\]\{\},\.]+/,
    space: { match: /\s+/, lineBreaks: true },
    comment: /#.*?$/,
    str: { match: /".*?"/, lineBreaks: true },
    num: /\d+(?:\.\d+)?\b/,
    id: /(?!\d)\w+/,
    lparen: "(",
    rparen: ")",
    lbracket: "[",
    rbracket: "]",
    lbrace: "{",
    rbrace: "}",
    comma: ",",
    dot: ".",
  });
