import { expect, test } from "vitest";
import { lexer } from "./lex.js";

const tokens = (
  source: string,
): { type: string | undefined; text: string }[] => {
  const lex = lexer();
  lex.reset(source);
  return [...lex].map(({ type, text }) => ({ type, text }));
};

test("comment", () => {
  expect(
    tokens(`# one comment

# another comment`),
  ).toEqual([
    { type: "comment", text: "# one comment" },
    { type: "space", text: "\n\n" },
    { type: "comment", text: "# another comment" },
  ]);
});

test("string", () => {
  expect(tokens('"one string" "another string"')).toEqual([
    { type: "str", text: '"one string"' },
    { type: "space", text: " " },
    { type: "str", text: '"another string"' },
  ]);
});

test("multiline string", () => {
  expect(tokens('"one string\nmultiple lines"')).toEqual([
    { type: "str", text: '"one string\nmultiple lines"' },
  ]);
});

test("integer", () => {
  expect(tokens("42")).toEqual([{ type: "num", text: "42" }]);
});

test("float", () => {
  expect(tokens("42.0")).toEqual([{ type: "num", text: "42.0" }]);
});

test("number followed by letter", () => {
  expect(() => tokens("1a")).toThrow("invalid syntax");
});

test("letter followed by number", () => {
  expect(tokens("a1")).toEqual([{ type: "id", text: "a1" }]);
});
