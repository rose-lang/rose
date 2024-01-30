import { describe, expect, test } from "vitest";
import { lexer } from "./lex.js";
import { Parser, filter, forest } from "./parse.js";
import { Printer } from "./pprint.js";

describe("roundtrip", () => {
  const roundtrip = (source: string) => {
    const lex = lexer();
    lex.reset(source);
    const tokens = [...lex];
    const trees = filter(tokens, forest(tokens));
    const mod = new Parser(tokens, trees).module();
    const printer = new Printer(tokens);
    printer.module(mod);
    expect(printer.flush()).toBe(source);
  };

  test("noop", () => {
    roundtrip("def noop() = ()\n");
  });

  test("add", () => {
    roundtrip("def add(a: f64, b: f64): f64 = a + b\n");
  });
});
