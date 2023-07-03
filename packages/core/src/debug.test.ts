import * as wasm from "@rose-lang/wasm";
import { expect, test } from "vitest";
import { Real, add, fn, mul } from "./index.js";

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 16, align: 8 },
    Instr: { size: 24, align: 8 },
    Type: { size: 8, align: 4 },
    Typexpr: { size: 20, align: 4 },
  });
});

test("test rose to rust formatting", () => {
  const f = fn([Real, Real], Real, (x, y) => add(mul(x, 2), y));
  const g = wasm.js2Rust(f.f.f);
  // TODO: maybe make a more readable IR text format
  expect(g).toBe(
    `Function {
    generics: [],
    types: [
        Tuple {
            members: [
                F64,
                F64,
            ],
        },
    ],
    funcs: [],
    param: Expr {
        id: Typexpr(
            0,
        ),
    },
    ret: F64,
    vars: [
        Expr {
            id: Typexpr(
                0,
            ),
        },
        F64,
        F64,
        F64,
        F64,
        F64,
    ],
    blocks: [
        Block {
            arg: Var(
                0,
            ),
            code: [
                Instr {
                    var: Var(
                        1,
                    ),
                    expr: Member {
                        tuple: Var(
                            0,
                        ),
                        member: Member(
                            0,
                        ),
                    },
                },
                Instr {
                    var: Var(
                        2,
                    ),
                    expr: Member {
                        tuple: Var(
                            0,
                        ),
                        member: Member(
                            1,
                        ),
                    },
                },
                Instr {
                    var: Var(
                        3,
                    ),
                    expr: F64 {
                        val: 2.0,
                    },
                },
                Instr {
                    var: Var(
                        4,
                    ),
                    expr: Binary {
                        op: Mul,
                        left: Var(
                            1,
                        ),
                        right: Var(
                            3,
                        ),
                    },
                },
                Instr {
                    var: Var(
                        5,
                    ),
                    expr: Binary {
                        op: Add,
                        left: Var(
                            4,
                        ),
                        right: Var(
                            2,
                        ),
                    },
                },
            ],
            ret: Var(
                5,
            ),
        },
    ],
    main: Block(
        0,
    ),
}`
  );
});
