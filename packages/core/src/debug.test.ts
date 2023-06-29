import * as wasm from "@rose-lang/wasm";
import { expect, test } from "vitest";
import { Real, add, fn, mul } from "./index.js";

test("test rose to rust formatting", () => {
  const f = fn([Real, Real], (x, y) => add(mul(x, 2), y));
  const g = wasm.js2Rust(f.f.f);
  expect(g).toBe(
    `Function {
    params: [
        Real,
        Real,
    ],
    ret: [
        Real,
    ],
    locals: [
        Real,
        Real,
        Real,
        Real,
    ],
    funcs: [],
    body: [
        Set {
            id: Local(
                0,
            ),
        },
        Set {
            id: Local(
                1,
            ),
        },
        Get {
            id: Local(
                1,
            ),
        },
        Real {
            val: 2.0,
        },
        Binary {
            op: MulReal,
        },
        Set {
            id: Local(
                2,
            ),
        },
        Get {
            id: Local(
                2,
            ),
        },
        Get {
            id: Local(
                0,
            ),
        },
        Binary {
            op: AddReal,
        },
        Set {
            id: Local(
                3,
            ),
        },
        Get {
            id: Local(
                3,
            ),
        },
    ],
}`
  );
});
