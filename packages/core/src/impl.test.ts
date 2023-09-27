import * as wasm from "@rose-lang/wasm";
import { describe, expect, test } from "vitest";
import {
  Dual,
  Fn,
  Real,
  Vec,
  fn,
  inner,
  mul,
  mulLin,
  neg,
  opaque,
  vjp,
} from "./impl.js";

const pprint = (f: Fn): string => f[inner].pprint();

test("core IR type layouts", () => {
  // these don't matter too much, but it's good to notice if sizes increase
  expect(Object.fromEntries(wasm.layouts())).toEqual({
    Expr: { size: 24, align: 8 },
    Func: { size: 44, align: 4 },
    Instr: { size: 32, align: 8 },
    Ty: { size: 12, align: 4 },
    Val: { size: 16, align: 8 },
  });
});

describe("pprint", () => {
  test("opaque", () => {
    const f = opaque([Real], Real, (x) => x);
    const s = pprint(f);
    expect(s).toBe(
      `
fn f0 = <>{
  type T0 = F64
  opaque: (T0) -> T0
}
`.trimStart(),
    );
  });

  test("graph", () => {
    const exp = opaque([Real], Real, Math.exp);
    const sin = opaque([Real], Real, Math.sin);
    const cos = opaque([Real], Real, Math.cos);

    exp.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      const y = exp(x);
      return { re: y, du: mulLin(dx, y) };
    });
    sin.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      return { re: sin(x), du: mulLin(dx, cos(x)) };
    });
    cos.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
      return { re: cos(x), du: mulLin(dx, neg(sin(x))) };
    });

    const Complex = { re: Real, im: Real } as const;

    const complexp = fn([Complex], Complex, (z) => {
      const c = exp(z.re);
      return { re: mul(c, cos(z.im)), im: mul(c, sin(z.im)) };
    });

    const f = fn([Complex, Complex], Vec(2, Complex), (z, w) => {
      const { ret, grad } = vjp(complexp)(z);
      return [ret, grad(w)];
    });

    const s = pprint(f);
    expect(s).toBe(
      `
fn f0 = <>{
  type T0 = F64
  opaque: (T0) -> T0
}

fn f1 = <>{
  type T0 = F64
  opaque: (T0) -> T0
}

fn f2 = <>{
  type T0 = F64
  opaque: (T0) -> T0
}

fn f3 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T1
  type T6 = (T2, T0)
  type T7 = (T2, T6)
  (x0: T2) -> T7 {
    let x3: T0 = f0<>(x0)
    let x6: T6 = (x0, x3)
    let x7: T7 = (x3, x6)
    x7
  }
}

fn f4 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T0
  type T6 = &T1
  type T7 = (T2, T0, T0, T0)
  type T8 = (T2, T7)
  (x0: T2) -> T8 {
    let x3: T0 = f1<>(x0)
    let x4: T0 = f2<>(x0)
    let x5: T0 = -x4
    let x8: T7 = (x0, x3, x4, x5)
    let x9: T8 = (x3, x8)
    x9
  }
}

fn f5 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T1
  type T6 = (T2, T0, T0)
  type T7 = (T2, T6)
  (x0: T2) -> T7 {
    let x3: T0 = f2<>(x0)
    let x4: T0 = f1<>(x0)
    let x7: T6 = (x0, x3, x4)
    let x8: T7 = (x3, x7)
    x8
  }
}

fn f6 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = F64
  type T4 = (T2, T2)
  type T5 = Unit
  type T6 = &T4
  type T7 = &T1
  type T8 = &T2
  type T9 = F64
  type T10 = &T9
  type T11 = (T9, T9)
  type T12 = (T9, T11)
  type T13 = (T2, T12)
  type T14 = (T9, T9, T9, T9)
  type T15 = (T9, T14)
  type T16 = (T2, T15)
  type T17 = &T0
  type T18 = (T9, T9, T9)
  type T19 = (T9, T18)
  type T20 = (T2, T19)
  type T21 = (T4, T2, T12, T2, T15, T0, T2, T19, T0, T4)
  type T22 = (T4, T21)
  (x0: T4) -> T22 {
    let x1: T2 = x0[1]
    let x31: T13 = f3<>(x1)
    let x2: T2 = x31[0]
    let x32: T12 = x31[1]
    let x3: T2 = x0[0]
    let x33: T16 = f4<>(x3)
    let x4: T2 = x33[0]
    let x34: T15 = x33[1]
    let x19: T0 = x2 * x4
    let x6: T2 = x0[0]
    let x35: T20 = f5<>(x6)
    let x7: T2 = x35[0]
    let x36: T19 = x35[1]
    let x27: T0 = x2 * x7
    let x9: T4 = (x27, x19)
    let x37: T21 = (x0, x2, x32, x4, x34, x19, x7, x36, x27, x9)
    let x38: T22 = (x9, x37)
    x38
  }
}

fn f7 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T1
  type T6 = (T2, T0, T0)
  (x9: T4, x14: T2, x8: T6) -> T3 {
    let x7: T1 = 0
    let x0: T2 = x8[0]
    let x3: T0 = x8[1]
    let x4: T0 = x8[2]
    let x10: T5 = accum x7
    let x15: T3 = x10 += x14
    let x11: T1 = resolve x10
    let x12: T1 = x11 * x4
    let x13: T3 = x9 += x12
    let x16: T3 = unit
    x16
  }
}

fn f8 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T0
  type T6 = &T1
  type T7 = (T2, T0, T0, T0)
  (x10: T4, x17: T2, x9: T7) -> T3 {
    let x8: T1 = 0
    let x0: T2 = x9[0]
    let x3: T0 = x9[1]
    let x4: T0 = x9[2]
    let x5: T0 = x9[3]
    let x11: T5 = accum x5
    let x13: T6 = accum x8
    let x18: T3 = x13 += x17
    let x14: T1 = resolve x13
    let x15: T1 = x14 * x5
    let x16: T3 = x10 += x15
    let x12: T0 = resolve x11
    let x19: T3 = unit
    x19
  }
}

fn f9 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = Unit
  type T4 = &T2
  type T5 = &T1
  type T6 = (T2, T0)
  (x8: T4, x13: T2, x7: T6) -> T3 {
    let x6: T1 = 0
    let x0: T2 = x7[0]
    let x3: T0 = x7[1]
    let x9: T5 = accum x6
    let x14: T3 = x9 += x13
    let x10: T1 = resolve x9
    let x11: T1 = x10 * x3
    let x12: T3 = x8 += x11
    let x15: T3 = unit
    x15
  }
}

fn f10 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = F64
  type T3 = F64
  type T4 = (T2, T2)
  type T5 = Unit
  type T6 = &T4
  type T7 = &T1
  type T8 = &T2
  type T9 = F64
  type T10 = &T9
  type T11 = (T9, T9)
  type T12 = (T9, T11)
  type T13 = (T2, T12)
  type T14 = (T9, T9, T9, T9)
  type T15 = (T9, T14)
  type T16 = (T2, T15)
  type T17 = &T0
  type T18 = (T9, T9, T9)
  type T19 = (T9, T18)
  type T20 = (T2, T19)
  type T21 = (T4, T2, T12, T2, T15, T0, T2, T19, T0, T4)
  (x33: T6, x85: T4, x32: T21) -> T5 {
    let x31: T1 = 0
    let x0: T4 = x32[0]
    let x34: T7 = accum x31
    let x1: T2 = x0[1]
    let x36: T8 = &x33[1]
    let x2: T2 = x32[1]
    let x37: T12 = x32[2]
    let x38: T8 = accum x2
    let x3: T2 = x0[0]
    let x41: T8 = &x33[0]
    let x4: T2 = x32[3]
    let x42: T15 = x32[4]
    let x43: T8 = accum x4
    let x19: T0 = x32[5]
    let x46: T17 = accum x19
    let x48: T7 = accum x31
    let x52: T7 = accum x31
    let x56: T7 = accum x31
    let x6: T2 = x0[0]
    let x60: T8 = &x33[0]
    let x7: T2 = x32[6]
    let x61: T19 = x32[7]
    let x62: T8 = accum x7
    let x27: T0 = x32[8]
    let x65: T17 = accum x27
    let x67: T7 = accum x31
    let x71: T7 = accum x31
    let x75: T7 = accum x31
    let x9: T4 = x32[9]
    let x79: T6 = accum x9
    let x86: T5 = x79 += x85
    let x80: T4 = resolve x79
    let x83: T2 = x80[1]
    let x84: T5 = x56 += x83
    let x81: T2 = x80[0]
    let x82: T5 = x75 += x81
    let x76: T1 = resolve x75
    let x78: T5 = x71 += x76
    let x77: T5 = x67 += x76
    let x72: T1 = resolve x71
    let x73: T1 = x72 * x2
    let x74: T5 = x62 += x73
    let x68: T1 = resolve x67
    let x69: T1 = x68 * x7
    let x70: T5 = x38 += x69
    let x66: T0 = resolve x65
    let x63: T2 = resolve x62
    let x64: T5 = f7<>(x60, x63, x61)
    let x57: T1 = resolve x56
    let x59: T5 = x52 += x57
    let x58: T5 = x48 += x57
    let x53: T1 = resolve x52
    let x54: T1 = x53 * x2
    let x55: T5 = x43 += x54
    let x49: T1 = resolve x48
    let x50: T1 = x49 * x4
    let x51: T5 = x38 += x50
    let x47: T0 = resolve x46
    let x44: T2 = resolve x43
    let x45: T5 = f8<>(x41, x44, x42)
    let x39: T2 = resolve x38
    let x40: T5 = f9<>(x36, x39, x37)
    let x35: T1 = resolve x34
    let x87: T5 = unit
    x87
  }
}

fn f11 = <>{
  type T0 = F64
  type T1 = F64
  type T2 = (T0, T0)
  type T3 = 2
  type T4 = [T3]T2
  type T5 = Unit
  type T6 = &T2
  type T7 = &T0
  type T8 = (T0, T0)
  type T9 = (T0, T8)
  type T10 = (T0, T9)
  type T11 = (T0, T0, T0, T0)
  type T12 = (T0, T11)
  type T13 = (T0, T12)
  type T14 = (T0, T0, T0)
  type T15 = (T0, T14)
  type T16 = (T0, T15)
  type T17 = (T2, T0, T9, T0, T12, T0, T0, T15, T0, T2)
  type T18 = (T2, T17)
  (x0: T2, x1: T2) -> T4 {
    let x2: T18 = f6<>(x0)
    let x3: T2 = x2[0]
    let x4: T17 = x2[1]
    let x5: T6 = accum x0
    let x6: T5 = f10<>(x5, x1, x4)
    let x7: T2 = resolve x5
    let x8: T4 = [x3, x7]
    x8
  }
}
`.trimStart(),
    );
  });
});
