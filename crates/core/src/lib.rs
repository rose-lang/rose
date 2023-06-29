pub mod id;

use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Size {
    Const { val: usize },
    Generic { id: id::Generic },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Type {
    Bool,
    Int,
    Real,
    Size { val: Size },
    Nat { bound: Size },
    Var { id: id::Typ },
}

#[derive(Debug)]
pub enum Typexpr {
    Vector {
        elem: Type,
        size: Size,
    },
    Tuple {
        members: Vec<Type>,
    },
    Typedef {
        def: Rc<Def<Typexpr>>,
        params: Vec<Size>,
    },
}

#[derive(Debug)]
pub struct Inst {
    pub def: Rc<Def<Function>>,
    /// Generic size parameters.
    pub params: Vec<Size>,
}

#[derive(Debug)]
pub struct Def<T> {
    /// Number of generic size parameters.
    pub generics: usize,
    pub types: Vec<Typexpr>,
    pub def: T,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Unop {
    // Bool -> Bool
    Not,

    // Int -> Int
    NegInt,
    AbsInt,

    // Real -> Real
    NegReal,
    AbsReal,
    Sqrt,

    // Vec<Int> -> Int
    SumInt,
    ProdInt,
    MaxInt,
    MinInt,

    // Vec<Real> -> Real
    SumReal,
    ProdReal,
    MaxReal,
    MinReal,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Binop {
    // Bool -> Bool -> Bool
    And,
    Or,
    EqBool,
    NeqBool,

    // Int -> Int -> Bool
    NeqInt,
    LtInt,
    LeqInt,
    EqInt,
    GtInt,
    GeqInt,

    // Real -> Real -> Bool
    NeqReal,
    LtReal,
    LeqReal,
    EqReal,
    GtReal,
    GeqReal,

    // Int -> Int -> Int
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    Mod,

    // Real -> Real -> Real
    AddReal,
    SubReal,
    MulReal,
    DivReal,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    Generic { id: id::Generic },
    Get { id: id::Local },
    Set { id: id::Local },
    Bool { val: bool },
    Int { val: u32 },
    Real { val: f64 },
    Vector { id: id::Typ },
    Tuple { id: id::Typ },
    Index,
    Member { id: id::Member },
    Call { id: id::Func },
    Unary { op: Unop },
    Binary { op: Binop },
    If,
    Else,
    End,
    For { limit: Size },
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Type>,
    pub ret: Vec<Type>,
    pub locals: Vec<Type>,
    pub funcs: Vec<Inst>,
    pub body: Vec<Instr>,
}
