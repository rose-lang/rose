use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// Index of a typevar in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Var(pub usize);

/// Index of a function instantiation in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Func(pub usize);

/// Index of a generic parameter in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Generic(pub usize);

/// Index of a member in a tuple.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Member(pub usize);

/// Index of a local variable in a function context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Local(pub usize);

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Size {
    Const { val: usize },
    Generic { id: Generic },
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
    Var { id: Var },
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
pub struct Inst<T> {
    pub def: Rc<Def<Function<T>>>,
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
    Generic { id: Generic },
    Get { id: Local },
    Set { id: Local },
    Bool { val: bool },
    Int { val: u32 },
    Real { val: f64 },
    Vector { dim: usize },
    Tuple { id: Var },
    Index,
    Member { id: Member },
    Call { id: Func },
    Unary { op: Unop },
    Binary { op: Binop },
    If,
    Else,
    End,
    For { limit: Size },
}

#[derive(Debug)]
pub struct Function<T> {
    pub params: Vec<Type>,
    pub ret: Vec<Type>,
    pub locals: Vec<Type>,
    pub funcs: Vec<Inst<T>>,
    pub body: Vec<Instr>,
}

impl<T> Function<T> {
    pub fn get_func(&self, id: Func) -> &Inst<T> {
        &self.funcs[id.0]
    }
}
