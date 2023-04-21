/// Index of a typedef in the module context.
#[derive(Clone, Copy, Debug)]
pub struct Typedef(pub usize);

/// Index of a function in the module context.
#[derive(Clone, Copy, Debug)]
pub struct Defn(pub usize);

/// Index of a typevar in a definition context.
#[derive(Clone, Copy, Debug)]
pub struct Var(pub usize);

/// Index of a function instantiation in a definition context.
#[derive(Clone, Copy, Debug)]
pub struct Func(pub usize);

/// Index of a generic parameter in a definition context.
#[derive(Clone, Copy, Debug)]
pub struct Generic(pub usize);

/// Index of a member in a tuple.
#[derive(Clone, Copy, Debug)]
pub struct Member(pub usize);

/// Index of a parameter in a function context.
#[derive(Clone, Copy, Debug)]
pub struct Param(pub usize);

/// Index of a local variable in a function context.
#[derive(Clone, Copy, Debug)]
pub struct Local(pub usize);

#[derive(Clone, Copy, Debug)]
pub enum Size {
    Const { val: usize },
    Generic { id: Generic },
}

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
    Vector { elem: Type, size: Size },
    Tuple { members: Vec<Type> },
    Typedef { id: Typedef, params: Vec<Size> },
}

#[derive(Debug)]
pub struct Inst {
    pub id: Defn,
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

#[derive(Clone, Copy, Debug)]
pub enum Instr {
    Generic { id: Generic },
    Param { id: Param },
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
pub struct Function {
    pub params: Vec<Type>,
    pub ret: Vec<Type>,
    pub locals: Vec<Type>,
    pub funcs: Vec<Inst>,
    pub body: Vec<Instr>,
}

#[derive(Debug)]
pub struct Module {
    pub types: Vec<Def<Typexpr>>,
    pub funcs: Vec<Def<Function>>,
}
