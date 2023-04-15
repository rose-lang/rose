/// Index of a struct typedef in the module context.
#[derive(Clone, Copy, Debug)]
pub struct Struct(pub usize);

/// Index of a generic parameter for a struct or function.
#[derive(Clone, Copy, Debug)]
pub struct Generic(pub usize);

/// Index of a function in the module context.
#[derive(Clone, Copy, Debug)]
pub struct Func(pub usize);

/// Index of a member in a struct.
#[derive(Clone, Copy, Debug)]
pub struct Member(pub usize);

/// Index of a parameter for a function.
#[derive(Clone, Copy, Debug)]
pub struct Param(pub usize);

/// Index of a local variable of a function context.
#[derive(Clone, Copy, Debug)]
pub struct Local(pub usize);

#[derive(Clone, Copy, Debug)]
pub enum Size {
    Const(usize),
    /// Index of a generic size parameter in the current context.
    Generic {
        id: Generic,
    },
}

#[derive(Clone, Debug)]
pub enum Type {
    Bool,
    Int, // TODO: introduce a `Size`-bounded natural number type
    Real,
    Vector { elem: Box<Type>, size: Size },
    Struct { id: Struct, params: Vec<Size> },
}

#[derive(Debug)]
pub struct Typedef {
    /// Number of generic size parameters.
    pub generics: usize,
    pub fields: Vec<Type>,
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

#[derive(Debug)]
pub enum Expr {
    Generic {
        id: Generic,
    },
    Param {
        id: Param,
    },
    Bool(bool),
    Int(u32),
    Real(f64),
    Vector {
        elems: Vec<Local>,
    },
    Struct {
        members: Vec<Local>,
    },
    Index {
        val: Local,
        index: Local,
    },
    Member {
        val: Local,
        member: Member,
    },
    Call {
        func: Func,
        generics: Vec<Size>,
        args: Vec<Local>,
    },
    Unary {
        op: Unop,
        arg: Local,
    },
    Binary {
        op: Binop,
        left: Local,
        right: Local,
    },
    If {
        cond: Local,
        then: Vec<Instr>,
        els: Vec<Instr>,
    },
    For {
        index: Local,
        limit: Size,
        body: Vec<Instr>,
    },
}

#[derive(Debug)]
pub struct Instr {
    pub target: Local,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Function {
    /// Number of generic size parameters.
    pub generics: usize,
    pub params: Vec<Type>,
    pub ret: Type,
    pub locals: Vec<Type>, // TODO: pull all the types out into a `Vec` and use indices
    /// Ordered sequence of assignments to locals.
    pub body: Vec<Instr>,
}

#[derive(Debug)]
pub struct Module {
    pub types: Vec<Typedef>,
    pub funcs: Vec<Function>,
}
