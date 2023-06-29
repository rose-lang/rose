pub mod build;
pub mod id;

use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// A type constraint.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Constraint {
    /// Can be the `index` type of an `Array`.
    Index,
    /// Has a zero value and an addition operation.
    Vector,
    /// Can be the `scope` type of a `Ref`.
    Scope,
}

/// A basic type or an index of a more complicated type.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Type {
    Unit,
    Bool,
    /// Satisfies `Constraint::Vector`.
    F64,
    /// A nonnegative integer less than `size`. Satisfies `Constraint::Index`.
    Fin {
        size: usize,
    },
    Generic {
        id: id::Generic,
    },
    /// Satisfies `Constraint::Scope`.
    Scope {
        /// Technically this points to the variable that will hold the value after the scope ends,
        /// but it doesn't matter as long as we're consistent.
        id: id::Var,
    },
    Expr {
        id: id::Typexpr,
    },
}

/// A more complicated type.
pub enum Typexpr {
    Ref {
        /// Must satisfy `Constraint::Scope`.
        scope: Type,
        inner: Type,
    },
    /// Satisfies `Constraint::Vector` if `elem` does.
    Array {
        /// Must satisfy `Constraint::Index`.
        index: Type,
        elem: Type,
    },
    /// Satisfies `Constraint::Vector` if all `members` do.
    Tuple { members: Vec<Type> },
    Def {
        def: Rc<Typedef>,
        /// Instantiations of the typedef's generic type parameters.
        params: Vec<Type>,
    },
}

/// A referenceable type definition. Guaranteed to be well-formed.
pub struct Typedef {
    generics: Vec<Option<Constraint>>,
    types: Vec<Typexpr>,
    def: Type,
    constraint: Option<Constraint>,
}

impl Typedef {
    /// Generic type parameters.
    pub fn generics(&self) -> &[Option<Constraint>] {
        &self.generics
    }

    /// Nontrivial types used in this typedef.
    pub fn types(&self) -> &[Typexpr] {
        &self.types
    }

    /// The definition of this type.
    pub fn def(&self) -> Type {
        self.def
    }

    /// Constraint satisfied by this type, if any.
    pub fn constraint(&self) -> Option<Constraint> {
        self.constraint
    }
}

/// Reference to a function, with types supplied for its generic parameters.
pub struct Func {
    pub def: Rc<Function>,
    pub generics: Vec<Type>,
}

/// A referenceable function definition. Guaranteed to be well-formed.
pub struct Function {
    generics: Vec<Option<Constraint>>,
    types: Vec<Typexpr>,
    funcs: Vec<Func>,
    param: Type,
    ret: Type,
    vars: Vec<Type>,
    body: Block,
}

impl Function {
    /// Generic type parameters.
    pub fn generics(&self) -> &[Option<Constraint>] {
        &self.generics
    }

    /// Nontrivial types used in this function definition.
    pub fn types(&self) -> &[Typexpr] {
        &self.types
    }

    /// Instantiations referenced functions with generic type parameters.
    pub fn funcs(&self) -> &[Func] {
        &self.funcs
    }

    /// Parameter types.
    pub fn param(&self) -> Type {
        self.param
    }

    /// Return types.
    pub fn ret(&self) -> Type {
        self.ret
    }

    /// Local variable types.
    pub fn vars(&self) -> &[Type] {
        &self.vars
    }

    /// Body of the function.
    pub fn body(&self) -> &Block {
        &self.body
    }
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Block {
    /// Input variable to this block.
    pub arg: id::Var,
    pub code: Vec<Instr>,
    /// Output variable from this block.
    pub ret: id::Var,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Instr {
    pub var: id::Var,
    pub expr: Expr,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum Expr {
    Unit,
    Bool {
        val: bool,
    },
    F64 {
        val: f64,
    },
    Fin {
        val: usize,
    },

    Array {
        elems: Vec<id::Var>,
    },
    Tuple {
        members: Vec<id::Var>,
    },

    Index {
        array: id::Var,
        index: id::Var,
    },
    Member {
        tuple: id::Var,
        member: id::Member,
    },

    Unary {
        op: Unop,
        arg: id::Var,
    },
    Binary {
        op: Binop,
        left: id::Var,
        right: id::Var,
    },

    Call {
        func: id::Func,
        arg: id::Var,
    },
    If {
        cond: id::Var,
        /// `arg` has type `Unit`.
        then: Box<Block>,
        /// `arg` has type `Unit`.
        els: Box<Block>,
    },
    For {
        /// Must satisfy `Constraint::Index`.
        index: Type,
        /// `arg` has type `index`.
        body: Box<Block>,
    },
    Accum {
        /// Final contents of the `Ref`.
        var: id::Var,
        /// Must satisfy `Constraint::Vector`.
        vector: Type,
        /// `arg` has type `Ref` with scope `var` and inner type `vector`.
        body: Box<Block>,
    },

    /// Accumulate into a `Ref`. Returned type is `Unit`.
    Add {
        /// The `Ref`, which must be in scope.
        accum: id::Var,
        /// Must be of the `Ref`'s inner type, which must satisfy `Constraint::Vector`.
        addend: id::Var,
    },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Unop {
    // `Bool` -> `Bool`
    Not,

    // `F64` -> `F64`
    Neg,
    Abs,
    Sqrt,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Binop {
    // `Bool` -> `Bool` -> `Bool`
    And,
    Or,
    Iff,
    Xor,

    // `F64` -> `F64` -> `Bool`
    Neq,
    Lt,
    Leq,
    Eq,
    Gt,
    Geq,

    // `F64` -> `F64` -> `F64`
    Add,
    Sub,
    Mul,
    Div,
}
