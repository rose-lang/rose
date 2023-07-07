pub mod id;

use enumset::{EnumSet, EnumSetType};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// A type constraint.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, EnumSetType)]
pub enum Constraint {
    /// Can be the `index` type of an `Array`.
    Index,
    /// Has a zero value and an addition operation.
    Vector,
    /// Can be the `scope` type of a `Ref`.
    Scope,
}

/// A type.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Ty {
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
        id: id::Block,
    },
    Ref {
        /// Must satisfy `Constraint::Scope`.
        scope: id::Ty,
        inner: id::Ty,
    },
    /// Satisfies `Constraint::Vector` if `elem` does.
    Array {
        /// Must satisfy `Constraint::Index`.
        index: id::Ty,
        elem: id::Ty,
    },
    /// Satisfies `Constraint::Vector` if all `members` do.
    Tuple {
        members: Vec<id::Ty>,
    },
}

/// Reference to a function, with types supplied for its generic parameters.
#[derive(Debug)]
pub struct Func {
    pub id: id::Function,
    pub generics: Vec<id::Ty>,
}

/// A function definition.
#[derive(Debug)]
pub struct Function {
    /// Generic type parameters.
    pub generics: Vec<EnumSet<Constraint>>,
    /// Types used in this function definition.
    pub types: Vec<Ty>,
    /// Instantiations referenced functions with generic type parameters.
    pub funcs: Vec<Func>,
    /// Parameter type.
    pub param: id::Ty,
    /// Return type.
    pub ret: id::Ty,
    /// Local variable types.
    pub vars: Vec<id::Ty>,
    /// Blocks of code.
    pub blocks: Vec<Block>,
    /// Main block.
    pub main: id::Block,
}

/// Wrapper for a `Function` that knows how to resolve its `id::Function`s.
pub trait FuncNode {
    fn def(&self) -> &Function;

    /// Only valid with `id::Function`s from `self.def().funcs`.
    fn get(&self, id: id::Function) -> Option<Self>
    where
        Self: Sized;
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

    Slice {
        /// Must actually be a `Ref` of an array, not just an array.
        array: id::Var,
        index: id::Var,
    },
    Field {
        /// Must actually be a `Ref` of a tuple, not just a tuple.
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
        then: id::Block,
        /// `arg` has type `Unit`.
        els: id::Block,
    },
    For {
        /// Must satisfy `Constraint::Index`.
        index: id::Ty,
        /// `arg` has type `index`.
        body: id::Block,
    },
    Accum {
        /// Final contents of the `Ref`.
        var: id::Var,
        /// Must satisfy `Constraint::Vector`.
        vector: id::Ty,
        /// `arg` has type `Ref` with scope `body` and inner type `vector`.
        body: id::Block,
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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
