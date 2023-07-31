pub mod id;

use enumset::{EnumSet, EnumSetType};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// A type constraint.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[allow(clippy::derived_hash_with_manual_eq)] // `PartialEq` impl comes from enumset; should be fine
#[derive(Debug, EnumSetType, Hash)]
pub enum Constraint {
    /// Not a `Ref`.
    Value,
    /// Can be the `index` type of an `Array`.
    Index,
    /// Allows a `Ref` to be read when used as its `scope` type.
    Read,
    /// Allows a `Ref` to be accumulated into when used as its `scope` type.
    Accum,
}

/// A type.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Ty {
    Unit,
    Bool,
    F64,
    /// A nonnegative integer less than `size`. Satisfies `Constraint::Index`.
    Fin {
        size: usize,
    },
    Generic {
        id: id::Generic,
    },
    Scope {
        /// Must be either `Read` or `Accum`.
        kind: Constraint,
        /// The `arg` variable of the `Expr` introducing this scope.
        id: id::Var,
    },
    Ref {
        scope: id::Ty,
        inner: id::Ty,
    },
    Array {
        /// Must satisfy `Constraint::Index`.
        index: id::Ty,
        elem: id::Ty,
    },
    Tuple {
        members: Vec<id::Ty>, // TODO: change to `Box<[id::Ty]`
    },
}

/// A function definition.
#[derive(Debug)]
pub struct Function {
    /// Generic type parameters.
    pub generics: Box<[EnumSet<Constraint>]>,
    /// Types used in this function definition.
    pub types: Box<[Ty]>,
    /// Local variable types.
    pub vars: Box<[id::Ty]>,
    /// Parameter variables.
    pub params: Box<[id::Var]>,
    /// Return variable.
    pub ret: id::Var,
    /// Function body.
    pub body: Box<[Instr]>,
}

/// Wrapper for a `Function` that knows how to resolve its `id::Function`s.
pub trait FuncNode {
    fn def(&self) -> &Function;

    /// Only valid with `id::Function`s from `self.def().funcs`.
    fn get(&self, id: id::Function) -> Option<Self>
    where
        Self: Sized;
}

// #[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Instr {
    pub var: id::Var,
    pub expr: Expr,
}

// #[cfg_attr(test, derive(TS), ts(export))]
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
        elems: Box<[id::Var]>,
    },
    Tuple {
        members: Box<[id::Var]>,
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
        field: id::Member,
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
    Select {
        /// Must be of type `Bool`.
        cond: id::Var,
        then: id::Var,
        els: id::Var,
    },

    Call {
        id: id::Function,
        generics: Box<[id::Ty]>,
        args: Box<[id::Var]>,
    },
    For {
        /// Must satisfy `Constraint::Index`.
        index: id::Ty,
        /// has type `index`.
        arg: id::Var,
        body: Box<[Instr]>,
        /// Variable from `body` holding an array element.
        ret: id::Var,
    },
    /// Scope for a `Ref` with `Constraint::Read`. Returns `Unit`.
    Read {
        /// Contents of the `Ref`.
        var: id::Var,
        /// Has type `Ref` with scope `arg` and inner type same as `var`.
        arg: id::Var,
        body: Box<[Instr]>,
        /// Variable from `body` holding the result of this block; escapes into outer scope.
        ret: id::Var,
    },
    /// Scope for a `Ref` with `Constraint::Accum`. Returns the final contents of the `Ref`.
    Accum {
        /// Topology of the `Ref`.
        shape: id::Var,
        /// Has type `Ref` with scope `arg` and inner type same as `shape`.
        arg: id::Var,
        body: Box<[Instr]>,
        /// Variable from `body` holding the result of this block; escapes into outer scope.
        ret: id::Var,
    },

    /// Read from a `Ref` whose `scope` satisfies `Constraint::Read`.
    Ask {
        /// The `Ref`, which must be in scope.
        var: id::Var,
    },
    /// Accumulate into a `Ref` whose `scope` satisfies `Constraint::Accum`. Returns `Unit`.
    Add {
        /// The `Ref`, which must be in scope.
        accum: id::Var,
        /// Must be of the `Ref`'s inner type.
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
