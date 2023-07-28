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
    /// May satisfy `Constraint::Read` or `Constraint::Accum` depending on the block.
    Scope {
        id: id::Block,
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
        members: Vec<id::Ty>,
    },
}

/// A function definition.
#[derive(Debug)]
pub struct Function {
    /// Generic type parameters.
    pub generics: Vec<EnumSet<Constraint>>,
    /// Types used in this function definition.
    pub types: Vec<Ty>,
    /// Local variable types.
    pub vars: Vec<id::Ty>,
    /// Parameter variables.
    pub params: Vec<id::Var>,
    /// Return variable.
    pub ret: id::Var,
    /// Blocks of code.
    pub blocks: Vec<Block>,
    /// Main block.
    pub main: Vec<Instr>,
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
        generics: Vec<id::Ty>,
        args: Vec<id::Var>,
    },
    For {
        /// Must satisfy `Constraint::Index`.
        index: id::Ty,
        /// `arg` has type `index`.
        body: id::Block,
    },
    /// Scope for a `Ref` with `Constraint::Read`.
    Read {
        /// Contents of the `Ref`.
        var: id::Var,
        /// `arg` has type `Ref` with scope `body` and inner type same as `var`.
        body: id::Block,
    },
    /// Scope for a `Ref` with `Constraint::Accum`.
    Accum {
        /// Final contents of the `Ref`.
        var: id::Var,
        /// Topology of the `Ref`.
        shape: id::Var,
        /// `arg` has type `Ref` with scope `body` and inner type same as `shape`.
        body: id::Block,
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
