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
        id: id::Block,
    },
    Expr {
        id: id::Typexpr,
    },
}

/// A more complicated type.
#[derive(Debug)]
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
        id: id::Typedef,
        /// Instantiations of the typedef's generic type parameters.
        params: Vec<Type>,
    },
}

/// A type definition.
#[derive(Debug)]
pub struct Typedef {
    /// Generic type parameters.
    pub generics: Vec<EnumSet<Constraint>>,
    /// Nontrivial types used in this typedef.
    pub types: Vec<Typexpr>,
    /// The definition of this type.
    pub def: Type,
    /// Constraints satisfied by this type, if any.
    pub constraints: EnumSet<Constraint>,
}

/// Wrapper for a `Typedef` that knows how to resolve its `id::Typedef`s.
pub trait TypeNode {
    fn def(&self) -> &Typedef;

    fn ty(&self, id: id::Typedef) -> Option<Self>
    where
        Self: Sized;
}

/// Reference to a function, with types supplied for its generic parameters.
#[derive(Debug)]
pub struct Func {
    pub id: id::Function,
    pub generics: Vec<Type>,
}

/// A function definition.
#[derive(Debug)]
pub struct Function {
    /// Generic type parameters.
    pub generics: Vec<EnumSet<Constraint>>,
    /// Nontrivial types used in this function definition.
    pub types: Vec<Typexpr>,
    /// Instantiations referenced functions with generic type parameters.
    pub funcs: Vec<Func>,
    /// Parameter types.
    pub param: Type,
    /// Return types.
    pub ret: Type,
    /// Local variable types.
    pub vars: Vec<Type>,
    /// Blocks of code.
    pub blocks: Vec<Block>,
    /// Main block.
    pub main: id::Block,
}

/// Wrapper for a `Function` that knows how to resolve its `id::Typedef`s and `id::Function`s.
pub trait FuncNode {
    type Ty: TypeNode;

    fn def(&self) -> &Function;

    fn ty(&self, id: id::Typedef) -> Option<Self::Ty>;

    fn func(&self, id: id::Function) -> Option<Self>
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
        index: Type,
        /// `arg` has type `index`.
        body: id::Block,
    },
    Accum {
        /// Final contents of the `Ref`.
        var: id::Var,
        /// Must satisfy `Constraint::Vector`.
        vector: Type,
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
