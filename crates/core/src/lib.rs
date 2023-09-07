pub mod id;

use enumset::{EnumSet, EnumSetType};

/// A type constraint.
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
        members: Box<[id::Ty]>,
    },
}

/// A function definition.
#[derive(Debug)]
pub struct Func {
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

/// Resolves `id::Func`s.
pub trait Refs<'a> {
    /// See `Node`.
    type Opaque;

    /// Resolve `id` to a function node.
    fn get(&self, id: id::Func) -> Option<Node<'a, Self::Opaque, Self>>
    where
        Self: Sized;
}

/// A node in a graph of functions.
#[derive(Clone, Debug, Copy)]
pub enum Node<'a, O, T: Refs<'a, Opaque = O>> {
    /// A function with an explicit body.
    Transparent {
        /// To traverse the graph by resolving functions called by this one.
        refs: T,
        /// The signature and definition of this function.
        def: &'a Func,
    },
    /// A function with an opaque body.
    Opaque {
        /// Generic type parameters.
        generics: &'a [EnumSet<Constraint>],
        /// Types used in this function's signature.
        types: &'a [Ty],
        /// Parameter types.
        params: &'a [id::Ty],
        /// Return type.
        ret: id::Ty,
        /// Definition of this function; semantics may vary.
        def: O,
    },
}

#[derive(Debug)]
pub struct Instr {
    pub var: id::Var,
    pub expr: Expr,
}

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
    Select {
        /// Must be of type `Bool`.
        cond: id::Var,
        then: id::Var,
        els: id::Var,
    },

    Call {
        id: id::Func,
        generics: Box<[id::Ty]>,
        args: Box<[id::Var]>,
    },
    For {
        /// Type must satisfy `Constraint::Index`.
        arg: id::Var,
        body: Box<[Instr]>,
        /// Variable from `body` holding an array element.
        ret: id::Var,
    },

    /// Start a scope for a `Ref` with `Constraint::Read`.
    Read {
        /// Contents of the `Ref`.
        var: id::Var,
    },
    /// Start a scope for a `Ref` with `Constraint::Accum`.
    Accum {
        /// Topology of the `Ref`.
        shape: id::Var,
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

    /// Consume a `Ref` to get its contained value.
    Resolve {
        /// The `Ref`, which must be in scope.
        var: id::Var,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Unop {
    // `Bool` -> `Bool`
    Not,

    // `F64` -> `F64`
    Neg,
    Abs,
    Sign,
    Sqrt,
}

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
