#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

// remember to `serde(rename)` everything here to avoid ts-rs name conflicts with non-ID types

#[cfg(test)]
use ts_rs::TS;

/// Index of a member in a tuple.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "MemberId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Member(usize);

pub fn member(id: usize) -> Member {
    Member(id)
}

impl Member {
    pub fn member(self) -> usize {
        self.0
    }
}

/// Index of a variant in an enum.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "VariantId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Variant(usize);

pub fn variant(id: usize) -> Variant {
    Variant(id)
}

impl Variant {
    pub fn variant(self) -> usize {
        self.0
    }
}

/// Index of an uninstantiated function reference in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "FunctionId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Function(usize);

pub fn function(id: usize) -> Function {
    Function(id)
}

impl Function {
    pub fn function(self) -> usize {
        self.0
    }
}

/// Index of a generic type parameter in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "GenericId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Generic(usize);

pub fn generic(id: usize) -> Generic {
    Generic(id)
}

impl Generic {
    pub fn generic(self) -> usize {
        self.0
    }
}

/// Index of a type in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "TyId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ty(usize);

pub fn ty(id: usize) -> Ty {
    Ty(id)
}

impl Ty {
    pub fn ty(self) -> usize {
        self.0
    }
}

/// Index of an instantiated function reference in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "FuncId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Func(usize);

pub fn func(id: usize) -> Func {
    Func(id)
}

impl Func {
    pub fn func(self) -> usize {
        self.0
    }
}

/// Index of a local variable in a function definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "VarId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Var(usize);

pub fn var(id: usize) -> Var {
    Var(id)
}

impl Var {
    pub fn var(self) -> usize {
        self.0
    }
}

/// Index of a block in a function definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(rename = "BlockId")
)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Block(usize);

pub fn block(id: usize) -> Block {
    Block(id)
}

impl Block {
    pub fn block(self) -> usize {
        self.0
    }
}
