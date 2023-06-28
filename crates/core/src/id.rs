#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// Index of a typevar in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Typ(usize);

pub fn typ(id: usize) -> Typ {
    Typ(id)
}

impl Typ {
    pub fn typ(self) -> usize {
        self.0
    }
}

/// Index of a function instantiation in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Func(usize);

pub fn func(id: usize) -> Func {
    Func(id)
}

impl Func {
    pub fn func(self) -> usize {
        self.0
    }
}

/// Index of a generic parameter in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Generic(usize);

pub fn generic(id: usize) -> Generic {
    Generic(id)
}

impl Generic {
    pub fn generic(self) -> usize {
        self.0
    }
}

/// Index of a member in a tuple.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Member(usize);

pub fn member(id: usize) -> Member {
    Member(id)
}

impl Member {
    pub fn member(self) -> usize {
        self.0
    }
}

/// Index of a local variable in a function context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Local(usize);

pub fn local(id: usize) -> Local {
    Local(id)
}

impl Local {
    pub fn local(self) -> usize {
        self.0
    }
}
