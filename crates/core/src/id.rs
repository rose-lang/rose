/// Index of a member in a tuple.
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

/// Index of an uninstantiated function reference in a definition context.
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

/// Index of a generic type parameter in a definition context.
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

/// Index of a local variable in a function definition context.
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
