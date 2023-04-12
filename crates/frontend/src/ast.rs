use crate::tokens::{Binop, Unop};

#[derive(Debug)]
pub enum Bind<'input> {
    Id { name: &'input str },
    Vector { elems: Vec<Bind<'input>> },
    Struct { members: Vec<&'input str> },
}

#[derive(Debug)]
pub enum Expr<'input> {
    Id {
        name: &'input str,
    },
    Int {
        val: u32,
    },
    Vector {
        elems: Vec<Expr<'input>>,
    },
    Struct {
        members: Vec<(&'input str, Expr<'input>)>,
    },
    Index {
        val: Box<Expr<'input>>,
        index: Box<Expr<'input>>,
    },
    Member {
        val: Box<Expr<'input>>,
        member: &'input str,
    },
    Let {
        bind: Bind<'input>,
        val: Box<Expr<'input>>,
        body: Box<Expr<'input>>,
    },
    Call {
        func: &'input str,
        args: Vec<Expr<'input>>,
    },
    If {
        cond: Box<Expr<'input>>,
        then: Box<Expr<'input>>,
        els: Box<Expr<'input>>,
    },
    For {
        index: &'input str,
        limit: Box<Expr<'input>>,
        body: Box<Expr<'input>>,
    },
    Unary {
        op: Unop,
        arg: Box<Expr<'input>>,
    },
    Binary {
        op: Binop,
        left: Box<Expr<'input>>,
        right: Box<Expr<'input>>,
    },
}

#[derive(Debug)]
pub enum Def<'input> {
    Type {
        name: &'input str,
        members: Vec<(&'input str, &'input str)>,
    },
    Func {
        name: &'input str,
        params: Vec<(Bind<'input>, &'input str)>,
        typ: &'input str,
        body: Expr<'input>,
    },
}

#[derive(Debug)]
pub struct Module<'input> {
    pub defs: Vec<Def<'input>>,
}
