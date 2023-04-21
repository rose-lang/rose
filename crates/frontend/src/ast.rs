use crate::tokens::{Binop, Unop};
use std::ops::Range;

#[derive(Clone, Copy, Debug)]
pub struct Spanned<T> {
    pub start: usize,
    pub val: T,
    pub end: usize,
}

impl<T> Spanned<T> {
    pub fn span(&self) -> Range<usize> {
        let &Spanned { start, end, .. } = self;
        Range { start, end }
    }
}

#[derive(Debug)]
pub enum Bind<'input> {
    Id { name: &'input str },
    Vector { elems: Vec<Bind<'input>> },
    Struct { members: Vec<&'input str> },
}

#[derive(Debug)]
pub enum Expr<'input> {
    Id {
        name: Spanned<&'input str>,
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
        member: Spanned<&'input str>,
    },
    Let {
        bind: Bind<'input>,
        val: Box<Expr<'input>>,
        body: Box<Expr<'input>>,
    },
    Call {
        func: Spanned<&'input str>,
        args: Vec<Expr<'input>>,
    },
    If {
        cond: Box<Expr<'input>>,
        then: Box<Expr<'input>>,
        els: Box<Expr<'input>>,
    },
    For {
        index: &'input str,
        limit: Spanned<&'input str>,
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
        members: Vec<(&'input str, Spanned<&'input str>)>,
    },
    Func {
        name: &'input str,
        params: Vec<(Bind<'input>, Spanned<&'input str>)>,
        typ: Spanned<&'input str>,
        body: Expr<'input>,
    },
}

#[derive(Debug)]
pub struct Module<'input> {
    pub defs: Vec<Def<'input>>,
}
