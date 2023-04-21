use logos::Logos;
use std::num::ParseIntError;

#[derive(Clone, Copy, Debug)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Copy, Clone, Debug, Logos)]
#[logos(error = Option<ParseIntError>, skip r"(\s|#[^\n]*)+")]
pub enum Token<'input> {
    #[token("=")]
    Bind,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Member,
    #[token("_")]
    Index,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,

    #[token("and")]
    And,
    #[token("def")]
    Def,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("let")]
    Let,
    #[token("mod")]
    Mod,
    #[token("not")]
    Not,
    #[token("or")]
    Or,
    #[token("then")]
    Then,
    #[token("type")]
    Type,

    #[regex(r"[A-Za-z][0-9A-Za-z]*")]
    Id(&'input str),

    #[regex(r"\d+", |lex| lex.slice().parse())]
    Int(u32),
}
