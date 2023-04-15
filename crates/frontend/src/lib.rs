mod ast;
mod lexer;
mod tokens;
mod translate;

#[macro_use]
extern crate lalrpop_util;

use std::ops::Range;

use lalrpop_util::ParseError;
use lexer::{Lexer, LexicalError};
use parser::ModuleParser;
use tokens::Token;
use translate::{translate, TypeError};

lalrpop_mod!(parser);

#[derive(Debug)]
pub enum OwnedToken {
    Basic(Token<'static>),
    Id(String),
}

impl<'a> From<Token<'a>> for OwnedToken {
    fn from(token: Token) -> Self {
        use OwnedToken::Basic;
        use Token::*;
        match token {
            Bind => Basic(Bind),
            Colon => Basic(Colon),
            Comma => Basic(Comma),
            Member => Basic(Member),
            Index => Basic(Index),
            LParen => Basic(LParen),
            RParen => Basic(RParen),
            LBracket => Basic(LBracket),
            RBracket => Basic(RBracket),
            LBrace => Basic(LBrace),
            RBrace => Basic(RBrace),
            Add => Basic(Add),
            Sub => Basic(Sub),
            Mul => Basic(Mul),
            Div => Basic(Div),
            Eq => Basic(Eq),
            Neq => Basic(Neq),
            Lt => Basic(Lt),
            Gt => Basic(Gt),
            Leq => Basic(Leq),
            Geq => Basic(Geq),
            And => Basic(And),
            Def => Basic(Def),
            Else => Basic(Else),
            For => Basic(For),
            If => Basic(If),
            In => Basic(In),
            Let => Basic(Let),
            Mod => Basic(Mod),
            Not => Basic(Not),
            Or => Basic(Or),
            Then => Basic(Then),
            Type => Basic(Type),
            Id(s) => OwnedToken::Id(s.to_owned()),
            Int(n) => Basic(Int(n)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("lex")]
    Lex(usize, LexicalError, usize),
    #[error("eof")]
    UnrecognizedEOF {
        location: usize,
        expected: Vec<String>,
    },
    #[error("wrong")]
    UnrecognizedToken {
        token: (usize, OwnedToken, usize),
        expected: Vec<String>,
    },
    #[error("extra")]
    ExtraToken { token: (usize, OwnedToken, usize) },
    #[error("type")]
    Typecheck(#[from] TypeError),
}

impl<'input> From<ParseError<usize, Token<'input>, (Range<usize>, LexicalError)>> for Error {
    fn from(error: ParseError<usize, Token<'input>, (Range<usize>, LexicalError)>) -> Self {
        match error {
            ParseError::InvalidToken { location } => {
                Error::Lex(location, LexicalError::InvalidToken, location)
            }
            ParseError::UnrecognizedEOF { location, expected } => {
                Error::UnrecognizedEOF { location, expected }
            }
            ParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Error::UnrecognizedToken {
                token: (start, token.into(), end),
                expected,
            },
            ParseError::ExtraToken {
                token: (start, token, end),
            } => Error::ExtraToken {
                token: (start, token.into(), end),
            },
            ParseError::User {
                error: (span, error),
            } => Error::Lex(span.start, error, span.end),
        }
    }
}

pub fn parse(source: &str) -> Result<rosebush::Module, Error> {
    let lexer = Lexer::new(&source);
    let ast = ModuleParser::new().parse(&source, lexer)?;
    Ok(translate(ast)?)
}
