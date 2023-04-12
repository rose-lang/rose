use crate::tokens::Token;
use logos::{Logos, SpannedIter};
use std::{num::ParseIntError, ops::Range};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug)]
pub enum LexicalError {
    InvalidToken,
    IntOverflow,
}

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, (Range<usize>, LexicalError)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| match token {
            Err(error) => Err((
                span,
                match error {
                    Some(ParseIntError { .. }) => LexicalError::IntOverflow,
                    None => LexicalError::InvalidToken,
                },
            )),
            Ok(tok) => Ok((span.start, tok, span.end)),
        })
    }
}
