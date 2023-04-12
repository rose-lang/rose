use crate::tokens::Token;
use logos::{Logos, SpannedIter};
use std::ops::Range;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug)]
pub enum LexicalError {
    InvalidToken(Range<usize>),
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
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| match token {
            Err(_) => Err(LexicalError::InvalidToken(span)),
            Ok(tok) => Ok((span.start, tok, span.end)),
        })
    }
}
