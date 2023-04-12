use clap::Parser;
use rosebush_frontend::lexer::{Lexer, LexicalError};
use std::ops::Range;

#[derive(Parser)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.filename).unwrap();
    let lexer = Lexer::new(&source);
    let res = rosebush_frontend::parser::ModuleParser::new().parse(&source, lexer);
    match res {
        Ok(expr) => {
            println!("{:?}", expr);
        }
        Err(error) => {
            match error {
                lalrpop_util::ParseError::User {
                    error: LexicalError::InvalidToken(span),
                } => {
                    let Range { start, end } = span;
                    eprintln!("invalid token at {start}..{end}: {:?}", &source[span]);
                }
                _ => {
                    eprintln!("{:?}", error);
                }
            }
            panic!();
        }
    }
}
