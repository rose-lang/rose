use clap::Parser;
use rosebush_frontend::lexer::Lexer;

#[derive(Parser)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.filename).unwrap();
    let lexer = Lexer::new(&source);
    let module = rosebush_frontend::parser::ModuleParser::new()
        .parse(&source, lexer)
        .unwrap();
    println!("{:?}", module);
}
