use clap::Parser;

#[derive(Parser)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.filename).unwrap();
    let module = rose_frontend::parse(&source).unwrap();
    for (name, func) in module.funcs {
        println!("{name} {:#?}", func.blocks());
    }
}
