use walrus::{ir::BinaryOp, FunctionBuilder, LocalId, Module, ModuleConfig, ValType};

pub const EXPORT_NAME: &str = "";

fn type_to_wasm(t: rose::Type) -> ValType {
    match t {
        rose::Type::Bool => ValType::I32,
        rose::Type::Int => ValType::I32,
        rose::Type::Real => ValType::F64,
        rose::Type::Size { val: _ } => todo!(),
        rose::Type::Nat { bound: _ } => todo!(),
        rose::Type::Var { id: _ } => todo!(),
    }
}

pub fn compile(f: &rose::Def<rose::Function>, _generics: &[usize]) -> Vec<u8> {
    let config = ModuleConfig::new();
    let mut module = Module::with_config(config);
    let params: Vec<ValType> = f.def.params.iter().map(|&t| type_to_wasm(t)).collect();
    let results: Vec<ValType> = f.def.ret.iter().map(|&t| type_to_wasm(t)).collect();
    let mut func = FunctionBuilder::new(&mut module.types, &params, &results);
    let params: Vec<LocalId> = params.into_iter().map(|t| module.locals.add(t)).collect();
    let mut body = func.func_body();
    for &id in params.iter() {
        body.local_get(id);
    }
    let locals: Vec<LocalId> = f
        .def
        .locals
        .iter()
        .map(|&t| module.locals.add(type_to_wasm(t)))
        .collect();
    for &instr in &f.def.body {
        match instr {
            rose::Instr::Generic { id: _ } => todo!(),
            rose::Instr::Get { id } => {
                body.local_get(locals[id.0]);
            }
            rose::Instr::Set { id } => {
                body.local_set(locals[id.0]);
            }
            rose::Instr::Bool { val: _ } => todo!(),
            rose::Instr::Int { val: _ } => todo!(),
            rose::Instr::Real { val: _ } => todo!(),
            rose::Instr::Vector { id: _ } => todo!(),
            rose::Instr::Tuple { id: _ } => todo!(),
            rose::Instr::Index => todo!(),
            rose::Instr::Member { id: _ } => todo!(),
            rose::Instr::Call { id: _ } => todo!(),
            rose::Instr::Unary { op: _ } => todo!(),
            rose::Instr::Binary { op } => match op {
                rose::Binop::And => todo!(),
                rose::Binop::Or => todo!(),
                rose::Binop::EqBool => todo!(),
                rose::Binop::NeqBool => todo!(),
                rose::Binop::NeqInt => todo!(),
                rose::Binop::LtInt => todo!(),
                rose::Binop::LeqInt => todo!(),
                rose::Binop::EqInt => todo!(),
                rose::Binop::GtInt => todo!(),
                rose::Binop::GeqInt => todo!(),
                rose::Binop::NeqReal => todo!(),
                rose::Binop::LtReal => todo!(),
                rose::Binop::LeqReal => todo!(),
                rose::Binop::EqReal => todo!(),
                rose::Binop::GtReal => todo!(),
                rose::Binop::GeqReal => todo!(),
                rose::Binop::AddInt => todo!(),
                rose::Binop::SubInt => todo!(),
                rose::Binop::MulInt => todo!(),
                rose::Binop::DivInt => todo!(),
                rose::Binop::Mod => todo!(),
                rose::Binop::AddReal => {
                    body.binop(BinaryOp::F64Add);
                }
                rose::Binop::SubReal => todo!(),
                rose::Binop::MulReal => todo!(),
                rose::Binop::DivReal => todo!(),
            },
            rose::Instr::If => todo!(),
            rose::Instr::Else => todo!(),
            rose::Instr::End => todo!(),
            rose::Instr::For { limit: _ } => todo!(),
        }
    }
    module
        .exports
        .add(EXPORT_NAME, func.finish(params, &mut module.funcs));
    module.emit_wasm()
}
