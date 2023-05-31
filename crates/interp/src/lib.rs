use rose::{Binop, Defn, Instr, Module};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Bool(bool),
    I32(i32),
    F64(f64),
    Tuple(Vec<Rc<Val>>),
    Vector(Vec<Rc<Val>>),
}

pub fn interp(module: &Module, function: Defn, args: Vec<Val>) -> Vec<Val> {
    let f = module.get_func(function);
    let mut locals: Vec<Option<Val>> = vec![None; f.def.locals.len()];
    let mut stack = args;
    for &instr in f.def.body.iter() {
        match instr {
            Instr::Generic { id } => todo!(),
            Instr::Get { id } => {
                stack.push(locals[id.0].as_ref().unwrap().clone());
            }
            Instr::Set { id } => {
                locals[id.0] = stack.pop();
            }
            Instr::Bool { val } => todo!(),
            Instr::Int { val } => todo!(),
            Instr::Real { val } => {
                stack.push(Val::F64(val));
            }
            Instr::Vector { dim } => todo!(),
            Instr::Tuple { id } => todo!(),
            Instr::Index => todo!(),
            Instr::Member { id } => todo!(),
            Instr::Call { id } => {
                let defn = f.def.get_func(id).id;
                let g = module.get_func(defn);
                let args = stack.drain(stack.len() - g.def.params.len()..).collect();
                stack.append(&mut interp(module, defn, args));
            }
            Instr::Unary { op } => todo!(),
            Instr::Binary { op } => match op {
                Binop::And => todo!(),
                Binop::Or => todo!(),
                Binop::EqBool => todo!(),
                Binop::NeqBool => todo!(),
                Binop::NeqInt => todo!(),
                Binop::LtInt => todo!(),
                Binop::LeqInt => todo!(),
                Binop::EqInt => todo!(),
                Binop::GtInt => todo!(),
                Binop::GeqInt => todo!(),
                Binop::NeqReal => todo!(),
                Binop::LtReal => todo!(),
                Binop::LeqReal => todo!(),
                Binop::EqReal => todo!(),
                Binop::GtReal => todo!(),
                Binop::GeqReal => todo!(),
                Binop::AddInt => todo!(),
                Binop::SubInt => todo!(),
                Binop::MulInt => todo!(),
                Binop::DivInt => todo!(),
                Binop::Mod => todo!(),
                Binop::AddReal => {
                    if let Val::F64(b) = stack.pop().unwrap() {
                        if let Val::F64(a) = stack.pop().unwrap() {
                            stack.push(Val::F64(a + b));
                        }
                    }
                }
                Binop::SubReal => todo!(),
                Binop::MulReal => {
                    if let Val::F64(b) = stack.pop().unwrap() {
                        if let Val::F64(a) = stack.pop().unwrap() {
                            stack.push(Val::F64(a * b));
                        }
                    }
                }
                Binop::DivReal => todo!(),
            },
            Instr::If => todo!(),
            Instr::Else => todo!(),
            Instr::End => todo!(),
            Instr::For { limit } => todo!(),
        }
    }
    stack
}

#[cfg(test)]
mod tests {
    use super::*;
    use rose::{Binop, Def, Func, Function, Inst, Local, Type};

    #[test]
    fn test_two_plus_two() {
        let module = Module {
            types: vec![],
            funcs: vec![Def {
                generics: 0,
                types: vec![],
                def: Function {
                    params: vec![Type::Real, Type::Real],
                    ret: vec![Type::Real],
                    locals: vec![],
                    funcs: vec![],
                    body: vec![Instr::Binary { op: Binop::AddReal }],
                },
            }],
        };
        let answer = interp(&module, Defn(0), vec![Val::F64(2.), Val::F64(2.)]);
        assert_eq!(answer, vec![Val::F64(4.)]);
    }

    #[test]
    fn test_nested_call() {
        let module = Module {
            types: vec![],
            funcs: vec![
                Def {
                    generics: 0,
                    types: vec![],
                    def: Function {
                        params: vec![],
                        ret: vec![Type::Real],
                        locals: vec![],
                        funcs: vec![],
                        body: vec![Instr::Real { val: 42. }],
                    },
                },
                Def {
                    generics: 0,
                    types: vec![],
                    def: Function {
                        params: vec![],
                        ret: vec![Type::Real],
                        locals: vec![Type::Real],
                        funcs: vec![Inst {
                            id: Defn(0),
                            params: vec![],
                        }],
                        body: vec![
                            Instr::Call { id: Func(0) },
                            Instr::Set { id: Local(0) },
                            Instr::Get { id: Local(0) },
                            Instr::Get { id: Local(0) },
                            Instr::Binary { op: Binop::MulReal },
                        ],
                    },
                },
            ],
        };
        let answer = interp(&module, Defn(1), vec![]);
        assert_eq!(answer, vec![Val::F64(1764.)]);
    }
}