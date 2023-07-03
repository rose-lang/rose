use rose::{id, Binop, Expr, FuncNode, Type, Typexpr, Unop};
use std::{cell::Cell, rc::Rc};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// A resolved type.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Typeval {
    Unit,
    Bool,
    F64,
    Fin {
        size: usize,
    },
    Scope, // we erase scope info in the interpreter
    Ref {
        inner: Rc<Typeval>,
    },
    Array {
        index: Rc<Typeval>,
        elem: Rc<Typeval>,
    },
    Tuple {
        members: Rc<Vec<Typeval>>,
    },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Unit,
    Bool(bool),
    F64(f64),
    Fin(usize),
    Ref(Rc<Cell<f64>>),
    Array(Rc<Vec<Val>>), // assume all indices are `Fin`
    Tuple(Rc<Vec<Val>>),
}

impl Val {
    fn bool(&self) -> bool {
        match self {
            &Val::Bool(x) => x,
            _ => unreachable!(),
        }
    }

    fn f64(&self) -> f64 {
        match self {
            &Val::F64(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Typeval {
    /// Return zero a value of `Ref` type for this type, which must satisfy `Constraint::Vector`.
    fn zero(&self) -> Val {
        match self {
            Self::F64 => Val::Ref(Rc::new(Cell::new(0.))),
            Self::Array { index, elem } => match index.as_ref() {
                &Typeval::Fin { size } => Val::Array(Rc::new(vec![elem.as_ref().zero(); size])),
                _ => unreachable!(),
            },
            Self::Tuple { members } => {
                Val::Tuple(Rc::new(members.iter().map(|x| x.zero()).collect()))
            }
            Self::Unit | Self::Bool | Self::Fin { .. } | Self::Scope | Self::Ref { .. } => {
                unreachable!()
            }
        }
    }
}

impl Val {
    /// Pull out the immutable inner value represented by this mutable `Ref` type.
    fn immut(&self) -> Self {
        match self {
            Self::Ref(x) => Self::F64(x.get()),
            Self::Array(x) => Self::Array(Rc::new(x.iter().map(|x| x.immut()).collect())),
            Self::Tuple(x) => Self::Tuple(Rc::new(x.iter().map(|x| x.immut()).collect())),
            Self::Unit | Self::Bool(..) | Self::F64(..) | Self::Fin { .. } => {
                unreachable!()
            }
        }
    }

    /// Add `x` to this value, which must represent a mutable `Ref` type.
    fn add(&self, x: &Self) {
        match (self, x) {
            (Self::Ref(a), Self::F64(b)) => a.set(a.get() + b),
            (Self::Array(a), Self::Array(b)) => {
                for (a, b) in a.iter().zip(b.iter()) {
                    a.add(b);
                }
            }
            (Self::Tuple(a), Self::Tuple(b)) => {
                for (a, b) in a.iter().zip(b.iter()) {
                    a.add(b);
                }
            }
            _ => unreachable!(),
        }
    }
}

fn resolve(generics: &[Typeval], types: &[Typeval], t: Type) -> Typeval {
    match t {
        Type::Unit => Typeval::Unit,
        Type::Bool => Typeval::Bool,
        Type::F64 => Typeval::F64,
        Type::Fin { size } => Typeval::Fin { size },
        Type::Generic { id } => generics[id.generic()].clone(),
        Type::Scope { id: _ } => Typeval::Scope,
        Type::Expr { id } => types[id.typexpr()].clone(),
    }
}

struct Interpreter<'a, F: FuncNode> {
    f: &'a F,
    generics: &'a [Typeval],
    types: Vec<Typeval>,
    vars: Vec<Option<Val>>,
}

impl<'a, F: FuncNode> Interpreter<'a, F> {
    fn new(f: &'a F, generics: &'a [Typeval]) -> Self {
        let mut types = vec![];
        for t in &f.def().types {
            let v = match t {
                &Typexpr::Ref { scope: _, inner } => Typeval::Ref {
                    inner: Rc::new(resolve(generics, &types, inner)),
                },
                &Typexpr::Array { index, elem } => Typeval::Array {
                    index: Rc::new(resolve(generics, &types, index)),
                    elem: Rc::new(resolve(generics, &types, elem)),
                },
                Typexpr::Tuple { members } => Typeval::Tuple {
                    members: Rc::new(
                        members
                            .iter()
                            .map(|x| resolve(generics, &types, *x))
                            .collect(),
                    ),
                },
                Typexpr::Def { id: _, params: _ } => todo!(),
            };
            types.push(v);
        }
        Self {
            f,
            generics,
            types,
            vars: vec![None; f.def().vars.len()],
        }
    }

    fn resolve(&self, t: Type) -> Typeval {
        resolve(self.generics, &self.types, t)
    }

    fn get(&self, var: id::Var) -> &Val {
        self.vars[var.var()].as_ref().unwrap()
    }

    fn expr(&mut self, expr: &Expr) -> Val {
        match expr {
            Expr::Unit => Val::Unit,
            &Expr::Bool { val } => Val::Bool(val),
            &Expr::F64 { val } => Val::F64(val),
            &Expr::Fin { val } => Val::Fin(val),

            Expr::Array { elems } => Val::Array(Rc::new(
                elems.iter().map(|&x| self.get(x).clone()).collect(),
            )),
            Expr::Tuple { members } => Val::Tuple(Rc::new(
                members.iter().map(|&x| self.get(x).clone()).collect(),
            )),

            &Expr::Index { array, index } => match (self.get(array), self.get(index)) {
                (Val::Array(v), &Val::Fin(i)) => v[i].clone(),
                _ => unreachable!(),
            },
            &Expr::Member { tuple, member } => match self.get(tuple) {
                Val::Tuple(x) => x[member.member()].clone(),
                _ => unreachable!(),
            },

            // a `Ref` of `F64` becomes `Ref`, while composites just wrap those individual refs
            &Expr::Slice { array, index } => match (self.get(array), self.get(index)) {
                (Val::Array(v), &Val::Fin(i)) => v[i].clone(),
                _ => unreachable!(),
            },

            &Expr::Unary { op, arg } => {
                let x = self.get(arg);
                match op {
                    Unop::Not => Val::Bool(!x.bool()),

                    Unop::Neg => Val::F64(-x.f64()),
                    Unop::Abs => Val::F64(x.f64().abs()),
                    Unop::Sqrt => Val::F64(x.f64().sqrt()),
                }
            }
            &Expr::Binary { op, left, right } => {
                let x = self.get(left);
                let y = self.get(right);
                match op {
                    Binop::And => Val::Bool(x.bool() && y.bool()),
                    Binop::Or => Val::Bool(x.bool() || y.bool()),
                    Binop::Iff => Val::Bool(x.bool() == y.bool()),
                    Binop::Xor => Val::Bool(x.bool() != y.bool()),

                    Binop::Neq => Val::Bool(x.f64() != y.f64()),
                    Binop::Lt => Val::Bool(x.f64() < y.f64()),
                    Binop::Leq => Val::Bool(x.f64() <= y.f64()),
                    Binop::Eq => Val::Bool(x.f64() == y.f64()),
                    Binop::Gt => Val::Bool(x.f64() > y.f64()),
                    Binop::Geq => Val::Bool(x.f64() >= y.f64()),

                    Binop::Add => Val::F64(x.f64() + y.f64()),
                    Binop::Sub => Val::F64(x.f64() - y.f64()),
                    Binop::Mul => Val::F64(x.f64() * y.f64()),
                    Binop::Div => Val::F64(x.f64() / y.f64()),
                }
            }

            &Expr::Call { func, arg } => {
                let f = &self.f.def().funcs[func.func()];
                let generics: Vec<Typeval> = f.generics.iter().map(|&t| self.resolve(t)).collect();
                call(
                    &self.f.func(f.id).unwrap(),
                    &generics,
                    self.get(arg).clone(),
                )
            }
            &Expr::If { cond, then, els } => {
                if self.get(cond).bool() {
                    self.block(then, Val::Unit).clone()
                } else {
                    self.block(els, Val::Unit).clone()
                }
            }
            &Expr::For { index, body } => {
                let n = match self.resolve(index) {
                    Typeval::Fin { size } => size,
                    _ => unreachable!(),
                };
                let v: Vec<Val> = (0..n)
                    .map(|i| self.block(body, Val::Fin(i)).clone())
                    .collect();
                Val::Array(Rc::new(v))
            }
            &Expr::Accum { var, vector, body } => {
                let x = self.resolve(vector).zero();
                let y = self.block(body, x.clone()).clone();
                self.vars[var.var()] = Some(x.immut());
                y
            }

            &Expr::Add { accum, addend } => {
                self.get(accum).add(self.get(addend));
                Val::Unit
            }
        }
    }

    fn block(&mut self, b: id::Block, arg: Val) -> &Val {
        let block = &self.f.def().blocks[b.block()];
        self.vars[block.arg.var()] = Some(arg);
        for instr in &block.code {
            self.vars[instr.var.var()] = Some(self.expr(&instr.expr));
        }
        self.vars[block.ret.var()].as_ref().unwrap()
    }
}

/// Assumes `generics` and `arg` are valid.
fn call(f: &impl FuncNode, generics: &[Typeval], arg: Val) -> Val {
    Interpreter::new(f, generics)
        .block(f.def().main, arg)
        .clone()
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// Guaranteed not to panic if `f` is valid.
pub fn interp(f: &impl FuncNode, generics: &[Typeval], arg: Val) -> Result<Val, Error> {
    // TODO: check that `generics` and `arg` are valid
    Ok(call(f, generics, arg))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rose::{Block, Func, Function, Instr, TypeNode};

    enum NoTypedefs {}

    impl TypeNode for NoTypedefs {
        fn def(&self) -> &rose::Typedef {
            match *self {}
        }

        fn ty(&self, _id: id::Typedef) -> Option<Self> {
            None
        }
    }

    struct FuncInSlice<'a> {
        funcs: &'a [Function],
        id: id::Function,
    }

    impl FuncNode for FuncInSlice<'_> {
        type Ty = NoTypedefs;

        fn def(&self) -> &Function {
            &self.funcs[self.id.function()]
        }

        fn ty(&self, _id: id::Typedef) -> Option<Self::Ty> {
            None
        }

        fn func(&self, id: id::Function) -> Option<Self> {
            Some(Self {
                funcs: self.funcs,
                id,
            })
        }
    }

    #[test]
    fn test_two_plus_two() {
        let funcs = vec![Function {
            generics: vec![],
            types: vec![Typexpr::Tuple {
                members: vec![Type::F64, Type::F64],
            }],
            funcs: vec![],
            param: Type::Expr { id: id::typexpr(0) },
            ret: Type::F64,
            vars: vec![
                Type::Expr { id: id::typexpr(0) },
                Type::F64,
                Type::F64,
                Type::F64,
            ],
            blocks: vec![Block {
                arg: id::var(0),
                code: vec![
                    Instr {
                        var: id::var(1),
                        expr: Expr::Member {
                            tuple: id::var(0),
                            member: id::member(0),
                        },
                    },
                    Instr {
                        var: id::var(2),
                        expr: Expr::Member {
                            tuple: id::var(0),
                            member: id::member(1),
                        },
                    },
                    Instr {
                        var: id::var(3),
                        expr: Expr::Binary {
                            op: Binop::Add,
                            left: id::var(1),
                            right: id::var(2),
                        },
                    },
                ],
                ret: id::var(3),
            }],
            main: id::block(0),
        }];
        let answer = interp(
            &FuncInSlice {
                funcs: &funcs,
                id: id::function(0),
            },
            &[],
            Val::Tuple(Rc::new(vec![Val::F64(2.), Val::F64(2.)])),
        )
        .unwrap();
        assert_eq!(answer, Val::F64(4.));
    }

    #[test]
    fn test_nested_call() {
        let funcs = vec![
            Function {
                generics: vec![],
                types: vec![],
                funcs: vec![],
                param: Type::Unit,
                ret: Type::F64,
                vars: vec![Type::Unit, Type::F64],
                blocks: vec![Block {
                    arg: id::var(0),
                    code: vec![Instr {
                        var: id::var(1),
                        expr: Expr::F64 { val: 42. },
                    }],
                    ret: id::var(1),
                }],
                main: id::block(0),
            },
            Function {
                generics: vec![],
                types: vec![],
                funcs: vec![Func {
                    id: id::function(0),
                    generics: vec![],
                }],
                param: Type::Unit,
                ret: Type::F64,
                vars: vec![Type::Unit, Type::F64, Type::F64],
                blocks: vec![Block {
                    arg: id::var(0),
                    code: vec![
                        Instr {
                            var: id::var(1),
                            expr: Expr::Call {
                                func: id::func(0),
                                arg: id::var(0),
                            },
                        },
                        Instr {
                            var: id::var(2),
                            expr: Expr::Binary {
                                op: Binop::Mul,
                                left: id::var(1),
                                right: id::var(1),
                            },
                        },
                    ],
                    ret: id::var(2),
                }],
                main: id::block(0),
            },
        ];
        let answer = interp(
            &FuncInSlice {
                funcs: &funcs,
                id: id::function(1),
            },
            &[],
            Val::Unit,
        )
        .unwrap();
        assert_eq!(answer, Val::F64(1764.));
    }
}
