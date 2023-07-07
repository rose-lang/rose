use indexmap::IndexSet;
use rose::{id, Binop, Expr, FuncNode, Ty, Unop};
use std::{cell::Cell, rc::Rc};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

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

/// Return zero a value of `Ref` type for this type, which must satisfy `Constraint::Vector`.
fn zero(types: &IndexSet<Ty>, ty: id::Ty) -> Val {
    match &types[ty.ty()] {
        Ty::F64 => Val::Ref(Rc::new(Cell::new(0.))),
        &Ty::Array { index, elem } => match types[index.ty()] {
            Ty::Fin { size } => Val::Array(Rc::new((0..size).map(|_| zero(types, elem)).collect())),
            _ => unreachable!(),
        },
        Ty::Tuple { members } => {
            Val::Tuple(Rc::new(members.iter().map(|&x| zero(types, x)).collect()))
        }
        Ty::Unit
        | Ty::Bool
        | Ty::Fin { .. }
        | Ty::Generic { .. }
        | Ty::Scope { .. }
        | Ty::Ref { .. } => unreachable!(),
    }
}

/// Resolve `ty` via `generics` and `types`, then return its ID in `typemap`, inserting if need be.
///
/// This is meant to be used to pull all the types from a callee into a broader context. The
/// `generics` are the IDs of all the types provided as generic type parameters for the callee. The
/// `types are the IDs of all the types that have been pulled in so far.
///
/// The interpreter is meant to be used with no generic "free variables," and does not do any scope
/// checking, so all scopes are replaced with a block ID of zero.
fn resolve(typemap: &mut IndexSet<Ty>, generics: &[id::Ty], types: &[id::Ty], ty: &Ty) -> id::Ty {
    let resolved = match ty {
        Ty::Generic { id } => return generics[id.generic()],

        Ty::Unit => Ty::Unit,
        Ty::Bool => Ty::Bool,
        Ty::F64 => Ty::F64,
        &Ty::Fin { size } => Ty::Fin { size },

        Ty::Scope { id: _ } => Ty::Scope { id: id::block(0) }, // we erase scope info
        Ty::Ref { scope, inner } => Ty::Ref {
            scope: types[scope.ty()],
            inner: types[inner.ty()],
        },
        Ty::Array { index, elem } => Ty::Array {
            index: types[index.ty()],
            elem: types[elem.ty()],
        },
        Ty::Tuple { members } => Ty::Tuple {
            members: members.iter().map(|&x| types[x.ty()]).collect(),
        },
    };
    let (i, _) = typemap.insert_full(resolved);
    id::ty(i)
}

struct Interpreter<'a, F: FuncNode> {
    typemap: &'a mut IndexSet<Ty>,
    f: &'a F, // reference instead of value because otherwise borrow checker complains in `fn block`
    types: Vec<id::Ty>,
    vars: Vec<Option<Val>>,
}

impl<'a, F: FuncNode> Interpreter<'a, F> {
    fn new(typemap: &'a mut IndexSet<Ty>, f: &'a F, generics: &'a [id::Ty]) -> Self {
        let mut types = vec![];
        for ty in &f.def().types {
            types.push(resolve(typemap, generics, &types, ty));
        }
        Self {
            typemap,
            f,
            types,
            vars: vec![None; f.def().vars.len()],
        }
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
            &Expr::Field { tuple, field } => match self.get(tuple) {
                Val::Tuple(x) => x[field.member()].clone(),
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
                let generics: Vec<id::Ty> =
                    f.generics.iter().map(|id| self.types[id.ty()]).collect();
                call(
                    self.f.get(f.id).unwrap(),
                    self.typemap,
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
                let n = match self.typemap[self.types[index.ty()].ty()] {
                    Ty::Fin { size } => size,
                    _ => unreachable!(),
                };
                let v: Vec<Val> = (0..n)
                    .map(|i| self.block(body, Val::Fin(i)).clone())
                    .collect();
                Val::Array(Rc::new(v))
            }
            &Expr::Accum { var, vector, body } => {
                let x = zero(self.typemap, self.types[vector.ty()]);
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
fn call(f: impl FuncNode, types: &mut IndexSet<Ty>, generics: &[id::Ty], arg: Val) -> Val {
    Interpreter::new(types, &f, generics)
        .block(f.def().main, arg)
        .clone()
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// Guaranteed not to panic if `f` is valid.
pub fn interp(
    f: impl FuncNode,
    mut types: IndexSet<Ty>,
    generics: &[id::Ty],
    arg: Val,
) -> Result<Val, Error> {
    // TODO: check that `generics` and `arg` are valid
    Ok(call(f, &mut types, generics, arg))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rose::{Block, Func, Function, Instr};

    #[derive(Clone, Copy, Debug)]
    struct FuncInSlice<'a> {
        funcs: &'a [Function],
        id: id::Function,
    }

    impl FuncNode for FuncInSlice<'_> {
        fn def(&self) -> &Function {
            &self.funcs[self.id.function()]
        }

        fn get(&self, id: id::Function) -> Option<Self> {
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
            types: vec![
                Ty::F64,
                Ty::Tuple {
                    members: vec![id::ty(0), id::ty(0)],
                },
            ],
            funcs: vec![],
            param: id::ty(1),
            ret: id::ty(0),
            vars: vec![id::ty(1), id::ty(0), id::ty(0), id::ty(0)],
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
            FuncInSlice {
                funcs: &funcs,
                id: id::function(0),
            },
            IndexSet::new(),
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
                types: vec![Ty::Unit, Ty::F64],
                funcs: vec![],
                param: id::ty(0),
                ret: id::ty(1),
                vars: vec![id::ty(0), id::ty(1)],
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
                types: vec![Ty::Unit, Ty::F64],
                funcs: vec![Func {
                    id: id::function(0),
                    generics: vec![],
                }],
                param: id::ty(0),
                ret: id::ty(1),
                vars: vec![id::ty(0), id::ty(1), id::ty(1)],
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
            FuncInSlice {
                funcs: &funcs,
                id: id::function(1),
            },
            IndexSet::new(),
            &[],
            Val::Unit,
        )
        .unwrap();
        assert_eq!(answer, Val::F64(1764.));
    }
}
