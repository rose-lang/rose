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
    F64(Cell<f64>),
    Fin(usize),
    Ref(Rc<Val>),
    Array(Vals), // assume all indices are `Fin`
    Tuple(Vals),
}

pub type Vals = Rc<Vec<Val>>; // TODO: change to `Rc<[Val]>` https://github.com/rose-lang/rose/issues/63

pub fn vals<const N: usize>(v: [Val; N]) -> Vals {
    Rc::new(v.to_vec())
}

pub fn collect_vals(it: impl Iterator<Item = Val>) -> Vals {
    Rc::new(it.collect())
}

pub fn val_f64(x: f64) -> Val {
    Val::F64(Cell::new(x))
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
            Val::F64(x) => x.get(),
            _ => unreachable!(),
        }
    }

    fn inner(&self) -> &Self {
        match self {
            Val::Ref(x) => x.as_ref(),
            _ => unreachable!(),
        }
    }

    /// Return a zero value with this value's topology.
    fn zero(&self) -> Self {
        match self {
            Self::Unit => Self::Unit,
            &Self::Bool(x) => Self::Bool(x),
            Self::F64(_) => Self::F64(Cell::new(0.)),
            &Self::Fin(x) => Self::Fin(x),
            Self::Ref(x) => Self::Ref(Rc::clone(x)),
            Self::Array(x) => Self::Array(collect_vals(x.iter().map(|x| x.zero()))),
            Self::Tuple(x) => Self::Tuple(collect_vals(x.iter().map(|x| x.zero()))),
        }
    }

    /// Add `x` to this value, which must represent a mutable `Ref` type.
    fn add(&self, x: &Self) {
        match (self, x) {
            (Self::Unit, Self::Unit)
            | (Self::Bool(_), Self::Bool(_))
            | (Self::Fin(_), Self::Fin(_))
            | (Self::Ref(_), Self::Ref(_)) => {}
            (Self::F64(a), Self::F64(b)) => a.set(a.get() + b.get()),
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
            &Expr::F64 { val } => val_f64(val),
            &Expr::Fin { val } => Val::Fin(val),

            Expr::Array { elems } => {
                Val::Array(collect_vals(elems.iter().map(|&x| self.get(x).clone())))
            }
            Expr::Tuple { members } => {
                Val::Tuple(collect_vals(members.iter().map(|&x| self.get(x).clone())))
            }

            &Expr::Index { array, index } => match (self.get(array), self.get(index)) {
                (Val::Array(v), &Val::Fin(i)) => v[i].clone(),
                _ => unreachable!(),
            },
            &Expr::Member { tuple, member } => match self.get(tuple) {
                Val::Tuple(x) => x[member.member()].clone(),
                _ => unreachable!(),
            },

            &Expr::Slice { array, index } => match (self.get(array).inner(), self.get(index)) {
                (Val::Array(v), &Val::Fin(i)) => v[i].clone(),
                _ => unreachable!(),
            },
            &Expr::Field { tuple, field } => match self.get(tuple).inner() {
                Val::Tuple(x) => x[field.member()].clone(),
                _ => unreachable!(),
            },

            &Expr::Unary { op, arg } => {
                let x = self.get(arg);
                match op {
                    Unop::Not => Val::Bool(!x.bool()),

                    Unop::Neg => val_f64(-x.f64()),
                    Unop::Abs => val_f64(x.f64().abs()),
                    Unop::Sqrt => val_f64(x.f64().sqrt()),
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

                    Binop::Add => val_f64(x.f64() + y.f64()),
                    Binop::Sub => val_f64(x.f64() - y.f64()),
                    Binop::Mul => val_f64(x.f64() * y.f64()),
                    Binop::Div => val_f64(x.f64() / y.f64()),
                }
            }
            &Expr::Select { cond, then, els } => {
                if self.get(cond).bool() {
                    self.get(then).clone()
                } else {
                    self.get(els).clone()
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
            &Expr::For { index, body } => {
                let n = match self.typemap[self.types[index.ty()].ty()] {
                    Ty::Fin { size } => size,
                    _ => unreachable!(),
                };
                Val::Array(collect_vals(
                    (0..n).map(|i| self.block(body, Val::Fin(i)).clone()),
                ))
            }
            &Expr::Read { var, body } => {
                let r = Val::Ref(Rc::new(self.get(var).clone()));
                let x = self.block(body, r).clone();
                x
            }
            &Expr::Accum { var, shape, body } => {
                let x = Val::Ref(Rc::new(self.get(shape).zero()));
                let y = self.block(body, x.clone()).clone();
                self.vars[var.var()] = Some(x.inner().clone());
                y
            }

            &Expr::Ask { var } => self.get(var).inner().clone(),
            &Expr::Add { accum, addend } => {
                self.get(accum).inner().add(self.get(addend));
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
    use enumset::EnumSet;
    use rose::{Block, Constraint, Func, Function, Instr};

    struct FuncBuilder {
        types: Vec<Ty>,
        vars: Vec<id::Ty>,
        blocks: Vec<Option<Block>>,
    }

    impl FuncBuilder {
        fn new() -> Self {
            Self {
                types: vec![],
                vars: vec![],
                blocks: vec![],
            }
        }

        fn ty(&mut self, ty: Ty) -> id::Ty {
            let i = self.types.len();
            self.types.push(ty);
            id::ty(i)
        }

        fn var(&mut self, var: id::Ty) -> id::Var {
            let i = self.vars.len();
            self.vars.push(var);
            id::var(i)
        }

        fn block(&mut self, block: impl FnOnce(BlockBuilder) -> Block) -> id::Block {
            let i = self.blocks.len();
            let id = id::block(i);
            self.blocks.push(None);
            let b = block(BlockBuilder {
                f: self,
                id,
                code: vec![],
            });
            self.blocks[i] = Some(b);
            id
        }

        fn done(
            self,
            generics: Vec<EnumSet<Constraint>>,
            funcs: Vec<Func>,
            main: id::Block,
        ) -> Function {
            let b = self.blocks[main.block()].as_ref().unwrap();
            Function {
                generics,
                types: self.types,
                funcs,
                param: self.vars[b.arg.var()],
                ret: self.vars[b.ret.var()],
                vars: self.vars,
                blocks: self.blocks.into_iter().map(|b| b.unwrap()).collect(),
                main,
            }
        }
    }

    struct BlockBuilder<'a> {
        f: &'a mut FuncBuilder,
        id: id::Block,
        code: Vec<Instr>,
    }

    impl BlockBuilder<'_> {
        fn ty(&mut self, ty: Ty) -> id::Ty {
            self.f.ty(ty)
        }

        fn var(&mut self, var: id::Ty) -> id::Var {
            self.f.var(var)
        }

        fn block(&mut self, block: impl FnOnce(BlockBuilder) -> Block) -> id::Block {
            self.f.block(block)
        }

        fn instr(&mut self, ty: id::Ty, expr: Expr) -> id::Var {
            let var = self.var(ty);
            self.code.push(Instr { var, expr });
            var
        }

        fn done(self, arg: id::Var, ret: id::Var) -> Block {
            Block {
                arg,
                code: self.code,
                ret,
            }
        }
    }

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
            Val::Tuple(vals([val_f64(2.), val_f64(2.)])),
        )
        .unwrap();
        assert_eq!(answer, val_f64(4.));
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
        assert_eq!(answer, val_f64(1764.));
    }

    #[test]
    fn test_nested_ref() {
        let mut f = FuncBuilder::new();

        let ty_unit = f.ty(Ty::Unit);
        let ty_f64 = f.ty(Ty::F64);
        let ty_vec2 = f.ty(Ty::Tuple {
            members: vec![ty_f64, ty_f64],
        });

        let main = f.block(|mut b| {
            let arg = b.var(ty_unit);

            let forty_two = b.instr(ty_f64, Expr::F64 { val: 42. });

            let outer = b.block(|mut b| {
                let h1 = b.ty(Ty::Scope { id: b.id });
                let ty_r1 = b.ty(Ty::Ref {
                    scope: h1,
                    inner: ty_f64,
                });
                let r1 = b.var(ty_r1);

                let ty_tup = b.ty(Ty::Tuple {
                    members: vec![ty_f64, ty_r1],
                });
                let tup = b.instr(
                    ty_tup,
                    Expr::Tuple {
                        members: vec![forty_two, r1],
                    },
                );

                let inner = b.block(|mut b| {
                    let h2 = b.ty(Ty::Scope { id: b.id });
                    let ty_r2 = b.ty(Ty::Ref {
                        scope: h2,
                        inner: ty_tup,
                    });
                    let r2 = b.var(ty_r2);

                    let unit = b.instr(
                        ty_unit,
                        Expr::Add {
                            accum: r2,
                            addend: tup,
                        },
                    );
                    b.done(r2, unit)
                });
                let accumed = b.var(ty_tup);
                b.instr(
                    ty_unit,
                    Expr::Accum {
                        var: accumed,
                        shape: tup,
                        body: inner,
                    },
                );

                let first = b.instr(
                    ty_f64,
                    Expr::Member {
                        tuple: accumed,
                        member: id::member(0),
                    },
                );
                b.done(r1, first)
            });
            let y = b.var(ty_f64);
            let x = b.instr(
                ty_f64,
                Expr::Accum {
                    var: y,
                    shape: forty_two,
                    body: outer,
                },
            );

            let ret = b.instr(
                ty_vec2,
                Expr::Tuple {
                    members: vec![x, y],
                },
            );
            b.done(arg, ret)
        });

        let funcs = vec![f.done(vec![], vec![], main)];

        let answer = interp(
            FuncInSlice {
                funcs: &funcs,
                id: id::function(0),
            },
            IndexSet::new(),
            &[],
            Val::Unit,
        )
        .unwrap();
        assert_eq!(answer, Val::Tuple(vals([val_f64(42.), val_f64(0.)])));
    }
}
