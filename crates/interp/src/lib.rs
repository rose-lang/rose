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
            Self::Ref(_) => unreachable!(),
            Self::Array(x) => Self::Array(collect_vals(x.iter().map(|x| x.zero()))),
            Self::Tuple(x) => Self::Tuple(collect_vals(x.iter().map(|x| x.zero()))),
        }
    }

    /// Add `x` to this value, which must represent a mutable `Ref` type.
    fn add(&self, x: &Self) {
        match (self, x) {
            (Self::Unit, Self::Unit)
            | (Self::Bool(_), Self::Bool(_))
            | (Self::Fin(_), Self::Fin(_)) => {}
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

        &Ty::Scope { kind, id: _ } => Ty::Scope {
            kind,
            id: id::var(usize::MAX), // we erase scope info
        },
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
        for ty in f.def().types.iter() {
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
            &Expr::Field { tuple, member } => match self.get(tuple).inner() {
                Val::Tuple(x) => x[member.member()].clone(),
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

            Expr::Call { id, generics, args } => {
                let resolved: Vec<id::Ty> = generics.iter().map(|id| self.types[id.ty()]).collect();
                let vals = args.iter().map(|id| self.vars[id.var()].clone().unwrap());
                call(self.f.get(*id).unwrap(), self.typemap, &resolved, vals)
            }
            Expr::For {
                index,
                arg,
                body,
                ret,
            } => {
                let n = match self.typemap[self.types[index.ty()].ty()] {
                    Ty::Fin { size } => size,
                    _ => unreachable!(),
                };
                Val::Array(collect_vals(
                    (0..n).map(|i| self.block(*arg, body, *ret, Val::Fin(i)).clone()),
                ))
            }
            Expr::Read {
                var,
                arg,
                body,
                ret,
            } => {
                let r = Val::Ref(Rc::new(self.get(*var).clone()));
                self.block(*arg, body, *ret, r);
                Val::Unit
            }
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => {
                let x = Val::Ref(Rc::new(self.get(*shape).zero()));
                self.block(*arg, body, *ret, x.clone());
                x.inner().clone()
            }

            &Expr::Ask { var } => self.get(var).inner().clone(),
            &Expr::Add { accum, addend } => {
                self.get(accum).inner().add(self.get(addend));
                Val::Unit
            }
        }
    }

    fn block(&mut self, param: id::Var, body: &[rose::Instr], ret: id::Var, arg: Val) -> &Val {
        self.vars[param.var()] = Some(arg);
        for instr in body.iter() {
            self.vars[instr.var.var()] = Some(self.expr(&instr.expr));
        }
        self.vars[ret.var()].as_ref().unwrap()
    }
}

/// Assumes `generics` and `arg` are valid.
fn call(
    f: impl FuncNode,
    types: &mut IndexSet<Ty>,
    generics: &[id::Ty],
    args: impl Iterator<Item = Val>,
) -> Val {
    let mut interp = Interpreter::new(types, &f, generics);
    for (var, arg) in f.def().params.iter().zip(args) {
        interp.vars[var.var()] = Some(arg.clone());
    }
    for instr in f.def().body.iter() {
        interp.vars[instr.var.var()] = Some(interp.expr(&instr.expr));
    }
    interp.vars[f.def().ret.var()].as_ref().unwrap().clone()
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// Guaranteed not to panic if `f` is valid.
pub fn interp(
    f: impl FuncNode,
    mut types: IndexSet<Ty>,
    generics: &[id::Ty],
    args: impl Iterator<Item = Val>,
) -> Result<Val, Error> {
    // TODO: check that `generics` and `arg` are valid
    Ok(call(f, &mut types, generics, args))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rose::{Function, Instr};

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
            generics: vec![].into(),
            types: vec![Ty::F64].into(),
            vars: vec![id::ty(0), id::ty(0), id::ty(0)].into(),
            params: vec![id::var(0), id::var(1)].into(),
            ret: id::var(2),
            body: vec![Instr {
                var: id::var(2),
                expr: Expr::Binary {
                    op: Binop::Add,
                    left: id::var(0),
                    right: id::var(1),
                },
            }]
            .into(),
        }];
        let answer = interp(
            FuncInSlice {
                funcs: &funcs,
                id: id::function(0),
            },
            IndexSet::new(),
            &[],
            [val_f64(2.), val_f64(2.)].into_iter(),
        )
        .unwrap();
        assert_eq!(answer, val_f64(4.));
    }

    #[test]
    fn test_nested_call() {
        let funcs = vec![
            Function {
                generics: vec![].into(),
                types: vec![Ty::F64].into(),
                vars: vec![id::ty(0)].into(),
                params: vec![].into(),
                ret: id::var(0),
                body: vec![Instr {
                    var: id::var(0),
                    expr: Expr::F64 { val: 42. },
                }]
                .into(),
            },
            Function {
                generics: vec![].into(),
                types: vec![Ty::F64].into(),
                vars: vec![id::ty(0), id::ty(0)].into(),
                params: vec![].into(),
                ret: id::var(1),
                body: vec![
                    Instr {
                        var: id::var(0),
                        expr: Expr::Call {
                            id: id::function(0),
                            generics: vec![].into(),
                            args: vec![].into(),
                        },
                    },
                    Instr {
                        var: id::var(1),
                        expr: Expr::Binary {
                            op: Binop::Mul,
                            left: id::var(0),
                            right: id::var(0),
                        },
                    },
                ]
                .into(),
            },
        ];
        let answer = interp(
            FuncInSlice {
                funcs: &funcs,
                id: id::function(1),
            },
            IndexSet::new(),
            &[],
            [].into_iter(),
        )
        .unwrap();
        assert_eq!(answer, val_f64(1764.));
    }
}
