use indexmap::IndexSet;
use rose::{id, Binop, Expr, Func, Node, Refs, Ty, Unop};
use std::{cell::Cell, convert::Infallible, rc::Rc};

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
    Ref(Rc<Val>, Option<usize>),
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

    fn fin(&self) -> usize {
        match self {
            &Val::Fin(i) => i,
            _ => unreachable!(),
        }
    }

    fn get(&self, i: usize) -> &Self {
        match self {
            Val::Array(x) => &x[i],
            Val::Tuple(x) => &x[i],
            _ => unreachable!(),
        }
    }

    fn slice(&self, i: usize) -> Self {
        match self {
            Val::Ref(x, None) => Val::Ref(Rc::clone(x), Some(i)),
            Val::Ref(x, Some(j)) => Val::Ref(Rc::new(x.get(*j).clone()), Some(i)),
            _ => unreachable!(),
        }
    }

    fn inner(&self) -> &Self {
        match self {
            Val::Ref(x, i) => match i {
                None => x.as_ref(),
                &Some(j) => x.get(j),
            },
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
            Self::Ref(..) => unreachable!(),
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
fn resolve(typemap: &mut IndexSet<Ty>, generics: &[id::Ty], types: &[id::Ty], ty: &Ty) -> id::Ty {
    let resolved = match ty {
        Ty::Generic { id } => return generics[id.generic()],

        Ty::Unit => Ty::Unit,
        Ty::Bool => Ty::Bool,
        Ty::F64 => Ty::F64,
        &Ty::Fin { size } => Ty::Fin { size },

        Ty::Ref { inner } => Ty::Ref {
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

/// An opaque function that can be called by the interpreter.
pub trait Opaque {
    fn call(&self, types: &IndexSet<Ty>, generics: &[id::Ty], args: &[Val]) -> Val;
}

impl Opaque for Infallible {
    fn call(&self, _: &IndexSet<Ty>, _: &[id::Ty], _: &[Val]) -> Val {
        match *self {}
    }
}

/// basically, the `'a` lifetime is for the graph of functions, and the `'b` lifetime is just for
/// this particular instance of interpretation
struct Interpreter<'a, 'b, O, T: Refs<'a, Opaque = O>> {
    typemap: &'b mut IndexSet<Ty>,
    refs: T,
    def: &'a Func,
    types: Vec<id::Ty>,
    vars: Vec<Option<Val>>,
}

impl<'a, 'b, O: Opaque, T: Refs<'a, Opaque = O>> Interpreter<'a, 'b, O, T> {
    fn new(typemap: &'b mut IndexSet<Ty>, refs: T, def: &'a Func, generics: &'b [id::Ty]) -> Self {
        let mut types = vec![];
        for ty in def.types.iter() {
            types.push(resolve(typemap, generics, &types, ty));
        }
        Self {
            typemap,
            refs,
            def,
            types,
            vars: vec![None; def.vars.len()],
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

            &Expr::Slice { array, index } => self.get(array).slice(self.get(index).fin()),
            &Expr::Field { tuple, member } => self.get(tuple).slice(member.member()),

            &Expr::Unary { op, arg } => {
                let x = self.get(arg);
                match op {
                    Unop::Not => Val::Bool(!x.bool()),

                    Unop::IMod => {
                        let n = match self.typemap[self.types[self.def.vars[arg.var()].ty()].ty()] {
                            Ty::Fin { size } => size,
                            _ => unreachable!(),
                        };
                        Val::Fin(x.fin() % n)
                    }

                    Unop::Neg => val_f64(-x.f64()),
                    Unop::Abs => val_f64(x.f64().abs()),
                    Unop::Sign => val_f64(x.f64().signum()),
                    Unop::Ceil => val_f64(x.f64().ceil()),
                    Unop::Floor => val_f64(x.f64().floor()),
                    Unop::Trunc => val_f64(x.f64().trunc()),
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

                    Binop::INeq => Val::Bool(x.fin() != y.fin()),
                    Binop::ILt => Val::Bool(x.fin() < y.fin()),
                    Binop::ILeq => Val::Bool(x.fin() <= y.fin()),
                    Binop::IEq => Val::Bool(x.fin() == y.fin()),
                    Binop::IGt => Val::Bool(x.fin() > y.fin()),
                    Binop::IGeq => Val::Bool(x.fin() >= y.fin()),

                    Binop::IAdd => Val::Fin(x.fin() + y.fin()),

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
                call(self.refs.get(*id).unwrap(), self.typemap, &resolved, vals)
            }
            Expr::For { arg, body, ret } => {
                let n = match self.typemap[self.types[self.def.vars[arg.var()].ty()].ty()] {
                    Ty::Fin { size } => size,
                    _ => unreachable!(),
                };
                Val::Array(collect_vals(
                    (0..n).map(|i| self.block(*arg, body, *ret, Val::Fin(i)).clone()),
                ))
            }

            &Expr::Accum { shape } => Val::Ref(Rc::new(self.get(shape).zero()), None),

            &Expr::Add { accum, addend } => {
                self.get(accum).inner().add(self.get(addend));
                Val::Unit
            }

            &Expr::Resolve { var } => self.get(var).inner().clone(),
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
fn call<'a, 'b, O: Opaque, T: Refs<'a, Opaque = O>>(
    f: Node<'a, O, T>,
    types: &'b mut IndexSet<Ty>,
    generics: &'b [id::Ty],
    args: impl Iterator<Item = Val>,
) -> Val {
    match f {
        Node::Transparent { refs, def } => {
            let mut interp = Interpreter::new(types, refs, def, generics);
            for (var, arg) in def.params.iter().zip(args) {
                interp.vars[var.var()] = Some(arg.clone());
            }
            for instr in def.body.iter() {
                interp.vars[instr.var.var()] = Some(interp.expr(&instr.expr));
            }
            interp.vars[def.ret.var()].as_ref().unwrap().clone()
        }
        Node::Opaque {
            generics: _,
            types: _,
            params: _,
            ret: _,
            def,
        } => {
            let vals: Box<[Val]> = args.collect();
            def.call(types, generics, &vals)
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// Guaranteed not to panic if `f` is valid.
pub fn interp<'a, O: Opaque, T: Refs<'a, Opaque = O>>(
    f: Node<'a, O, T>,
    mut types: IndexSet<Ty>,
    generics: &'a [id::Ty],
    args: impl Iterator<Item = Val>,
) -> Result<Val, Error> {
    // TODO: check that `generics` and `arg` are valid
    Ok(call(f, &mut types, generics, args))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rose::{Func, Instr};

    type CustomRef<'a> = &'a dyn Fn(&IndexSet<Ty>, &[id::Ty], &[Val]) -> Val;
    type CustomBox = Box<dyn Fn(&IndexSet<Ty>, &[id::Ty], &[Val]) -> Val>;

    struct Custom<'a> {
        f: CustomRef<'a>,
    }

    impl Opaque for Custom<'_> {
        fn call(&self, types: &IndexSet<Ty>, generics: &[id::Ty], args: &[Val]) -> Val {
            (self.f)(types, generics, args)
        }
    }

    struct FuncInSlice<'a> {
        custom: &'a [CustomBox],
        funcs: &'a [Func],
        id: id::Func,
    }

    impl<'a> Refs<'a> for FuncInSlice<'a> {
        type Opaque = Custom<'a>;

        fn get(&self, id: id::Func) -> Option<Node<'a, Custom<'a>, Self>> {
            if id.func() < self.id.func() {
                node(self.custom, self.funcs, id)
            } else {
                None
            }
        }
    }

    fn node<'a>(
        custom: &'a [CustomBox],
        funcs: &'a [Func],
        id: id::Func,
    ) -> Option<Node<'a, Custom<'a>, FuncInSlice<'a>>> {
        let n = custom.len();
        let i = id.func();
        if i < n {
            Some(Node::Opaque {
                generics: &[],
                types: &[],
                params: &[],
                ret: id::ty(0),
                def: Custom { f: &custom[i] },
            })
        } else {
            funcs.get(i - n).map(|def| Node::Transparent {
                refs: FuncInSlice { custom, funcs, id },
                def,
            })
        }
    }

    #[test]
    fn test_two_plus_two() {
        let funcs = vec![Func {
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
            node(&[], &funcs, id::func(0)).unwrap(),
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
            Func {
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
            Func {
                generics: vec![].into(),
                types: vec![Ty::F64].into(),
                vars: vec![id::ty(0), id::ty(0)].into(),
                params: vec![].into(),
                ret: id::var(1),
                body: vec![
                    Instr {
                        var: id::var(0),
                        expr: Expr::Call {
                            id: id::func(0),
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
            node(&[], &funcs, id::func(1)).unwrap(),
            IndexSet::new(),
            &[],
            [].into_iter(),
        )
        .unwrap();
        assert_eq!(answer, val_f64(1764.));
    }

    #[test]
    fn test_custom() {
        let custom: [CustomBox; 1] = [Box::new(|_, _, args| {
            Val::F64(Cell::new(args[0].f64().powf(args[1].f64())))
        })];
        let funcs = [Func {
            generics: [].into(),
            types: [Ty::F64].into(),
            vars: [id::ty(0), id::ty(0), id::ty(0)].into(),
            params: [id::var(0), id::var(1)].into(),
            ret: id::var(2),
            body: [Instr {
                var: id::var(2),
                expr: Expr::Call {
                    id: id::func(0),
                    generics: [].into(),
                    args: [id::var(0), id::var(1)].into(),
                },
            }]
            .into(),
        }];
        let answer = interp(
            node(&custom, &funcs, id::func(1)).unwrap(),
            IndexSet::new(),
            &[],
            [val_f64(std::f64::consts::E), val_f64(std::f64::consts::PI)].into_iter(),
        )
        .unwrap();
        assert_eq!(answer, val_f64(23.140692632779263));
    }
}
