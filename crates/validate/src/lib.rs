use enumset::EnumSet;
use indexmap::IndexMap;
use rose::{id, Binop, Constraint, Expr, FuncNode, Function, Instr, Ty, Unop};

#[derive(Debug, thiserror::Error)]
pub enum InstrError {
    #[error("variable ID is out of range")]
    InvalidVar,

    #[error("variable was already declared")]
    Redeclare,

    #[error("primitive expression does not match its type")]
    InvalidPrimitive,

    #[error("type is not an array")]
    NotArray,

    #[error("array has the wrong number of elements")]
    InvalidArray,

    #[error("array element {0} does not match its type")]
    InvalidElem(usize),

    #[error("type is not a tuple")]
    NotTuple,

    #[error("tuple has the wrong number of members")]
    InvalidTuple,

    #[error("tuple member {} does not match its type", .0.member())]
    InvalidMember(id::Member),

    #[error("array type does not match index and element types")]
    InvalidIndex,

    #[error("wrong number of generics")]
    GenericsCount,

    #[error("wrong number of arguments")]
    ArgsCount,

    #[error("type error")]
    TypeError, // TODO
}

struct Validator<'a, F: FuncNode> {
    node: &'a F,
    f: &'a Function,
    constraints: IndexMap<Ty, EnumSet<Constraint>>,
    /// indices from `self.f.types` into `self.constraints`
    types: Vec<id::Ty>,
    /// same length as `self.f.vars`
    declared: Vec<bool>,
}

impl<F: FuncNode> Validator<'_, F> {
    fn ty(&self, t: id::Ty) -> &Ty {
        let (ty, _) = self.constraints.get_index(t.ty()).unwrap();
        ty
    }

    fn constr(&self, t: id::Ty) -> EnumSet<Constraint> {
        let (_, &constrs) = self.constraints.get_index(t.ty()).unwrap();
        constrs
    }

    fn var_ty_id(&self, x: id::Var) -> id::Ty {
        self.types[self.f.vars[x.var()].ty()]
    }

    fn var_ty(&self, x: id::Var) -> &Ty {
        self.ty(self.var_ty_id(x))
    }

    fn resolve(
        &mut self,
        generics: &[id::Ty],
        types: &[Option<id::Ty>],
        ty: &rose::Ty,
    ) -> Option<id::Ty> {
        let (deduped, constrs) = match ty {
            // inner scopes can't be in the param or return types, which are all we care about here
            rose::Ty::Scope { kind: _, id: _ } => return None,
            rose::Ty::Generic { id } => return Some(self.types[generics[id.generic()].ty()]),

            rose::Ty::Unit => (rose::Ty::Unit, EnumSet::only(Constraint::Value)),
            rose::Ty::Bool => (rose::Ty::Bool, EnumSet::only(Constraint::Value)),
            rose::Ty::F64 => (rose::Ty::F64, EnumSet::only(Constraint::Value)),
            &rose::Ty::Fin { size } => (
                rose::Ty::Fin { size },
                Constraint::Value | Constraint::Index,
            ),

            rose::Ty::Ref { scope, inner } => (
                rose::Ty::Ref {
                    scope: types[scope.ty()]?,
                    inner: types[inner.ty()]?,
                },
                EnumSet::empty(),
            ),
            rose::Ty::Array { index, elem } => (
                rose::Ty::Array {
                    index: types[index.ty()]?,
                    elem: types[elem.ty()]?,
                },
                EnumSet::only(Constraint::Value),
            ),
            rose::Ty::Tuple { members } => (
                rose::Ty::Tuple {
                    members: members
                        .iter()
                        .map(|&x| types[x.ty()])
                        .collect::<Option<_>>()?,
                },
                EnumSet::only(Constraint::Value),
            ),
        };
        let (i, _) = self.constraints.insert_full(deduped, constrs);
        Some(id::ty(i))
    }

    fn instr(&mut self, instr: &Instr) -> Result<(), InstrError> {
        let Instr { var, expr } = instr;
        if var.var() >= self.f.vars.len() {
            return Err(InstrError::InvalidVar);
        } else if self.declared[var.var()] {
            return Err(InstrError::Redeclare);
        }
        self.declared[var.var()] = true;
        let check = |p: bool| -> Result<(), InstrError> {
            if p {
                Ok(())
            } else {
                Err(InstrError::TypeError)
            }
        };
        let t = self.var_ty_id(*var);
        let ty = self.ty(t);

        match expr {
            Expr::Unit => check(*ty == Ty::Unit)?,
            Expr::Bool { .. } => check(*ty == Ty::Bool)?,
            Expr::F64 { .. } => check(*ty == Ty::F64)?,
            Expr::Fin { val } => check(matches!(ty, Ty::Fin { size } if val < size))?,

            Expr::Array { elems } => match ty {
                &Ty::Array { index, elem } => {
                    match self.ty(index) {
                        &Ty::Fin { size } => {
                            if elems.len() != size {
                                return Err(InstrError::InvalidArray);
                            }
                            for (i, &x) in elems.iter().enumerate() {
                                if self.var_ty_id(x) != elem {
                                    return Err(InstrError::InvalidElem(i));
                                }
                            }
                        }
                        _ => unreachable!(), // `Fin` is currently the only `Index` type
                    }
                }
                _ => return Err(InstrError::NotArray),
            },
            Expr::Tuple { members } => match ty {
                Ty::Tuple { members: types } => {
                    if members.len() != types.len() {
                        return Err(InstrError::InvalidTuple);
                    }
                    for (i, (&x, &xt)) in members.iter().zip(types.iter()).enumerate() {
                        if self.var_ty_id(x) != xt {
                            return Err(InstrError::InvalidMember(id::member(i)));
                        }
                    }
                }
                _ => return Err(InstrError::NotTuple),
            },

            &Expr::Index { array, index } => check(
                *self.var_ty(array)
                    == (Ty::Array {
                        index: self.var_ty_id(index),
                        elem: t,
                    }),
            )?,
            &Expr::Member { tuple, member } => match self.var_ty(tuple) {
                Ty::Tuple { members } if members.get(member.member()) == Some(&t) => {}
                _ => return Err(InstrError::TypeError),
            },

            &Expr::Slice { array, index } => match (ty, self.var_ty(array)) {
                (
                    &Ty::Ref {
                        scope: scope_elem,
                        inner: elem,
                    },
                    &Ty::Ref {
                        scope: scope_arr,
                        inner: arr,
                    },
                ) if scope_elem == scope_arr
                    && *self.ty(arr)
                        == (Ty::Array {
                            index: self.var_ty_id(index),
                            elem,
                        }) => {}
                _ => return Err(InstrError::TypeError),
            },
            &Expr::Field { tuple, member } => match (ty, self.var_ty(tuple)) {
                (
                    &Ty::Ref {
                        scope: scope_mem,
                        inner: mem,
                    },
                    &Ty::Ref {
                        scope: scope_tup,
                        inner: tup,
                    },
                ) if scope_mem == scope_tup => match self.ty(tup) {
                    Ty::Tuple { members } if members.get(member.member()) == Some(&mem) => {}
                    _ => return Err(InstrError::TypeError),
                },
                _ => return Err(InstrError::TypeError),
            },

            &Expr::Unary { op, arg } => match op {
                Unop::Not => check(*ty == Ty::Bool && self.var_ty_id(arg) == t)?,
                Unop::Neg | Unop::Abs | Unop::Sqrt => {
                    check(*ty == Ty::F64 && self.var_ty_id(arg) == t)?
                }
            },
            &Expr::Binary { op, left, right } => match op {
                Binop::And | Binop::Or | Binop::Iff | Binop::Xor => check(
                    *ty == Ty::Bool && self.var_ty_id(left) == t && self.var_ty_id(right) == t,
                )?,
                Binop::Neq | Binop::Lt | Binop::Leq | Binop::Eq | Binop::Gt | Binop::Geq => {
                    let l = self.var_ty_id(left);
                    check(*ty == Ty::Bool && *self.ty(l) == Ty::F64 && self.var_ty_id(right) == l)?
                }
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => check(
                    *ty == Ty::F64 && self.var_ty_id(left) == t && self.var_ty_id(right) == t,
                )?,
            },
            &Expr::Select { cond, then, els } => {
                let a = self.var_ty_id(then);
                let b = self.var_ty_id(els);
                check(*self.var_ty(cond) == Ty::Bool && a == b && t == a)?
            }

            Expr::Call { id, generics, args } => match self.node.get(*id) {
                Some(node) => {
                    let g = node.def();
                    if generics.len() != g.generics.len() {
                        return Err(InstrError::GenericsCount);
                    } else if args.len() != g.params.len() {
                        return Err(InstrError::ArgsCount);
                    }
                    for (i, (expected, actual)) in
                        g.generics.iter().zip(generics.iter()).enumerate()
                    {
                        match self.types.get(actual.ty()) {
                            Some(generic) => check(self.constr(*generic).is_superset(*expected))?,
                            None => return Err(InstrError::TypeError),
                        }
                    }
                    let mut types = vec![];
                    for typ in g.types.iter() {
                        let i = self.resolve(generics, &types, typ);
                        types.push(i);
                    }
                    for (i, (expected, actual)) in g.params.iter().zip(args.iter()).enumerate() {
                        check(
                            self.types[self.f.vars[actual.var()].ty()]
                                == types[g.vars[expected.var()].ty()].unwrap(),
                        )?;
                    }
                    check(
                        self.types[self.f.vars[self.f.ret.var()].ty()]
                            == types[g.vars[g.ret.var()].ty()].unwrap(),
                    )?;
                }
                None => return Err(InstrError::TypeError),
            },
            Expr::For {
                index,
                arg,
                body,
                ret,
            } => todo!(),
            Expr::Read {
                var,
                arg,
                body,
                ret,
            } => todo!(),
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => todo!(),

            &Expr::Ask { var } => match self.var_ty(var) {
                &Ty::Ref { scope, inner } => {
                    let h = self.constr(scope);
                    check(h.contains(Constraint::Read) && t == inner)?
                }
                _ => return Err(InstrError::TypeError),
            },
            &Expr::Add { accum, addend } => match self.var_ty(accum) {
                &Ty::Ref { scope, inner } => {
                    let h = self.constr(scope);
                    let a = self.var_ty_id(addend);
                    check(h.contains(Constraint::Accum) && a == inner && *ty == Ty::Unit)?
                }
                _ => return Err(InstrError::TypeError),
            },
        }

        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("constraints for generic {} are impossible", .0.generic())]
    ImpossibleConstraints(id::Generic),

    #[error("generic ID for type {} is out of range", .0.ty())]
    InvalidGeneric(id::Ty),

    #[error("kind for scope type {} is invalid", .0.ty())]
    InvalidKind(id::Ty),

    #[error("variable ID for scope type {} is invalid", .0.ty())]
    InvalidRef(id::Ty),

    #[error("scope type ID for type {} is not strictly less", .0.ty())]
    InvalidScope(id::Ty),

    #[error("scope for type {} is not a scope", .0.ty())]
    NotScope(id::Ty),

    #[error("inner type ID for type {} is not strictly less", .0.ty())]
    InvalidInner(id::Ty),

    #[error("index type ID for type {} is not strictly less", .0.ty())]
    InvalidIndex(id::Ty),

    #[error("index for type {} is not an index", .0.ty())]
    NotIndex(id::Ty),

    #[error("element type ID for type {} is not strictly less", .0.ty())]
    InvalidElem(id::Ty),

    #[error("member {} type ID for type {} is not strictly less", .1.member(), .0.ty())]
    InvalidMember(id::Ty, id::Member),

    #[error("type ID for variable {} is out of range", .0.var())]
    InvalidVar(id::Var),

    #[error("variable ID for parameter {0} is out of range")]
    InvalidParam(usize),

    #[error("return variable ID is out of range")]
    InvalidRet,

    #[error("instruction {0} is invalid")]
    InvalidBody(usize, #[source] InstrError),
}

/// Validate `f`, assuming that all of its referenced functions are valid.
pub fn validate_func(f: impl FuncNode) -> Result<(), Error> {
    let def = f.def();

    for (i, constrs) in def.generics.iter().enumerate() {
        // for now we have no compatible constraints
        if (constrs.contains(Constraint::Index) && !constrs.contains(Constraint::Value))
            || ((constrs.contains(Constraint::Read) || constrs.contains(Constraint::Accum))
                && constrs.len() > 1)
        {
            return Err(Error::ImpossibleConstraints(id::generic(i)));
        }
    }

    let mut constraints = IndexMap::new();

    let mut types = vec![];
    for (i, ty) in def.types.iter().enumerate() {
        let t = id::ty(i);
        let (deduped, constrs) = match ty {
            Ty::Unit => (Ty::Unit, EnumSet::only(Constraint::Value)),
            Ty::Bool => (Ty::Bool, EnumSet::only(Constraint::Value)),
            Ty::F64 => (Ty::F64, EnumSet::only(Constraint::Value)),
            &Ty::Fin { size } => (Ty::Fin { size }, Constraint::Value | Constraint::Index),
            &Ty::Generic { id } => match def.generics.get(id.generic()) {
                None => return Err(Error::InvalidGeneric(t)),
                Some(&constrs) => (Ty::Generic { id }, constrs),
            },
            &Ty::Scope { kind, id } => {
                if !(kind == EnumSet::only(Constraint::Read)
                    || kind == EnumSet::only(Constraint::Accum))
                {
                    return Err(Error::InvalidKind(t));
                } else if id.var() >= def.vars.len() {
                    return Err(Error::InvalidRef(t));
                } else {
                    (Ty::Scope { kind, id }, EnumSet::only(kind))
                }
            }
            &Ty::Ref { scope, inner } => {
                if scope >= t {
                    return Err(Error::InvalidScope(t));
                } else if inner >= t {
                    return Err(Error::InvalidInner(t));
                } else if !matches!(def.types[scope.ty()], Ty::Scope { .. }) {
                    return Err(Error::NotScope(t));
                } else {
                    let scope = types[scope.ty()];
                    let inner = types[inner.ty()];
                    (Ty::Ref { scope, inner }, EnumSet::empty())
                }
            }
            &Ty::Array { index, elem } => {
                if index >= t {
                    return Err(Error::InvalidIndex(t));
                } else if elem >= t {
                    return Err(Error::InvalidElem(t));
                } else if !matches!(def.types[index.ty()], Ty::Fin { .. }) {
                    return Err(Error::NotIndex(t));
                } else {
                    let index = types[index.ty()];
                    let elem = types[elem.ty()];
                    (Ty::Array { index, elem }, EnumSet::only(Constraint::Value))
                }
            }
            Ty::Tuple { members } => {
                let mut mems = vec![];
                for (i, &member) in members.iter().enumerate() {
                    if member >= t {
                        return Err(Error::InvalidMember(t, id::member(i)));
                    } else {
                        mems.push(types[member.ty()]);
                    }
                }
                (
                    Ty::Tuple { members: mems },
                    EnumSet::only(Constraint::Value),
                )
            }
        };
        let (j, prev) = constraints.insert_full(deduped, constrs);
        if let Some(prev) = prev {
            assert_eq!(constrs, prev); // equivalent types should satisfy the same constraints
        }
        types.push(id::ty(j));
    }

    for (i, ty) in def.vars.iter().enumerate() {
        if ty.ty() >= def.types.len() {
            return Err(Error::InvalidVar(id::var(i)));
        }
    }

    for (i, param) in def.params.iter().enumerate() {
        if param.var() >= def.vars.len() {
            return Err(Error::InvalidParam(i));
        }
    }

    if def.ret.var() >= def.vars.len() {
        return Err(Error::InvalidRet);
    }

    let mut declared = vec![false; def.vars.len()];
    for param in def.params.iter() {
        declared[param.var()] = true;
    }

    let mut validator = Validator {
        node: &f,
        f: def,
        constraints,
        types,
        declared,
    };
    for (i, instr) in def.body.iter().enumerate() {
        validator
            .instr(instr)
            .map_err(|e| Error::InvalidBody(i, e))?;
    }

    Ok(())
}
