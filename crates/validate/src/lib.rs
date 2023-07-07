use enumset::EnumSet;
use indexmap::IndexMap;
use rose::{id, Binop, Block, Constraint, Expr, FuncNode, Function, Instr, Ty, Unop};

#[derive(Debug, thiserror::Error)]
pub enum BlockError {
    #[error("variable {} is redeclared at instruction {1}", .0.var())]
    Redeclare(id::Var, usize),

    #[error("primitive expression at instruction {0} does not match its type")]
    InvalidPrimitive(usize),

    #[error("type at array instruction {0} is not an array")]
    NotArray(usize),

    #[error("array at instruction {0} has the wrong number of elements")]
    InvalidArray(usize),

    #[error("array element {1} at instruction {0} does not match its type")]
    InvalidElem(usize, usize),

    #[error("type at tuple instruction {0} is not a tuple")]
    NotTuple(usize),

    #[error("tuple at instruction {0} has the wrong number of members")]
    InvalidTuple(usize),

    #[error("tuple member {} at instruction {0} does not match its type", .1.member())]
    InvalidMember(usize, id::Member),

    #[error("array type at index instruction {0} does not match index and element types")]
    InvalidIndex(usize),

    #[error("type error at instruction {0}")]
    TypeError(usize), // TODO
}

struct Validator<'a> {
    f: &'a Function,
    constraints: IndexMap<Ty, EnumSet<Constraint>>,
    /// indices from `self.f.types` into `self.constraints`
    types: Vec<id::Ty>,
    /// same length as `self.f.vars`
    declared: Vec<bool>,
    /// same length as `self.f.blocks`
    visited: Vec<bool>,
}

impl<'a> Validator<'a> {
    fn ty(&self, t: id::Ty) -> &Ty {
        let (ty, _) = self.constraints.get_index(t.ty()).unwrap();
        ty
    }

    fn var_ty_id(&self, x: id::Var) -> id::Ty {
        self.types[self.f.vars[x.var()].ty()]
    }

    fn var_ty(&self, x: id::Var) -> &Ty {
        self.ty(self.var_ty_id(x))
    }

    /// `b` must be from `self.f`, and the entry in `self.visited` for it must already be `true`.
    fn block(&mut self, b: &'a Block) -> Result<(), BlockError> {
        // TODO: check that variable references are valid
        for (i, Instr { var, expr }) in b.code.iter().enumerate() {
            if self.declared[var.var()] {
                return Err(BlockError::Redeclare(*var, i));
            }
            self.declared[var.var()] = true;
            let check = |p: bool| -> Result<(), BlockError> {
                if p {
                    Ok(())
                } else {
                    Err(BlockError::TypeError(i))
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
                                    return Err(BlockError::InvalidArray(i));
                                }
                                for (j, &x) in elems.iter().enumerate() {
                                    if self.var_ty_id(x) != elem {
                                        return Err(BlockError::InvalidElem(i, j));
                                    }
                                }
                            }
                            _ => unreachable!(), // `Fin` is currently the only `Index` type
                        }
                    }
                    _ => return Err(BlockError::NotArray(i)),
                },
                Expr::Tuple { members } => match ty {
                    Ty::Tuple { members: types } => {
                        if members.len() != types.len() {
                            return Err(BlockError::InvalidTuple(i));
                        }
                        for (j, (&x, &xt)) in members.iter().zip(types.iter()).enumerate() {
                            if self.var_ty_id(x) != xt {
                                return Err(BlockError::InvalidMember(i, id::member(j)));
                            }
                        }
                    }
                    _ => return Err(BlockError::NotTuple(i)),
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
                    _ => return Err(BlockError::TypeError(i)),
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
                    _ => return Err(BlockError::TypeError(i)),
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
                        _ => return Err(BlockError::TypeError(i)),
                    },
                    _ => return Err(BlockError::TypeError(i)),
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
                        check(
                            *ty == Ty::Bool && *self.ty(l) == Ty::F64 && self.var_ty_id(right) == l,
                        )?
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => check(
                        *ty == Ty::F64 && self.var_ty_id(left) == t && self.var_ty_id(right) == t,
                    )?,
                },

                Expr::Call { func, arg } => todo!(),
                Expr::If { cond, then, els } => todo!(),
                Expr::For { index, body } => todo!(),
                Expr::Accum { var, vector, body } => todo!(),

                &Expr::Add { accum, addend } => {
                    let acc = self.var_ty(accum);
                    let add = self.var_ty_id(addend);
                    let constrs = self.constraints[add.ty()];
                    check(
                        *ty == Ty::Unit
                            && constrs.contains(Constraint::Vector)
                            // checking `Accum` already guarantees `scope` is valid
                            && matches!(acc, &Ty::Ref { scope: _, inner } if inner == add),
                    )?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("constraints for generic {} are incompatible", .0.generic())]
    IncompatibleConstraints(id::Generic),

    #[error("generic ID for type {} is out of range", .0.ty())]
    InvalidGeneric(id::Ty),

    #[error("block ID for type {} is out of range", .0.ty())]
    InvalidBlock(id::Ty),

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

    #[error("function ID for funcref {} is out of range", .0.func())]
    InvalidFunction(id::Func),

    #[error("funcref {} has the wrong number of generics", .0.func())]
    InvalidGenerics(id::Func),

    #[error("generic {} in funcref {} does not satisfy its constraints", .1.generic(), .0.func())]
    InvalidConstraints(id::Func, id::Generic),

    #[error("parameter type ID is out of range")]
    InvalidParam,

    #[error("return type ID is out of range")]
    InvalidRet,

    #[error("type ID for variable {} is out of range", .0.var())]
    InvalidVar(id::Var),

    #[error("main block ID is out of range")]
    InvalidMain,

    #[error("main block argument type does not match function parameter")]
    InvalidMainArg,

    #[error("main block return type does not match function return type")]
    InvalidMainRet,

    #[error("body is invalid")]
    InvalidBody(#[from] BlockError),
}

/// Validate `f`, assuming that all of its referenced functions are valid.
pub fn validate_func(f: impl FuncNode) -> Result<(), Error> {
    let def = f.def();

    for (i, constrs) in def.generics.iter().enumerate() {
        // for now we have no compatible constraints
        if constrs.len() > 1 {
            return Err(Error::IncompatibleConstraints(id::generic(i)));
        }
    }

    let mut constraints = IndexMap::new();

    let mut types = vec![];
    for (i, ty) in def.types.iter().enumerate() {
        let t = id::ty(i);
        let (deduped, constrs) = match ty {
            Ty::Unit => (Ty::Unit, EnumSet::empty()),
            Ty::Bool => (Ty::Bool, EnumSet::empty()),
            Ty::F64 => (Ty::F64, EnumSet::only(Constraint::Vector)),
            &Ty::Fin { size } => (Ty::Fin { size }, EnumSet::only(Constraint::Index)),
            &Ty::Generic { id } => match def.generics.get(id.generic()) {
                None => return Err(Error::InvalidGeneric(t)),
                Some(&constrs) => (Ty::Generic { id }, constrs),
            },
            &Ty::Scope { id } => {
                if id.block() >= def.blocks.len() {
                    return Err(Error::InvalidBlock(t));
                } else {
                    (Ty::Scope { id }, EnumSet::only(Constraint::Scope))
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
                    (
                        Ty::Array { index, elem },
                        EnumSet::only(Constraint::Vector) & constraints[elem.ty()],
                    )
                }
            }
            Ty::Tuple { members } => {
                let mut mems = vec![];
                let mut constrs = EnumSet::only(Constraint::Vector);
                for (i, &member) in members.iter().enumerate() {
                    if member >= t {
                        return Err(Error::InvalidMember(t, id::member(i)));
                    } else {
                        let mem = types[member.ty()];
                        mems.push(mem);
                        constrs &= constraints[mem.ty()];
                    }
                }
                (Ty::Tuple { members: mems }, constrs)
            }
        };
        let (j, prev) = constraints.insert_full(deduped, constrs);
        if let Some(prev) = prev {
            assert_eq!(constrs, prev); // equivalent types should satisfy the same constraints
        }
        types.push(id::ty(j));
    }

    for (i, func) in def.funcs.iter().enumerate() {
        match f.get(func.id) {
            None => return Err(Error::InvalidFunction(id::func(i))),
            Some(g) => {
                if g.def().generics.len() != func.generics.len() {
                    return Err(Error::InvalidGenerics(id::func(i)));
                } else {
                    for (j, ty) in func.generics.iter().enumerate() {
                        if !constraints[types[ty.ty()].ty()].is_superset(g.def().generics[j]) {
                            return Err(Error::InvalidConstraints(id::func(i), id::generic(j)));
                        }
                    }
                }
            }
        }
    }

    if def.param.ty() >= def.types.len() {
        return Err(Error::InvalidParam);
    }

    if def.ret.ty() >= def.types.len() {
        return Err(Error::InvalidRet);
    }

    for (i, ty) in def.vars.iter().enumerate() {
        if ty.ty() >= def.types.len() {
            return Err(Error::InvalidVar(id::var(i)));
        }
    }

    match def.blocks.get(def.main.block()) {
        None => Err(Error::InvalidMain),
        Some(b) => {
            if types[def.vars[b.arg.var()].ty()] != types[def.param.ty()] {
                return Err(Error::InvalidMainArg);
            }

            if types[def.vars[b.ret.var()].ty()] != types[def.ret.ty()] {
                return Err(Error::InvalidMainRet);
            }

            let mut declared = vec![false; def.vars.len()];
            declared[b.arg.var()] = true;

            let mut visited = vec![false; def.blocks.len()];
            visited[def.main.block()] = true;

            Ok(Validator {
                f: def,
                constraints,
                types,
                declared,
                visited,
            }
            .block(b)?)
        }
    }
}
