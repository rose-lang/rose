use enumset::EnumSet;
use indexmap::IndexMap;
use rose::{id, Binop, Constraint, Expr, FuncNode, Function, Instr, Ty, Unop};

#[derive(Debug, thiserror::Error)]
pub enum InstrError {
    #[error("variable ID is out of range")]
    InvalidVar,

    #[error("variable was already declared")]
    Redeclare,

    #[error("type is not unit")]
    UnitType,

    #[error("type is not boolean")]
    BoolType,

    #[error("type is not 64-bit float")]
    F64Type,

    #[error("type is not a bounded integer")]
    FinType,

    #[error("out of range for bounded integer type")]
    FinTooBig,

    #[error("type is not an array")]
    ArrayType,

    #[error("literal array index type is not a bounded integer")]
    ArrayIndex,

    #[error("array has the wrong number of elements")]
    ArraySize,

    #[error("variable ID for array element {0} is out of range")]
    ArrayInvalidElem(usize),

    #[error("array element {0} does not match its type")]
    ArrayElemType(usize),

    #[error("type is not a tuple")]
    TupleType,

    #[error("tuple has the wrong number of members")]
    TupleSize,

    #[error("variable ID for tuple member {} is out of range", .0.member())]
    TupleInvalidMember(id::Member),

    #[error("tuple member {} does not match its type", .0.member())]
    TupleMemberType(id::Member),

    #[error("index variable ID for index instruction is out of range")]
    IndexInvalidArray,

    #[error("array variable ID for index instruction is out of range")]
    IndexInvalidIndex,

    #[error("index array type does not match index and element types")]
    IndexType,

    #[error("tuple variable ID for member instruction is out of range")]
    MemberInvalidTuple,

    #[error("tuple variable for member instruction is not a tuple")]
    MemberNotTuple,

    #[error("member ID for member instruction is out of range")]
    MemberInvalidMember,

    #[error("member does not match its type")]
    MemberType,

    #[error("index variable ID for slice instruction is out of range")]
    SliceInvalidArray,

    #[error("array variable ID for slice instruction is out of range")]
    SliceInvalidIndex,

    #[error("array variable for slice instruction is not a reference")]
    SliceArrayNotRef,

    #[error("return variable for slice instruction is not a reference")]
    SliceNotRef,

    #[error("slice scope mismatch")]
    SliceScope,

    #[error("slice array type does not match index and return types")]
    SliceType,

    #[error("tuple variable ID for field instruction is out of range")]
    FieldInvalidTuple,

    #[error("tuple variable for field instruction is not a reference")]
    FieldTupleNotRef,

    #[error("return variable for field instruction is not a reference")]
    FieldNotRef,

    #[error("field scope mismatch")]
    FieldScope,

    #[error("referenced tuple for field instruction is not a tuple")]
    FieldNotTuple,

    #[error("member ID for field instruction is out of range")]
    FieldInvalidMember,

    #[error("field tuple type does not match member and return types")]
    FieldType,

    #[error("argument variable ID is out of range")]
    UnaryInvalidArg,

    #[error("unary type error")]
    UnaryType,

    #[error("left variable ID is out of range")]
    BinaryInvalidLeft,

    #[error("right variable ID is out of range")]
    BinaryInvalidRight,

    #[error("binary type error")]
    BinaryType,

    #[error("condition variable ID is out of range")]
    SelectInvalidCond,

    #[error("true case variable ID is out of range")]
    SelectInvalidThen,

    #[error("false case variable ID is out of range")]
    SelectInvalidEls,

    #[error("select type error")]
    SelectType,

    #[error("missing function")]
    CallFunction,

    #[error("wrong number of generics")]
    CallGenericsCount,

    #[error("type ID for generic {0} is out of range")]
    CallInvalidGeneric(usize),

    #[error("generic {0} does not satisfy its constraints")]
    CallGeneric(usize),

    #[error("wrong number of arguments")]
    CallArgsCount,

    #[error("variable ID for argument {0} is out of range")]
    CallInvalidArg(usize),

    #[error("type for argument {0} does not match")]
    CallArg(usize),

    #[error("return type does not match")]
    CallRet,

    #[error("variable ID is out of range")]
    AskInvalidVar,

    #[error("variable is not a reference")]
    AskNotRef,

    #[error("scope does not allow reading")]
    AskRead,

    #[error("type mismatch")]
    AskType,

    #[error("accumulator variable ID is out of range")]
    AddInvalidAccum,

    #[error("addend variable ID is out of range")]
    AddInvalidAddend,

    #[error("accumulator is not a reference")]
    AddNotRef,

    #[error("scope does not allow accumulation")]
    AddAccum,

    #[error("type mismatch")]
    AddType,
}

fn check(p: bool, e: InstrError) -> Result<(), InstrError> {
    if p {
        Ok(())
    } else {
        Err(e)
    }
}

#[derive(Clone, Copy)]
enum Scope {
    Undefined,
    Defined,
    Expired,
}

struct Validator<'a, F: FuncNode> {
    node: &'a F,
    f: &'a Function,
    constraints: IndexMap<Ty, EnumSet<Constraint>>,
    /// indices from `self.f.types` into `self.constraints`
    types: Vec<id::Ty>,
    /// same length as `self.f.vars`
    vars: Vec<Scope>,
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

    fn var_ty_id(&self, x: id::Var) -> Option<id::Ty> {
        Some(self.types[self.f.vars.get(x.var())?.ty()])
    }

    fn var_ty(&self, x: id::Var) -> Option<&Ty> {
        Some(self.ty(self.var_ty_id(x)?))
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
        use InstrError::*;

        let Instr { var, expr } = instr;
        match self.vars.get(var.var()) {
            None => return Err(InvalidVar),
            Some(Scope::Defined | Scope::Expired) => return Err(Redeclare),
            Some(Scope::Undefined) => self.vars[var.var()] = Scope::Defined,
        }
        let t = self.var_ty_id(*var).unwrap();
        let ty = self.ty(t);

        match expr {
            Expr::Unit => check(*ty == Ty::Unit, UnitType),
            Expr::Bool { .. } => check(*ty == Ty::Bool, BoolType),
            Expr::F64 { .. } => check(*ty == Ty::F64, F64Type),
            &Expr::Fin { val } => match ty {
                &Ty::Fin { size } => check(val < size, FinTooBig),
                _ => Err(FinType),
            },

            Expr::Array { elems } => match ty {
                &Ty::Array { index, elem } => match self.ty(index) {
                    &Ty::Fin { size } => {
                        if elems.len() != size {
                            return Err(ArraySize);
                        }
                        for (i, &x) in elems.iter().enumerate() {
                            match self.var_ty_id(x) {
                                Some(ti) => check(ti == elem, ArrayElemType(i))?,
                                None => return Err(ArrayInvalidElem(i)),
                            }
                        }
                        Ok(())
                    }
                    _ => Err(ArrayIndex),
                },
                _ => Err(ArrayType),
            },
            Expr::Tuple { members } => match ty {
                Ty::Tuple { members: types } => {
                    if members.len() != types.len() {
                        return Err(TupleSize);
                    }
                    for (i, (&x, &xt)) in members.iter().zip(types.iter()).enumerate() {
                        let id = id::member(i);
                        match self.var_ty_id(x) {
                            Some(tx) => check(tx == xt, TupleMemberType(id))?,
                            None => return Err(TupleInvalidMember(id)),
                        }
                    }
                    Ok(())
                }
                _ => Err(TupleType),
            },

            &Expr::Index { array, index } => {
                let arr = self.var_ty(array).ok_or(IndexInvalidArray)?;
                let index = self.var_ty_id(index).ok_or(IndexInvalidIndex)?;
                check(*arr == Ty::Array { index, elem: t }, IndexType)
            }
            &Expr::Member { tuple, member } => match self.var_ty(tuple) {
                Some(Ty::Tuple { members }) => match members.get(member.member()) {
                    Some(&mem) => check(t == mem, MemberType),
                    None => Err(MemberInvalidMember),
                },
                Some(_) => Err(MemberNotTuple),
                None => Err(MemberInvalidTuple),
            },

            &Expr::Slice { array, index } => {
                let array = self.var_ty(array).ok_or(SliceInvalidArray)?;
                let index = self.var_ty_id(index).ok_or(SliceInvalidIndex)?;
                let (scope_arr, arr) = match array {
                    &Ty::Ref { scope, inner } => (scope, inner),
                    _ => return Err(SliceArrayNotRef),
                };
                let (scope_elem, elem) = match ty {
                    &Ty::Ref { scope, inner } => (scope, inner),
                    _ => return Err(SliceNotRef),
                };
                check(scope_elem == scope_arr, SliceScope)?;
                check(*self.ty(arr) == Ty::Array { index, elem }, SliceType)
            }
            &Expr::Field { tuple, member } => {
                let (scope_tup, tup) = match self.var_ty(tuple).ok_or(FieldInvalidTuple)? {
                    &Ty::Ref { scope, inner } => (scope, inner),
                    _ => return Err(FieldTupleNotRef),
                };
                let (scope_mem, mem) = match ty {
                    &Ty::Ref { scope, inner } => (scope, inner),
                    _ => return Err(FieldNotRef),
                };
                check(scope_mem == scope_tup, FieldScope)?;
                match self.ty(tup) {
                    Ty::Tuple { members } => match members.get(member.member()) {
                        Some(&m) => check(mem == m, FieldType),
                        None => Err(FieldInvalidMember),
                    },
                    _ => Err(FieldNotTuple),
                }
            }

            &Expr::Unary { op, arg } => {
                let x = self.var_ty_id(arg).ok_or(UnaryInvalidArg)?;
                let p = match op {
                    Unop::Not => *ty == Ty::Bool && x == t,
                    Unop::Neg | Unop::Abs | Unop::Sqrt => *ty == Ty::F64 && x == t,
                };
                check(p, UnaryType)
            }
            &Expr::Binary { op, left, right } => {
                let l = self.var_ty_id(left).ok_or(BinaryInvalidLeft)?;
                let r = self.var_ty_id(right).ok_or(BinaryInvalidRight)?;
                let p = match op {
                    Binop::And | Binop::Or | Binop::Iff | Binop::Xor => {
                        *ty == Ty::Bool && l == t && r == t
                    }
                    Binop::Neq | Binop::Lt | Binop::Leq | Binop::Eq | Binop::Gt | Binop::Geq => {
                        *ty == Ty::Bool && *self.ty(l) == Ty::F64 && r == l
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                        *ty == Ty::F64 && l == t && r == t
                    }
                };
                check(p, BinaryType)
            }
            &Expr::Select { cond, then, els } => {
                let c = self.var_ty(cond).ok_or(SelectInvalidCond)?;
                let a = self.var_ty_id(then).ok_or(SelectInvalidThen)?;
                let b = self.var_ty_id(els).ok_or(SelectInvalidEls)?;
                check(*c == Ty::Bool && a == b && t == a, SelectType)
            }

            Expr::Call { id, generics, args } => match self.node.get(*id) {
                Some(node) => {
                    let g = node.def();
                    if generics.len() != g.generics.len() {
                        return Err(CallGenericsCount);
                    }
                    for (i, (expected, actual)) in
                        g.generics.iter().zip(generics.iter()).enumerate()
                    {
                        match self.types.get(actual.ty()) {
                            Some(generic) => {
                                check(self.constr(*generic).is_superset(*expected), CallGeneric(i))?
                            }
                            None => return Err(CallInvalidGeneric(i)),
                        }
                    }
                    let mut types = vec![];
                    for typ in g.types.iter() {
                        let i = self.resolve(generics, &types, typ);
                        types.push(i);
                    }
                    if args.len() != g.params.len() {
                        return Err(CallArgsCount);
                    }
                    for (i, (expected, &actual)) in g.params.iter().zip(args.iter()).enumerate() {
                        match self.var_ty_id(actual) {
                            Some(arg) => check(
                                arg == types[g.vars[expected.var()].ty()].unwrap(),
                                CallArg(i),
                            )?,
                            None => return Err(CallInvalidArg(i)),
                        }
                    }
                    check(t == types[g.vars[g.ret.var()].ty()].unwrap(), CallRet)
                }
                None => Err(CallFunction),
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

            &Expr::Ask { var } => match self.var_ty(var).ok_or(AskInvalidVar)? {
                &Ty::Ref { scope, inner } => {
                    check(self.constr(scope).contains(Constraint::Read), AskRead)?;
                    check(t == inner, AskType)
                }
                _ => Err(AskNotRef),
            },
            &Expr::Add { accum, addend } => {
                let acc = self.var_ty(accum).ok_or(AddInvalidAccum)?;
                let add = self.var_ty_id(addend).ok_or(AddInvalidAddend)?;
                match acc {
                    &Ty::Ref { scope, inner } => {
                        check(self.constr(scope).contains(Constraint::Accum), AddAccum)?;
                        check(add == inner && *ty == Ty::Unit, AddType)
                    }
                    _ => Err(AddNotRef),
                }
            }
        }
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

    let mut vars = vec![Scope::Undefined; def.vars.len()];
    for param in def.params.iter() {
        vars[param.var()] = Scope::Defined;
    }

    let mut validator = Validator {
        node: &f,
        f: def,
        constraints,
        types,
        vars,
    };
    for (i, instr) in def.body.iter().enumerate() {
        validator
            .instr(instr)
            .map_err(|e| Error::InvalidBody(i, e))?;
    }

    Ok(())
}
