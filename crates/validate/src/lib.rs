use rose::{id, FuncNode, Function, Type, Typexpr};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("generic ID {} is out of range", .id.generic())]
    InvalidGeneric { id: id::Generic },

    #[error("scope ID {} is out of range", .id.block())]
    InvalidScope { id: id::Block },

    #[error("type ID {} is out of range", .id.typexpr())]
    InvalidType { id: id::Typexpr },
}

struct FuncValidator<'a> {
    f: &'a Function,
}

impl FuncValidator<'_> {
    fn ty(&self, t: Type) -> Result<(), TypeError> {
        match t {
            Type::Unit | Type::Bool | Type::F64 | Type::Fin { .. } => Ok(()),
            Type::Generic { id } => {
                if id.generic() < self.f.generics.len() {
                    Ok(())
                } else {
                    Err(TypeError::InvalidGeneric { id })
                }
            }
            Type::Scope { id } => {
                if id.block() < self.f.blocks.len() {
                    Ok(())
                } else {
                    Err(TypeError::InvalidScope { id })
                }
            }
            Type::Expr { id } => {
                if id.typexpr() < self.f.types.len() {
                    Ok(())
                } else {
                    Err(TypeError::InvalidType { id })
                }
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("constraints for generic {} are incompatible", .0.generic())]
    IncompatibleConstraints(id::Generic),

    #[error("scope for type {} is invalid", .0.typexpr())]
    InvalidScope(id::Typexpr, #[source] TypeError),

    #[error("scope for type {} is not a scope", .0.typexpr())]
    NotScope(id::Typexpr),

    #[error("inner for type {} is invalid", .0.typexpr())]
    InvalidInner(id::Typexpr, #[source] TypeError),

    #[error("index for type {} is invalid", .0.typexpr())]
    InvalidIndex(id::Typexpr, #[source] TypeError),

    #[error("index for type {} is not an index", .0.typexpr())]
    NotIndex(id::Typexpr),

    #[error("element for type {} is invalid", .0.typexpr())]
    InvalidElem(id::Typexpr, #[source] TypeError),

    #[error("member {} for type {} is invalid", .1.member(), .0.typexpr())]
    InvalidMember(id::Typexpr, id::Member, #[source] TypeError),
}

/// Validate `f`, assuming that all of its referenced types and functions are valid.
pub fn validate_func(f: impl FuncNode) -> Result<(), Error> {
    let def = f.def();
    let validate = FuncValidator { f: def };

    for (i, constraints) in def.generics.iter().enumerate() {
        // for now we have no compatible constraints
        if constraints.len() > 1 {
            return Err(Error::IncompatibleConstraints(id::generic(i)));
        }
    }

    for (i, typexpr) in def.types.iter().enumerate() {
        let id = id::typexpr(i);
        match typexpr {
            &Typexpr::Ref { scope, inner } => {
                validate.ty(scope).map_err(|e| Error::InvalidScope(id, e))?;
                validate.ty(inner).map_err(|e| Error::InvalidInner(id, e))?;
                if !matches!(scope, Type::Scope { .. }) {
                    return Err(Error::NotScope(id));
                }
            }
            &Typexpr::Array { index, elem } => {
                validate.ty(index).map_err(|e| Error::InvalidIndex(id, e))?;
                validate.ty(elem).map_err(|e| Error::InvalidElem(id, e))?;
                if !matches!(index, Type::Fin { .. }) {
                    return Err(Error::NotIndex(id));
                }
            }
            Typexpr::Tuple { members } => {
                for (i, &member) in members.iter().enumerate() {
                    validate
                        .ty(member)
                        .map_err(|e| Error::InvalidMember(id, id::member(i), e))?;
                }
            }
            Typexpr::Def { id, params } => todo!(),
        }
    }

    Ok(())
}
