use rose::{id, FuncNode, Ty};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("constraints for generic {} are incompatible", .0.generic())]
    IncompatibleConstraints(id::Generic),

    #[error("generic ID for type {} is out of range", .0.ty())]
    InvalidGeneric(id::Ty),

    #[error("block ID for type {} is out of range", .0.ty())]
    InvalidBlock(id::Ty),

    #[error("scope type ID for type {} is out of range", .0.ty())]
    InvalidScope(id::Ty),

    #[error("scope for type {} is not a scope", .0.ty())]
    NotScope(id::Ty),

    #[error("inner type ID for type {} is out of range", .0.ty())]
    InvalidInner(id::Ty),

    #[error("index type ID for type {} is out of range", .0.ty())]
    InvalidIndex(id::Ty),

    #[error("index for type {} is not an index", .0.ty())]
    NotIndex(id::Ty),

    #[error("element type ID for type {} is out of range", .0.ty())]
    InvalidElem(id::Ty),

    #[error("member {} type ID for type {} is out of range", .1.member(), .0.ty())]
    InvalidMember(id::Ty, id::Member),
}

/// Validate `f`, assuming that all of its referenced types and functions are valid.
pub fn validate_func(f: impl FuncNode) -> Result<(), Error> {
    let def = f.def();

    for (i, constraints) in def.generics.iter().enumerate() {
        // for now we have no compatible constraints
        if constraints.len() > 1 {
            return Err(Error::IncompatibleConstraints(id::generic(i)));
        }
    }

    for (i, ty) in def.types.iter().enumerate() {
        let t = id::ty(i);
        match ty {
            Ty::Unit | Ty::Bool | Ty::F64 | Ty::Fin { .. } => Ok(()),
            Ty::Generic { id } => {
                if id.generic() >= def.generics.len() {
                    Err(Error::InvalidGeneric(t))
                } else {
                    Ok(())
                }
            }
            Ty::Scope { id } => {
                if id.block() >= def.blocks.len() {
                    Err(Error::InvalidScope(t))
                } else {
                    Ok(())
                }
            }
            Ty::Ref { scope, inner } => {
                if scope.ty() >= def.types.len() {
                    Err(Error::InvalidScope(t))
                } else if inner.ty() >= def.types.len() {
                    Err(Error::InvalidInner(t))
                } else if !matches!(def.types[scope.ty()], Ty::Scope { .. }) {
                    Err(Error::NotScope(t))
                } else {
                    Ok(())
                }
            }
            Ty::Array { index, elem } => {
                if index.ty() >= def.types.len() {
                    Err(Error::InvalidIndex(t))
                } else if elem.ty() >= def.types.len() {
                    Err(Error::InvalidElem(t))
                } else if !matches!(def.types[index.ty()], Ty::Fin { .. }) {
                    Err(Error::NotIndex(t))
                } else {
                    Ok(())
                }
            }
            Ty::Tuple { members } => members.iter().enumerate().try_for_each(|(i, member)| {
                if member.ty() >= def.types.len() {
                    Err(Error::InvalidMember(t, id::member(i)))
                } else {
                    Ok(())
                }
            }),
        }?;
    }

    Ok(())
}
