use std::rc::Rc;

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// A type definition under construction. Not guaranteed to be well-formed.
pub struct Typedef {
    pub generics: Vec<Option<crate::Constraint>>,
    pub types: Vec<crate::Typexpr>,
    pub def: crate::Type,
    pub constraint: Option<crate::Constraint>,
}

impl Typedef {
    pub fn check(self) -> Result<crate::Typedef, Error> {
        let Typedef {
            generics,
            types,
            def,
            constraint,
        } = self;
        // TODO: actually check validity
        Ok(crate::Typedef {
            generics,
            types,
            def,
            constraint,
        })
    }
}

/// A function definition under construction. Not guaranteed to be well-formed.
pub struct Function {
    pub generics: Vec<Option<crate::Constraint>>,
    pub types: Vec<crate::Typexpr>,
    pub funcs: Vec<crate::Func>,
    pub param: crate::Type,
    pub ret: crate::Type,
    pub vars: Vec<crate::Type>,
    pub body: crate::Block,
}

impl Function {
    pub fn check(self) -> Result<crate::Function, Error> {
        let Function {
            generics,
            types,
            funcs,
            param,
            ret,
            vars,
            body,
        } = self;
        // TODO: actually check validity
        Ok(crate::Function {
            generics,
            types,
            funcs,
            param,
            ret,
            vars,
            body,
        })
    }
}
