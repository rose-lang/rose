use crate::{ast, tokens};
use rose::{self as ir, id};
use std::{collections::HashMap, ops::Range, rc::Rc};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("")]
    UnknownType { name: Range<usize> },

    #[error("")]
    BadTensorType { name: Range<usize> },

    #[error("")]
    UnknownVar { name: Range<usize> },

    #[error("")]
    EmptyVec,

    #[error("")]
    UnknownFunc { name: Range<usize> },

    #[error("")]
    IndexPrimitive { t: ir::Type },

    #[error("")]
    IndexTuple,

    #[error("")]
    MemberPrimitive { t: ir::Type },

    #[error("")]
    BadSwizzle { swizzle: Range<usize> },

    #[error("")]
    ForNonSize,

    #[error("")]
    BadBinaryArgs {
        op: tokens::Binop,
        left: ir::Type,
        right: ir::Type,
    },
}

type ParsedTypes<'input> = (
    HashMap<&'input str, id::Generic>,
    Vec<ir::Typexpr>,
    Vec<ir::Type>,
);

fn parse_types<'input>(
    lookup: impl Fn(&'input str) -> Option<Rc<ir::Def<ir::Typexpr>>>,
    typenames: impl Iterator<Item = ast::Spanned<&'input str>>,
) -> Result<ParsedTypes<'input>, TypeError> {
    let mut generics = HashMap::new();
    let mut typevars = vec![];
    let mut types = vec![];
    for typ in typenames {
        types.push(if let Some(def) = lookup(typ.val) {
            let typexpr = ir::Typexpr::Typedef {
                def,
                params: vec![],
            };
            let id = id::typ(typevars.len());
            typevars.push(typexpr);
            ir::Type::Var { id }
        } else {
            let mut t = ir::Type::Real;
            let dims = typ
                .val
                .strip_prefix('R')
                .ok_or_else(|| TypeError::UnknownType { name: typ.span() })?;
            if !dims.is_empty() {
                for dim in dims.rsplit('x') {
                    let typexpr = ir::Typexpr::Vector {
                        elem: t,
                        size: if let Ok(n) = dim.parse() {
                            ir::Size::Const { val: n }
                        } else if dim.is_empty() {
                            // TODO: change this condition to check for valid identifiers
                            return Err(TypeError::BadTensorType { name: typ.span() });
                        } else {
                            ir::Size::Generic {
                                id: if let Some(&i) = generics.get(dim) {
                                    i
                                } else {
                                    let i = id::generic(generics.len());
                                    generics.insert(dim, i);
                                    i
                                },
                            }
                        },
                    };
                    let id = id::typ(typevars.len());
                    typevars.push(typexpr);
                    t = ir::Type::Var { id };
                }
            }
            t
        });
    }
    Ok((generics, typevars, types))
}

#[derive(Debug)]
pub struct Type<'input> {
    pub def: Rc<rose::Def<rose::Typexpr>>,
    /// Sorted lexicographically.
    pub fields: Vec<&'input str>,
}

#[derive(Debug)]
pub struct Module<'input> {
    pub types: HashMap<&'input str, Type<'input>>,
    pub funcs: HashMap<&'input str, Rc<rose::Def<rose::Function>>>,
}

struct FunCtx<'input, 'a> {
    m: &'a Module<'input>,
    f: ir::Function,
    t: Vec<ir::Typexpr>,
    g: HashMap<&'input str, id::Generic>,
    l: HashMap<&'input str, id::Local>,
}

impl<'input, 'a> FunCtx<'input, 'a> {
    fn newtype(&mut self, t: ir::Typexpr) -> id::Typ {
        let id = id::typ(self.t.len());
        self.t.push(t);
        id
    }

    fn newlocal(&mut self, t: ir::Type) -> id::Local {
        let id = id::local(self.f.locals.len());
        self.f.locals.push(t);
        id
    }

    fn newfunc(&mut self, f: ir::Inst) -> id::Func {
        let id = id::func(self.f.funcs.len());
        self.f.funcs.push(f);
        id
    }

    fn gettype(&self, id: id::Typ) -> &ir::Typexpr {
        &self.t[id.typ()]
    }

    fn getlocal(&self, id: id::Local) -> ir::Type {
        self.f.locals[id.local()]
    }

    fn lookup(&self, name: &'input str) -> Option<(ir::Type, ir::Instr)> {
        if let Some(&id) = self.l.get(name) {
            Some((self.getlocal(id), ir::Instr::Get { id }))
        } else if let Some(&id) = self.g.get(name) {
            Some((
                ir::Type::Size {
                    val: ir::Size::Generic { id },
                },
                ir::Instr::Generic { id },
            ))
        } else {
            None
        }
    }

    fn bind(&mut self, t: ir::Type, bind: ast::Bind<'input>) {
        let id = self.newlocal(t);
        self.f.body.push(ir::Instr::Set { id });
        match bind {
            ast::Bind::Id { name } => {
                self.l.insert(name, id);
            }
            ast::Bind::Vector { elems: _ } => todo!(),
            ast::Bind::Struct { members: _ } => todo!(),
        }
    }

    fn unify(
        &mut self,
        _f: &ir::Def<ir::Function>,
        _args: &[ir::Type],
    ) -> Result<(Vec<ir::Size>, ir::Type), TypeError> {
        todo!()
    }

    fn typecheck(&mut self, expr: ast::Expr<'input>) -> Result<ir::Type, TypeError> {
        // TODO: prevent clobbering and don't let variable names escape their scope
        match expr {
            ast::Expr::Id { name } => {
                let (t, instr) = self
                    .lookup(name.val)
                    .ok_or_else(|| TypeError::UnknownVar { name: name.span() })?;
                self.f.body.push(instr);
                Ok(t)
            }
            ast::Expr::Int { val } => {
                self.f.body.push(ir::Instr::Int { val });
                Ok(ir::Type::Size {
                    val: ir::Size::Const {
                        val: val
                            .try_into()
                            .expect("pointer sizes smaller than `u32` are not supported"),
                    },
                })
            }
            ast::Expr::Vector { elems } => {
                let n = elems.len();
                let mut t = None;
                for elem in elems {
                    // TODO: make sure all the element types are compatible
                    t = Some(self.typecheck(elem)?);
                }
                match t {
                    Some(elem) => Ok(ir::Type::Var {
                        id: self.newtype(ir::Typexpr::Vector {
                            elem,
                            size: ir::Size::Const { val: n },
                        }),
                    }),
                    None => Err(TypeError::EmptyVec),
                }
            }
            ast::Expr::Struct { members: _ } => todo!(),
            ast::Expr::Index { val, index } => {
                let t = self.typecheck(*val)?;
                self.typecheck(*index)?; // TODO: ensure this is `Nat` and bounded by `val` size
                self.f.body.push(ir::Instr::Index);
                match t {
                    ir::Type::Var { id } => match self.gettype(id) {
                        ir::Typexpr::Vector { elem, .. } => Ok(*elem),
                        ir::Typexpr::Tuple { .. } => Err(TypeError::IndexTuple),
                        ir::Typexpr::Typedef { def: _, params: _ } => todo!(),
                    },
                    _ => Err(TypeError::IndexPrimitive { t }),
                }
            }
            ast::Expr::Member { val, member } => {
                let t = self.typecheck(*val)?;
                match t {
                    ir::Type::Var { id } => match self.gettype(id) {
                        ir::Typexpr::Vector { elem, size: _ } => {
                            let t = *elem;
                            let i = match member.val {
                                // TODO: check against vector size
                                "r" | "x" => Ok(0),
                                "g" | "y" => Ok(1),
                                "b" | "z" => Ok(2),
                                "a" | "w" => Ok(3),
                                // TODO: allow multi-character swizzles
                                _ => Err(TypeError::BadSwizzle {
                                    swizzle: member.span(),
                                }),
                            }?;
                            self.f.body.push(ir::Instr::Int { val: i });
                            self.f.body.push(ir::Instr::Index);
                            Ok(t)
                        }
                        ir::Typexpr::Tuple { members: _ } => todo!(),
                        ir::Typexpr::Typedef { def: _, params: _ } => todo!(),
                    },
                    _ => Err(TypeError::MemberPrimitive { t }),
                }
            }
            ast::Expr::Let { bind, val, body } => {
                let t = self.typecheck(*val)?;
                self.bind(t, bind);
                self.typecheck(*body)
            }
            ast::Expr::Call { func, args } => {
                let mut types = vec![];
                for arg in args {
                    types.push(self.typecheck(arg)?);
                }
                if let Some(def) = self.m.funcs.get(func.val) {
                    let (params, ret) = self.unify(def, &types)?;
                    let id = self.newfunc(ir::Inst {
                        def: Rc::clone(def),
                        params,
                    });
                    self.f.body.push(ir::Instr::Call { id });
                    Ok(ret)
                } else {
                    // TODO: validate argument types for builtin functions
                    // TODO: support builtin functions on integers
                    match func.val {
                        "abs" => {
                            self.f.body.push(ir::Instr::Unary {
                                op: ir::Unop::AbsReal,
                            });
                            Ok(ir::Type::Real)
                        }
                        "max" => {
                            self.f.body.push(ir::Instr::Unary {
                                op: ir::Unop::MaxReal,
                            });
                            Ok(ir::Type::Real)
                        }
                        "min" => {
                            self.f.body.push(ir::Instr::Unary {
                                op: ir::Unop::MinReal,
                            });
                            Ok(ir::Type::Real)
                        }
                        "prod" => {
                            self.f.body.push(ir::Instr::Unary {
                                op: ir::Unop::ProdReal,
                            });
                            Ok(ir::Type::Real)
                        }
                        "sqrt" => {
                            self.f.body.push(ir::Instr::Unary { op: ir::Unop::Sqrt });
                            Ok(ir::Type::Real)
                        }
                        "sum" => {
                            self.f.body.push(ir::Instr::Unary {
                                op: ir::Unop::SumReal,
                            });
                            Ok(ir::Type::Real)
                        }
                        _ => Err(TypeError::UnknownFunc { name: func.span() }),
                    }
                }
            }
            ast::Expr::If { cond, then, els } => {
                self.typecheck(*cond)?; // TODO: ensure this is `Bool`
                self.f.body.push(ir::Instr::If);
                let t = self.typecheck(*then)?;
                self.f.body.push(ir::Instr::Else);
                self.typecheck(*els)?; // TODO: ensure this matches `t`
                self.f.body.push(ir::Instr::End);
                Ok(t)
            }
            ast::Expr::For { index, limit, body } => match self.lookup(limit.val) {
                Some((ir::Type::Size { val }, _)) => {
                    self.f.body.push(ir::Instr::For { limit: val });
                    let id = self.newlocal(ir::Type::Nat { bound: val });
                    self.l.insert(index, id);
                    self.f.body.push(ir::Instr::Set { id });
                    let t = self.typecheck(*body)?;
                    self.f.body.push(ir::Instr::End);
                    let id = self.newtype(ir::Typexpr::Vector { elem: t, size: val });
                    Ok(ir::Type::Var { id })
                }
                Some(_) => Err(TypeError::ForNonSize),
                None => Err(TypeError::UnknownVar { name: limit.span() }),
            },
            ast::Expr::Unary { op: _, arg: _ } => todo!(),
            ast::Expr::Binary { op, left, right } => {
                let l = self.typecheck(*left)?;
                let r = self.typecheck(*right)?;
                match (l, op, r) {
                    // TODO: handle integer arithmetic
                    (ir::Type::Real, tokens::Binop::Add, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::AddReal,
                        });
                        Ok(ir::Type::Real)
                    }
                    (ir::Type::Real, tokens::Binop::Sub, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::SubReal,
                        });
                        Ok(ir::Type::Real)
                    }
                    (ir::Type::Real, tokens::Binop::Mul, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::MulReal,
                        });
                        Ok(ir::Type::Real)
                    }
                    (ir::Type::Real, tokens::Binop::Div, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::DivReal,
                        });
                        Ok(ir::Type::Real)
                    }

                    // TODO: handle `mod` on other integer types
                    (ir::Type::Int, tokens::Binop::Mod, ir::Type::Size { val }) => {
                        self.f.body.push(ir::Instr::Binary { op: ir::Binop::Mod });
                        Ok(ir::Type::Nat { bound: val })
                    }

                    // TODO: handle comparison on booleans and integers
                    (ir::Type::Real, tokens::Binop::Eq, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::EqReal,
                        });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Real, tokens::Binop::Neq, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::NeqReal,
                        });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Real, tokens::Binop::Lt, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::LtReal,
                        });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Real, tokens::Binop::Gt, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::GtReal,
                        });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Real, tokens::Binop::Leq, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::LeqReal,
                        });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Real, tokens::Binop::Geq, ir::Type::Real) => {
                        self.f.body.push(ir::Instr::Binary {
                            op: ir::Binop::GeqReal,
                        });
                        Ok(ir::Type::Bool)
                    }

                    (ir::Type::Bool, tokens::Binop::And, ir::Type::Bool) => {
                        self.f.body.push(ir::Instr::Binary { op: ir::Binop::And });
                        Ok(ir::Type::Bool)
                    }
                    (ir::Type::Bool, tokens::Binop::Or, ir::Type::Bool) => {
                        self.f.body.push(ir::Instr::Binary { op: ir::Binop::Or });
                        Ok(ir::Type::Bool)
                    }

                    _ => Err(TypeError::BadBinaryArgs {
                        op,
                        left: l,
                        right: r,
                    }),
                }
            }
        }
    }
}

impl<'input> Module<'input> {
    fn define(&mut self, def: ast::Def<'input>) -> Result<(), TypeError> {
        match def {
            ast::Def::Type { name, mut members } => {
                // TODO: check for duplicate field names
                members.sort_by_key(|&(name, _)| name); // to ignore field order in literals
                let (genericnames, typevars, fields) = parse_types(
                    |s| self.types.get(s).map(|Type { def, .. }| Rc::clone(def)),
                    members.iter().map(|&(_, t)| t),
                )?;
                let t = Rc::new(ir::Def {
                    generics: genericnames.len(),
                    types: typevars,
                    def: ir::Typexpr::Tuple { members: fields },
                });
                // TODO: check for duplicate type names
                self.types.insert(
                    name,
                    Type {
                        def: t,
                        fields: members
                            .into_iter()
                            .map(|(name, _)| name)
                            .collect::<Vec<_>>(),
                    },
                );
            }
            ast::Def::Func {
                name,
                params,
                typ,
                body,
            } => {
                let (genericnames, typevars, mut paramtypes) = parse_types(
                    |s| self.types.get(s).map(|Type { def, .. }| Rc::clone(def)),
                    // TODO: handle return type separately from params w.r.t. generics
                    params.iter().map(|&(_, t)| t).chain([typ]),
                )?;
                let ret = paramtypes.pop().expect("`parse_types` should preserve len");
                let mut ctx = FunCtx {
                    m: self,
                    f: ir::Function {
                        params: paramtypes.clone(), // should be a way to do this without `clone`...
                        ret: vec![ret],
                        locals: vec![],
                        funcs: vec![],
                        body: vec![],
                    },
                    t: typevars,
                    g: genericnames,
                    l: HashMap::new(),
                };
                for ((bind, _), t) in params.into_iter().zip(paramtypes).rev() {
                    ctx.bind(t, bind);
                }
                ctx.typecheck(body)?; // TODO: ensure this matches `ret`
                let f = Rc::new(ir::Def {
                    generics: ctx.g.len(),
                    types: ctx.t,
                    def: ctx.f,
                });
                // TODO: check for duplicate function names
                self.funcs.insert(name, f);
            }
        }
        Ok(())
    }
}

pub fn translate(ast: ast::Module) -> Result<Module, TypeError> {
    let mut m = Module {
        types: HashMap::new(),
        funcs: HashMap::new(),
    };
    for def in ast.defs {
        m.define(def)?;
    }
    Ok(m)
}
