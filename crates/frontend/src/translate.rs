use crate::ast;
use rose as ir;
use std::collections::HashMap;

// TODO: give source locations
#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("")]
    UnknownType,
    #[error("")]
    BadTensorType,
    #[error("")]
    UnknownVar,
    #[error("")]
    EmptyVec,
    #[error("")]
    UnknownFunc,
    #[error("")]
    IndexNonVector,
}

fn parse_types<'input>(
    lookup: impl Fn(&'input str) -> Option<ir::Typedef>,
    typenames: impl Iterator<Item = &'input str>,
) -> Result<
    (
        HashMap<&'input str, ir::Generic>,
        Vec<ir::Typexpr>,
        Vec<ir::Type>,
    ),
    TypeError,
> {
    let mut generics = HashMap::new();
    let mut typevars = vec![];
    let mut types = vec![];
    for typ in typenames {
        types.push(if let Some(id) = lookup(typ) {
            let typexpr = ir::Typexpr::Typedef { id, params: vec![] };
            let id = ir::Var(typevars.len());
            typevars.push(typexpr);
            ir::Type::Var { id }
        } else {
            let mut t = ir::Type::Real;
            let dims = typ.strip_prefix("R").ok_or(TypeError::UnknownType)?;
            if !dims.is_empty() {
                for dim in dims.rsplit("x") {
                    let typexpr = ir::Typexpr::Vector {
                        elem: t,
                        size: if let Ok(n) = dim.parse() {
                            ir::Size::Const { val: n }
                        } else if dim.is_empty() {
                            // TODO: change this condition to check for valid identifiers
                            return Err(TypeError::BadTensorType);
                        } else {
                            ir::Size::Generic {
                                id: if let Some(&i) = generics.get(dim) {
                                    i
                                } else {
                                    let i = ir::Generic(generics.len());
                                    generics.insert(dim, i);
                                    i
                                },
                            }
                        },
                    };
                    let id = ir::Var(typevars.len());
                    typevars.push(typexpr);
                    t = ir::Type::Var { id };
                }
            }
            t
        });
    }
    Ok((generics, typevars, types))
}

struct ModCtx<'input> {
    m: ir::Module,
    t: HashMap<&'input str, (ir::Typedef, Vec<&'input str>)>, // sorted field names
    f: HashMap<&'input str, ir::Defn>,
}

struct FunCtx<'input, 'a> {
    ctx: &'a ModCtx<'input>,
    f: ir::Function,
    t: Vec<ir::Typexpr>,
    g: HashMap<&'input str, ir::Generic>,
    l: HashMap<&'input str, ir::Local>,
}

impl<'input, 'a> FunCtx<'input, 'a> {
    fn newtype(&mut self, t: ir::Typexpr) -> ir::Var {
        let id = ir::Var(self.t.len());
        self.t.push(t);
        id
    }

    fn newlocal(&mut self, t: ir::Type) -> ir::Local {
        let id = ir::Local(self.f.locals.len());
        self.f.locals.push(t);
        id
    }

    fn newfunc(&mut self, f: ir::Inst) -> ir::Func {
        let id = ir::Func(self.f.funcs.len());
        self.f.funcs.push(f);
        id
    }

    fn gettype(&self, id: ir::Var) -> &ir::Typexpr {
        &self.t[id.0]
    }

    fn getlocal(&self, id: ir::Local) -> ir::Type {
        self.f.locals[id.0]
    }

    fn unify(
        &mut self,
        f: ir::Defn,
        args: &[ir::Type],
    ) -> Result<(Vec<ir::Size>, ir::Type), TypeError> {
        todo!()
    }

    fn typecheck(&mut self, expr: ast::Expr<'input>) -> Result<ir::Type, TypeError> {
        // TODO: prevent clobbering and don't let variable names escape their scope
        match expr {
            ast::Expr::Id { name } => {
                if let Some(&id) = self.l.get(name) {
                    self.f.body.push(ir::Instr::Get { id });
                    Ok(self.getlocal(id))
                } else if let Some(&id) = self.g.get(name) {
                    self.f.body.push(ir::Instr::Generic { id });
                    Ok(ir::Type::Int)
                } else {
                    Err(TypeError::UnknownVar)
                }
            }
            ast::Expr::Int { val } => {
                self.f.body.push(ir::Instr::Int { val });
                Ok(ir::Type::Int)
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
            ast::Expr::Struct { members } => todo!(),
            ast::Expr::Index { val, index } => {
                let v = self.typecheck(*val)?;
                self.typecheck(*index)?; // TODO: ensure this is `Int` and bounded by `val` size
                self.f.body.push(ir::Instr::Index);
                match v {
                    ir::Type::Var { id } => match self.gettype(id) {
                        ir::Typexpr::Vector { elem, .. } => Ok(*elem),
                        _ => return Err(TypeError::IndexNonVector),
                    },
                    _ => Err(TypeError::IndexNonVector),
                }
            }
            ast::Expr::Member { val, member } => todo!(),
            ast::Expr::Let { bind, val, body } => {
                let t = self.typecheck(*val)?;
                let id = self.newlocal(t);
                self.f.body.push(ir::Instr::Set { id });
                match bind {
                    ast::Bind::Id { name } => {
                        self.l.insert(name, id);
                    }
                    ast::Bind::Vector { elems } => todo!(),
                    ast::Bind::Struct { members } => todo!(),
                }
                self.typecheck(*body)
            }
            ast::Expr::Call { func, args } => {
                let mut types = vec![];
                for arg in args {
                    types.push(self.typecheck(arg)?);
                }
                if let Some(&id) = self.ctx.f.get(func) {
                    let (params, ret) = self.unify(id, &types)?;
                    let id = self.newfunc(ir::Inst { id, params });
                    self.f.body.push(ir::Instr::Call { id });
                    Ok(ret)
                } else {
                    match func {
                        "abs" => todo!(),
                        "max" => todo!(),
                        "min" => todo!(),
                        "prod" => todo!(),
                        "sqrt" => todo!(),
                        "sum" => todo!(),
                        _ => Err(TypeError::UnknownFunc),
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
            ast::Expr::For { index, limit, body } => todo!(),
            ast::Expr::Unary { op, arg } => todo!(),
            ast::Expr::Binary { op, left, right } => todo!(),
        }
    }
}

impl<'input> ModCtx<'input> {
    fn newtype(&mut self, t: ir::Def<ir::Typexpr>) -> ir::Typedef {
        let id = ir::Typedef(self.m.types.len());
        self.m.types.push(t);
        id
    }

    fn newfunc(&mut self, f: ir::Def<ir::Function>) -> ir::Defn {
        let id = ir::Defn(self.m.funcs.len());
        self.m.funcs.push(f);
        id
    }

    fn define(&mut self, def: ast::Def<'input>) -> Result<(), TypeError> {
        match def {
            ast::Def::Type { name, mut members } => {
                // TODO: check for duplicate field names
                members.sort(); // easiest way to ignore field order in literals
                let (genericnames, typevars, fields) = parse_types(
                    |s| self.t.get(s).map(|&(i, _)| i),
                    members.iter().map(|&(_, t)| t),
                )?;
                let t = self.newtype(ir::Def {
                    generics: genericnames.len(),
                    types: typevars,
                    def: ir::Typexpr::Tuple { members: fields },
                });
                // TODO: check for duplicate type names
                self.t.insert(
                    name,
                    (
                        t,
                        members
                            .into_iter()
                            .map(|(name, _)| name)
                            .collect::<Vec<_>>(),
                    ),
                );
            }
            ast::Def::Func {
                name,
                params,
                typ,
                body,
            } => {
                let (genericnames, typevars, mut params) = parse_types(
                    |s| self.t.get(s).map(|&(i, _)| i),
                    // TODO: handle return type separately from params w.r.t. generics
                    params.iter().map(|&(_, t)| t).chain([typ]),
                )?;
                let ret = params.pop().expect("`parse_types` should preserve len");
                let mut ctx = FunCtx {
                    ctx: self,
                    f: ir::Function {
                        params,
                        ret: vec![ret],
                        locals: vec![],
                        funcs: vec![],
                        body: vec![],
                    },
                    t: typevars,
                    g: genericnames,
                    l: HashMap::new(),
                };
                ctx.typecheck(body)?; // TODO: ensure this matches `ret`
                let f = self.newfunc(ir::Def {
                    generics: ctx.g.len(),
                    types: ctx.t,
                    def: ctx.f,
                });
                // TODO: check for duplicate function names
                self.f.insert(name, f);
            }
        }
        Ok(())
    }
}

pub fn translate(ast: ast::Module) -> Result<ir::Module, TypeError> {
    let mut ctx = ModCtx {
        m: ir::Module {
            types: vec![],
            funcs: vec![],
        },
        f: HashMap::new(),
        t: HashMap::new(),
    };
    for def in ast.defs {
        ctx.define(def)?;
    }
    Ok(ctx.m)
}
