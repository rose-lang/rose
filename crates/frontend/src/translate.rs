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

struct Context<'a> {
    typenames: &'a HashMap<&'a str, (ir::Typedef, Vec<&'a str>)>,
    types: &'a [ir::Def<ir::Typexpr>],

    funcnames: &'a HashMap<&'a str, ir::Funcdef>,
    funcs: &'a [ir::Def<ir::Function>],

    genericnames: HashMap<&'a str, ir::Generic>,

    typevars: Vec<ir::Typexpr>,
    funcinsts: Vec<ir::Inst>,

    localnames: HashMap<&'a str, ir::Local>,
    locals: Vec<ir::Type>,
    body: Vec<ir::Instr>,
}

impl<'a> Context<'a> {
    fn unify(
        &mut self,
        f: ir::Funcdef,
        args: &[ir::Type],
    ) -> Result<(Vec<ir::Size>, ir::Type), TypeError> {
        todo!()
    }

    fn typecheck(&mut self, expr: ast::Expr<'a>) -> Result<ir::Type, TypeError> {
        // TODO: don't let variable names escape their scope
        match expr {
            ast::Expr::Id { name } => {
                if let Some(&id) = self.localnames.get(name) {
                    self.body.push(ir::Instr::Get { id });
                    Ok(self.locals[id.0])
                } else if let Some(&id) = self.genericnames.get(name) {
                    self.body.push(ir::Instr::Generic { id });
                    Ok(ir::Type::Int)
                } else {
                    Err(TypeError::UnknownVar)
                }
            }
            ast::Expr::Int { val } => {
                self.body.push(ir::Instr::Int { val });
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
                    Some(elem) => {
                        let id = ir::Var(self.typevars.len());
                        self.typevars.push(ir::Typexpr::Vector {
                            elem,
                            size: ir::Size::Const { val: n },
                        });
                        Ok(ir::Type::Var { id })
                    }
                    None => Err(TypeError::EmptyVec),
                }
            }
            ast::Expr::Struct { members } => todo!(),
            ast::Expr::Index { val, index } => {
                let v = self.typecheck(*val)?;
                self.typecheck(*index)?; // TODO: ensure this is `Int` and bounded by `val` size
                self.body.push(ir::Instr::Index);
                match v {
                    ir::Type::Var { id } => match &self.typevars[id.0] {
                        ir::Typexpr::Vector { elem, .. } => Ok(*elem),
                        _ => return Err(TypeError::IndexNonVector),
                    },
                    _ => Err(TypeError::IndexNonVector),
                }
            }
            ast::Expr::Member { val, member } => todo!(),
            ast::Expr::Let { bind, val, body } => {
                let t = self.typecheck(*val)?;
                let id = ir::Local(self.locals.len());
                self.locals.push(t);
                self.body.push(ir::Instr::Set { id });
                match bind {
                    ast::Bind::Id { name } => {
                        self.localnames.insert(name, id);
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
                if let Some(&id) = self.funcnames.get(func) {
                    let (params, ret) = self.unify(id, &types)?;
                    let f = ir::Func(self.funcinsts.len());
                    self.funcinsts.push(ir::Inst { id, params });
                    self.body.push(ir::Instr::Call { id: f });
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
                self.body.push(ir::Instr::If);
                let t = self.typecheck(*then)?;
                self.body.push(ir::Instr::Else);
                self.typecheck(*els)?; // TODO: ensure this matches `t`
                self.body.push(ir::Instr::End);
                Ok(t)
            }
            ast::Expr::For { index, limit, body } => todo!(),
            ast::Expr::Unary { op, arg } => todo!(),
            ast::Expr::Binary { op, left, right } => todo!(),
        }
    }
}

pub fn translate(ast: ast::Module) -> Result<ir::Module, TypeError> {
    let mut typenames = HashMap::new();
    let mut types = vec![];

    let mut funcnames = HashMap::new();
    let mut funcs = vec![];

    for def in ast.defs {
        match def {
            ast::Def::Type { name, mut members } => {
                // TODO: check for duplicate field names
                members.sort(); // easiest way to ignore field order in literals
                let (genericnames, typevars, fields) = parse_types(
                    |s| typenames.get(s).map(|&(i, _)| i),
                    members.iter().map(|&(_, t)| t),
                )?;
                typenames.insert(
                    name,
                    (
                        ir::Typedef(types.len()),
                        members
                            .into_iter()
                            .map(|(name, _)| name)
                            .collect::<Vec<_>>(),
                    ),
                );
                types.push(ir::Def {
                    generics: genericnames.len(),
                    types: typevars,
                    def: ir::Typexpr::Tuple { members: fields },
                });
            }
            ast::Def::Func {
                name,
                params,
                typ,
                body,
            } => {
                let (genericnames, typevars, mut paramtypes) = parse_types(
                    |s| typenames.get(s).map(|&(i, _)| i),
                    // TODO: handle return type separately from params w.r.t. generics
                    params.iter().map(|&(_, t)| t).chain([typ]),
                )?;
                let generics = genericnames.len();
                let ret = paramtypes.pop().expect("`parse_types` should preserve len");

                let mut ctx = Context {
                    typenames: &typenames,
                    types: &types,

                    funcnames: &funcnames,
                    funcs: &funcs,

                    genericnames,

                    typevars,
                    funcinsts: vec![],

                    localnames: HashMap::new(),
                    locals: vec![],
                    body: vec![],
                };
                ctx.typecheck(body)?;
                let Context {
                    locals,
                    typevars,
                    funcinsts,
                    body: instrs,
                    ..
                } = ctx;

                funcnames.insert(name, ir::Funcdef(funcs.len()));
                funcs.push(ir::Def {
                    generics,
                    types: typevars,
                    def: ir::Function {
                        params: paramtypes,
                        ret: vec![ret],
                        locals,
                        funcs: funcinsts,
                        body: instrs,
                    },
                });
            }
        }
    }

    Ok(ir::Module { types, funcs })
}
