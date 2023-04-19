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
    lookup: impl Fn(&'input str) -> Option<ir::Struct>,
    types: impl Iterator<Item = &'input str>,
) -> Result<(HashMap<&'input str, ir::Generic>, Vec<ir::Type>), TypeError> {
    let mut generics = HashMap::new();
    let mut fields = vec![];
    for typ in types {
        fields.push(if let Some(id) = lookup(typ) {
            ir::Type::Struct { id, params: vec![] }
        } else {
            let mut t = ir::Type::Real;
            let dims = typ.strip_prefix("R").ok_or(TypeError::UnknownType)?;
            if !dims.is_empty() {
                for dim in dims.rsplit("x") {
                    t = ir::Type::Vector {
                        elem: Box::new(t),
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
                }
            }
            t
        });
    }
    Ok((generics, fields))
}

// TODO: structurally unify structs
fn unify(params: &[ir::Type], args: &[ir::Type]) -> Result<Vec<ir::Size>, TypeError> {
    todo!()
}

fn rewrite(typ: &ir::Type, generics: &[ir::Size]) -> ir::Type {
    todo!()
}

struct Context<'a> {
    typenames: &'a HashMap<&'a str, (ir::Struct, HashMap<&'a str, ir::Member>)>,
    types: &'a [ir::Typedef],

    funcnames: &'a HashMap<&'a str, ir::Func>,
    funcs: &'a [ir::Function],

    genericnames: HashMap<&'a str, ir::Generic>,

    localnames: HashMap<&'a str, ir::Local>,
    locals: Vec<ir::Type>,
    body: Vec<ir::Instr>,
}

impl<'a> Context<'a> {
    fn typecheck(&mut self, expr: ast::Expr<'a>) -> Result<ir::Type, TypeError> {
        // TODO: don't let variable names escape their scope
        match expr {
            ast::Expr::Id { name } => {
                if let Some(&id) = self.localnames.get(name) {
                    self.body.push(ir::Instr::Get { id });
                    Ok(self.locals[id.0].clone())
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
                    Some(elem) => Ok(ir::Type::Vector {
                        elem: Box::new(elem),
                        size: ir::Size::Const { val: n },
                    }),
                    None => Err(TypeError::EmptyVec),
                }
            }
            ast::Expr::Struct { members } => todo!(),
            ast::Expr::Index { val, index } => {
                let v = self.typecheck(*val)?;
                self.typecheck(*index)?; // TODO: ensure this is `Int` and bounded by `val` size
                self.body.push(ir::Instr::Index);
                match v {
                    ir::Type::Vector { elem, size: _ } => Ok(*elem),
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
                    let ir::Function { params, ret, .. } = &self.funcs[id.0];
                    let generics = unify(params, &types)?;
                    let t = rewrite(&ret[0], &generics); // TODO: assert only one return value
                    self.body.push(ir::Instr::Call { id, generics });
                    Ok(t)
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
            ast::Def::Type { name, members } => {
                let (genericnames, fields) = parse_types(
                    |s| typenames.get(s).map(|&(i, _)| i),
                    members.iter().map(|&(_, t)| t),
                )?;
                typenames.insert(
                    name,
                    (
                        ir::Struct(types.len()),
                        members
                            .into_iter()
                            .enumerate()
                            .map(|(i, (name, _))| (name, ir::Member(i)))
                            .collect::<HashMap<_, _>>(),
                    ),
                );
                types.push(ir::Typedef {
                    generics: genericnames.len(),
                    fields,
                });
            }
            ast::Def::Func {
                name,
                params,
                typ,
                body,
            } => {
                let (genericnames, mut paramtypes) = parse_types(
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

                    localnames: HashMap::new(),
                    locals: vec![],
                    body: vec![],
                };
                ctx.typecheck(body)?;
                let Context {
                    locals,
                    body: instrs,
                    ..
                } = ctx;

                funcnames.insert(name, ir::Func(funcs.len()));
                funcs.push(ir::Function {
                    generics,
                    params: paramtypes,
                    ret: vec![ret],
                    locals,
                    body: instrs,
                });
            }
        }
    }

    Ok(ir::Module { types, funcs })
}
