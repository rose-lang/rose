use crate::ast;
use rosebush as ir;
use std::collections::HashMap;

// TODO: give source locations
#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("")]
    UnknownType,
    #[error("")]
    BadTensorType,
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
                            ir::Size::Const(n)
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

                let mut localnames = HashMap::new();
                let mut locals = vec![];
                let mut body = vec![];

                for (name, id) in genericnames {
                    let target = ir::Local(locals.len());
                    localnames.insert(name, target);
                    locals.push(ir::Type::Int);
                    body.push(ir::Instr {
                        target,
                        value: ir::Expr::Generic { id },
                    });
                }

                for (i, (param, _)) in params.into_iter().enumerate() {
                    match param {
                        ast::Bind::Id { name } => {
                            let target = ir::Local(locals.len());
                            localnames.insert(name, target);
                            locals.push(paramtypes[i].clone());
                            body.push(ir::Instr {
                                target,
                                value: ir::Expr::Param { id: ir::Param(i) },
                            });
                        }
                        ast::Bind::Vector { .. } => todo!(),
                        ast::Bind::Struct { .. } => todo!(),
                    }
                }

                // TODO: translate the body

                funcnames.insert(name, funcs.len());
                funcs.push(ir::Function {
                    generics,
                    params: paramtypes,
                    ret,
                    locals,
                    body,
                });
            }
        }
    }

    Ok(ir::Module { types, funcs })
}
