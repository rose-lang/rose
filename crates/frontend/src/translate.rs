use crate::{ast, tokens};
use enumset::EnumSet;
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
    IndexRef,

    #[error("")]
    IndexTuple,

    #[error("")]
    MemberPrimitive { t: ir::Type },

    #[error("")]
    MemberRef,

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
    lookup: impl Fn(&'input str) -> Option<Rc<ir::Typedef>>,
    typenames: impl Iterator<Item = ast::Spanned<&'input str>>,
) -> Result<ParsedTypes<'input>, TypeError> {
    let mut generics = HashMap::new();
    let mut typevars = vec![];
    let mut types = vec![];
    for typ in typenames {
        types.push(if let Some(def) = lookup(typ.val) {
            let typexpr = ir::Typexpr::Def {
                def,
                params: vec![],
            };
            let id = id::typexpr(typevars.len());
            typevars.push(typexpr);
            ir::Type::Expr { id }
        } else {
            let mut t = ir::Type::F64;
            let dims = typ
                .val
                .strip_prefix('R')
                .ok_or_else(|| TypeError::UnknownType { name: typ.span() })?;
            if !dims.is_empty() {
                for dim in dims.rsplit('x') {
                    let typexpr = ir::Typexpr::Array {
                        elem: t,
                        index: if let Ok(n) = dim.parse() {
                            ir::Type::Fin { size: n }
                        } else if dim.is_empty() {
                            // TODO: change this condition to check for valid identifiers
                            return Err(TypeError::BadTensorType { name: typ.span() });
                        } else {
                            ir::Type::Generic {
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
                    let id = id::typexpr(typevars.len());
                    typevars.push(typexpr);
                    t = ir::Type::Expr { id };
                }
            }
            t
        });
    }
    Ok((generics, typevars, types))
}

#[derive(Debug)]
pub struct Type<'input> {
    pub def: Rc<rose::Typedef>,
    /// Sorted lexicographically.
    pub fields: Vec<&'input str>,
}

#[derive(Debug)]
pub struct Module<'input> {
    pub types: HashMap<&'input str, Type<'input>>,
    pub funcs: HashMap<&'input str, Rc<rose::Function>>,
}

struct BlockCtx<'input, 'a> {
    m: &'a Module<'input>,
    g: HashMap<&'input str, id::Generic>,
    l: HashMap<&'input str, id::Var>,
    t: Vec<ir::Typexpr>,
    f: Vec<ir::Func>,
    v: Vec<ir::Type>,
    b: Vec<ir::Block>,
    c: Vec<ir::Instr>,
}

impl<'input, 'a> BlockCtx<'input, 'a> {
    fn newtype(&mut self, t: ir::Typexpr) -> id::Typexpr {
        let id = id::typexpr(self.t.len());
        self.t.push(t);
        id
    }

    fn newlocal(&mut self, t: ir::Type) -> id::Var {
        let id = id::var(self.v.len());
        self.v.push(t);
        id
    }

    fn newfunc(&mut self, f: ir::Func) -> id::Func {
        let id = id::func(self.f.len());
        self.f.push(f);
        id
    }

    fn gettype(&self, id: id::Typexpr) -> &ir::Typexpr {
        &self.t[id.typexpr()]
    }

    fn getlocal(&self, id: id::Var) -> ir::Type {
        self.v[id.var()]
    }

    fn instr(&mut self, t: ir::Type, expr: ir::Expr) -> id::Var {
        let var = self.newlocal(t);
        self.c.push(ir::Instr { var, expr });
        var
    }

    fn bind(&mut self, bind: ast::Bind<'input>, val: id::Var) {
        match bind {
            ast::Bind::Id { name } => {
                self.l.insert(name, val);
            }
            ast::Bind::Vector { elems: _ } => todo!(),
            ast::Bind::Struct { members: _ } => todo!(),
        }
    }

    fn unify(
        &mut self,
        _f: &ir::Function,
        _args: &[ir::Type],
    ) -> Result<(Vec<ir::Type>, ir::Type), TypeError> {
        todo!()
    }

    fn typecheck(&mut self, expr: ast::Expr<'input>) -> Result<id::Var, TypeError> {
        // TODO: prevent clobbering and don't let variable names escape their scope
        match expr {
            ast::Expr::Id { name } => self
                .l
                .get(name.val)
                .copied()
                .ok_or_else(|| TypeError::UnknownVar { name: name.span() }),
            ast::Expr::Int { val: _ } => todo!(), // the IR doesn't currently have `Int`
            ast::Expr::Vector { elems } => {
                let vars = elems
                    .into_iter()
                    .map(|elem| self.typecheck(elem))
                    .collect::<Result<Vec<id::Var>, TypeError>>()?;
                match vars.first() {
                    Some(&x) => {
                        let t = self.newtype(ir::Typexpr::Array {
                            index: ir::Type::Fin { size: vars.len() },
                            elem: self.getlocal(x),
                        });
                        Ok(self.instr(ir::Type::Expr { id: t }, ir::Expr::Array { elems: vars }))
                    }
                    None => Err(TypeError::EmptyVec),
                }
            }
            ast::Expr::Struct { members: _ } => todo!(),
            ast::Expr::Index { val, index } => {
                let v = self.typecheck(*val)?;
                let i = self.typecheck(*index)?; // TODO: check index type
                let t = match self.getlocal(v) {
                    ir::Type::Expr { id } => match self.gettype(id) {
                        ir::Typexpr::Ref { .. } => Err(TypeError::IndexRef),
                        ir::Typexpr::Array { elem, .. } => Ok(*elem),
                        ir::Typexpr::Tuple { .. } => Err(TypeError::IndexTuple),
                        ir::Typexpr::Def { def: _, params: _ } => todo!(),
                    },
                    t => Err(TypeError::IndexPrimitive { t }),
                }?;
                Ok(self.instr(t, ir::Expr::Index { array: v, index: i }))
            }
            ast::Expr::Member { val, member } => {
                let tuple = self.typecheck(*val)?;
                match self.getlocal(tuple) {
                    ir::Type::Expr { id } => match self.gettype(id) {
                        ir::Typexpr::Ref { .. } => Err(TypeError::MemberRef),
                        ir::Typexpr::Array { index: _, elem } => {
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
                            Ok(self.instr(
                                *elem,
                                ir::Expr::Member {
                                    tuple,
                                    member: id::member(i),
                                },
                            ))
                        }
                        ir::Typexpr::Tuple { members: _ } => todo!(),
                        ir::Typexpr::Def { def: _, params: _ } => todo!(),
                    },
                    t => Err(TypeError::MemberPrimitive { t }),
                }
            }
            ast::Expr::Let { bind, val, body } => {
                let var = self.typecheck(*val)?;
                self.bind(bind, var);
                self.typecheck(*body)
            }
            ast::Expr::Call { func, args } => {
                let vars = args
                    .into_iter()
                    .map(|elem| self.typecheck(elem))
                    .collect::<Result<Vec<id::Var>, TypeError>>()?;
                let types: Vec<ir::Type> = vars.iter().map(|&v| self.getlocal(v)).collect();
                if let Some(def) = self.m.funcs.get(func.val) {
                    let (generics, ret) = self.unify(def, &types)?;
                    let func = self.newfunc(ir::Func {
                        def: Rc::clone(def),
                        generics,
                    });
                    let t = self.newtype(ir::Typexpr::Tuple { members: types });
                    let arg =
                        self.instr(ir::Type::Expr { id: t }, ir::Expr::Tuple { members: vars });
                    Ok(self.instr(ret, ir::Expr::Call { func, arg }))
                } else {
                    // TODO: validate argument types for builtin functions
                    match func.val {
                        "abs" => Ok(self.instr(
                            ir::Type::F64,
                            ir::Expr::Unary {
                                op: ir::Unop::Abs,
                                arg: vars[0],
                            },
                        )),
                        "sqrt" => Ok(self.instr(
                            ir::Type::F64,
                            ir::Expr::Unary {
                                op: ir::Unop::Abs,
                                arg: vars[0],
                            },
                        )),
                        _ => Err(TypeError::UnknownFunc { name: func.span() }),
                    }
                }
            }
            ast::Expr::If { cond, then, els } => {
                let c = self.typecheck(*cond)?; // TODO: ensure this is `Bool`
                let code = std::mem::take(&mut self.c);

                let arg_then = self.newlocal(ir::Type::Unit);
                let ret_then = self.typecheck(*then)?;
                let block_then = id::block(self.b.len());
                let code_then = std::mem::take(&mut self.c);
                self.b.push(ir::Block {
                    arg: arg_then,
                    code: code_then,
                    ret: ret_then,
                });

                let arg_els = self.newlocal(ir::Type::Unit);
                let ret_els = self.typecheck(*els)?;
                let block_els = id::block(self.b.len());
                let code_els = std::mem::replace(&mut self.c, code);
                self.b.push(ir::Block {
                    arg: arg_els,
                    code: code_els,
                    ret: ret_els,
                });

                Ok(self.instr(
                    self.getlocal(ret_then), // TODO: ensure this matches the type of `ret_els`
                    ir::Expr::If {
                        cond: c,
                        then: block_then,
                        els: block_els,
                    },
                ))
            }
            ast::Expr::For { index, limit, body } => {
                let &id = self
                    .g
                    .get(limit.val)
                    .ok_or_else(|| TypeError::UnknownVar { name: limit.span() })?;
                let i = ir::Type::Generic { id };
                let code = std::mem::take(&mut self.c);

                let arg = self.newlocal(i);
                self.l.insert(index, arg);
                let elem = self.typecheck(*body)?;
                let body = id::block(self.b.len());
                let code_for = std::mem::replace(&mut self.c, code);
                self.b.push(ir::Block {
                    arg,
                    code: code_for,
                    ret: elem,
                });

                let v = self.newtype(ir::Typexpr::Array {
                    index: i,
                    elem: self.getlocal(elem),
                });
                Ok(self.instr(ir::Type::Expr { id: v }, ir::Expr::For { index: i, body }))
            }
            ast::Expr::Unary { op: _, arg: _ } => todo!(),
            ast::Expr::Binary { op, left, right } => {
                let l = self.typecheck(*left)?;
                let r = self.typecheck(*right)?;
                let (t, binop) = match op {
                    tokens::Binop::Add => (ir::Type::F64, ir::Binop::Add),
                    tokens::Binop::Sub => (ir::Type::F64, ir::Binop::Sub),
                    tokens::Binop::Mul => (ir::Type::F64, ir::Binop::Mul),
                    tokens::Binop::Div => (ir::Type::F64, ir::Binop::Div),
                    tokens::Binop::Mod => todo!(),
                    tokens::Binop::Eq => (ir::Type::F64, ir::Binop::Eq),
                    tokens::Binop::Neq => (ir::Type::F64, ir::Binop::Neq),
                    tokens::Binop::Lt => (ir::Type::F64, ir::Binop::Lt),
                    tokens::Binop::Gt => (ir::Type::F64, ir::Binop::Gt),
                    tokens::Binop::Leq => (ir::Type::F64, ir::Binop::Leq),
                    tokens::Binop::Geq => (ir::Type::F64, ir::Binop::Geq),
                    tokens::Binop::And => (ir::Type::F64, ir::Binop::And),
                    tokens::Binop::Or => (ir::Type::F64, ir::Binop::Or),
                };
                let expr = ir::Expr::Binary {
                    op: binop,
                    left: l,
                    right: r,
                };
                Ok(self.instr(t, expr))
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
                let (genericnames, mut typevars, fields) = parse_types(
                    |s| self.types.get(s).map(|Type { def, .. }| Rc::clone(def)),
                    members.iter().map(|&(_, t)| t),
                )?;
                let def = ir::Type::Expr {
                    id: id::typexpr(typevars.len()),
                };
                typevars.push(ir::Typexpr::Tuple { members: fields });
                let t = Rc::new(ir::Typedef {
                    generics: vec![EnumSet::only(ir::Constraint::Index); genericnames.len()],
                    types: typevars,
                    def,
                    // TODO: check constraints once the text syntax supports non-vector structs
                    constraints: EnumSet::only(ir::Constraint::Vector),
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
                let (genericnames, mut typevars, mut paramtypes) = parse_types(
                    |s| self.types.get(s).map(|Type { def, .. }| Rc::clone(def)),
                    // TODO: handle return type separately from params w.r.t. generics
                    params.iter().map(|&(_, t)| t).chain([typ]),
                )?;
                let generics = vec![EnumSet::only(ir::Constraint::Index); genericnames.len()];
                let ret = paramtypes.pop().expect("`parse_types` should preserve len");
                let param = ir::Type::Expr {
                    id: id::typexpr(typevars.len()),
                };
                typevars.push(ir::Typexpr::Tuple {
                    members: paramtypes.clone(), // should be a way to do this without `clone`...
                });
                let arg = id::var(0);
                let mut ctx = BlockCtx {
                    m: self,
                    g: genericnames,
                    l: HashMap::new(),
                    t: typevars,
                    f: vec![],
                    v: vec![param],
                    b: vec![],
                    c: vec![],
                };
                for (i, ((bind, _), t)) in params.into_iter().zip(paramtypes).enumerate() {
                    let expr = ir::Expr::Member {
                        tuple: arg,
                        member: id::member(i),
                    };
                    let var = ctx.instr(t, expr);
                    ctx.bind(bind, var);
                }
                let retvar = ctx.typecheck(body)?; // TODO: ensure this matches `ret`
                let main = id::block(ctx.b.len());
                ctx.b.push(ir::Block {
                    arg,
                    code: ctx.c,
                    ret: retvar,
                });
                let f = Rc::new(ir::Function {
                    generics,
                    types: ctx.t,
                    funcs: ctx.f,
                    param,
                    ret,
                    vars: ctx.v,
                    blocks: ctx.b,
                    main,
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
