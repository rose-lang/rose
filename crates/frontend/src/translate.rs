use crate::{ast, tokens};
use enumset::EnumSet;
use indexmap::{IndexMap, IndexSet};
use rose::{self as ir, id};
use std::{collections::HashMap, ops::Range};

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
    IndexPrimitive { t: ir::Ty },

    #[error("")]
    IndexRef,

    #[error("")]
    IndexTuple,

    #[error("")]
    MemberPrimitive { t: ir::Ty },

    #[error("")]
    MemberRef,

    #[error("")]
    BadSwizzle { swizzle: Range<usize> },

    #[error("")]
    ForNonSize,

    #[error("")]
    BadBinaryArgs {
        op: tokens::Binop,
        left: ir::Ty,
        right: ir::Ty,
    },
}

fn parse_types<'input>(
    lookup: impl Fn(&'input str) -> Option<&Typedef>,
    types: &mut IndexSet<ir::Ty>,
    typenames: impl Iterator<Item = ast::Spanned<&'input str>>,
) -> Result<(HashMap<&'input str, id::Generic>, Vec<id::Ty>), TypeError> {
    let mut generics = HashMap::new();
    let mut params = vec![];
    for typ in typenames {
        params.push(if lookup(typ.val).is_some() {
            todo!()
        } else {
            let (real, _) = types.insert_full(ir::Ty::F64);
            let mut t = id::ty(real);
            let dims = typ
                .val
                .strip_prefix('R')
                .ok_or_else(|| TypeError::UnknownType { name: typ.span() })?;
            if !dims.is_empty() {
                for dim in dims.rsplit('x') {
                    let index = if let Ok(n) = dim.parse() {
                        ir::Ty::Fin { size: n }
                    } else if dim.is_empty() {
                        // TODO: change this condition to check for valid identifiers
                        return Err(TypeError::BadTensorType { name: typ.span() });
                    } else {
                        ir::Ty::Generic {
                            id: if let Some(&i) = generics.get(dim) {
                                i
                            } else {
                                let i = id::generic(generics.len());
                                generics.insert(dim, i);
                                i
                            },
                        }
                    };
                    let (i, _) = types.insert_full(index);
                    let typexpr = ir::Ty::Array {
                        elem: t,
                        index: id::ty(i),
                    };
                    let (id, _) = types.insert_full(typexpr);
                    t = id::ty(id);
                }
            }
            t
        });
    }
    Ok((generics, params))
}

#[derive(Debug)]
pub struct Typedef<'input> {
    pub types: Vec<ir::Ty>,
    /// Sorted lexicographically.
    pub fields: IndexMap<&'input str, id::Ty>,
}

#[derive(Debug)]
pub struct Module<'input> {
    types: IndexMap<&'input str, Typedef<'input>>,
    funcs: IndexMap<&'input str, rose::Function>,
}

#[derive(Clone, Copy, Debug)]
pub struct FuncRef<'input, 'a> {
    m: &'a Module<'input>,
    id: id::Function,
}

impl<'input, 'a> rose::FuncNode for FuncRef<'input, 'a> {
    fn def(&self) -> &rose::Function {
        &self.m.funcs[self.id.function()]
    }

    fn get(&self, id: id::Function) -> Option<Self> {
        Some(Self { m: self.m, id })
    }
}

impl Module<'_> {
    pub fn get_type(&self, name: &str) -> Option<&Typedef> {
        self.types.get(name)
    }

    pub fn get_func(&self, name: &str) -> Option<FuncRef> {
        let i = self.funcs.get_index_of(name)?;
        Some(FuncRef {
            m: self,
            id: id::function(i),
        })
    }
}

struct BlockCtx<'input, 'a> {
    m: &'a Module<'input>,
    g: HashMap<&'input str, id::Generic>,
    l: HashMap<&'input str, id::Var>,
    t: IndexSet<ir::Ty>,
    v: Vec<id::Ty>,
    c: Vec<ir::Instr>,
}

impl<'input, 'a> BlockCtx<'input, 'a> {
    fn newtype(&mut self, t: ir::Ty) -> id::Ty {
        let (i, _) = self.t.insert_full(t);
        id::ty(i)
    }

    fn newlocal(&mut self, t: id::Ty) -> id::Var {
        let id = id::var(self.v.len());
        self.v.push(t);
        id
    }

    fn gettype(&self, id: id::Ty) -> &ir::Ty {
        &self.t[id.ty()]
    }

    fn getlocal(&self, id: id::Var) -> id::Ty {
        self.v[id.var()]
    }

    fn instr(&mut self, t: id::Ty, expr: ir::Expr) -> id::Var {
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
        _args: &[id::Ty],
    ) -> Result<(Vec<id::Ty>, id::Ty), TypeError> {
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
                        let index = self.newtype(ir::Ty::Fin { size: vars.len() });
                        let ty = self.newtype(ir::Ty::Array {
                            index,
                            elem: self.getlocal(x),
                        });
                        Ok(self.instr(ty, ir::Expr::Array { elems: vars.into() }))
                    }
                    None => Err(TypeError::EmptyVec),
                }
            }
            ast::Expr::Struct { members: _ } => todo!(),
            ast::Expr::Index { val, index } => {
                let v = self.typecheck(*val)?;
                let i = self.typecheck(*index)?; // TODO: check index type
                let t = match self.gettype(self.getlocal(v)) {
                    ir::Ty::Ref { .. } => Err(TypeError::IndexRef),
                    &ir::Ty::Array { elem, .. } => Ok(elem),
                    ir::Ty::Tuple { .. } => Err(TypeError::IndexTuple),
                    t => Err(TypeError::IndexPrimitive { t: t.clone() }),
                }?;
                Ok(self.instr(t, ir::Expr::Index { array: v, index: i }))
            }
            ast::Expr::Member { val, member } => {
                let tuple = self.typecheck(*val)?;
                match self.gettype(self.getlocal(tuple)) {
                    ir::Ty::Ref { .. } => Err(TypeError::MemberRef),
                    ir::Ty::Array { index: _, elem } => {
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
                    ir::Ty::Tuple { members: _ } => todo!(),
                    t => Err(TypeError::MemberPrimitive { t: t.clone() }),
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
                    .collect::<Result<Box<[id::Var]>, TypeError>>()?;
                let types: Vec<id::Ty> = vars.iter().map(|&v| self.getlocal(v)).collect();
                if let Some((i, _, f)) = self.m.funcs.get_full(func.val) {
                    let (generics, ret) = self.unify(f, &types)?;
                    Ok(self.instr(
                        ret,
                        ir::Expr::Call {
                            id: id::function(i),
                            generics: generics.into(),
                            args: vars,
                        },
                    ))
                } else {
                    let real = self.newtype(ir::Ty::F64);
                    // TODO: validate argument types for builtin functions
                    match func.val {
                        "abs" => Ok(self.instr(
                            real,
                            ir::Expr::Unary {
                                op: ir::Unop::Abs,
                                arg: vars[0],
                            },
                        )),
                        "sqrt" => Ok(self.instr(
                            real,
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

                // IR doesn't currently support branching, so just evaluate both branches
                let t = self.typecheck(*then)?;
                let e = self.typecheck(*els)?;

                Ok(self.instr(
                    self.getlocal(t), // TODO: ensure this matches the type of `e`
                    ir::Expr::Select {
                        cond: c,
                        then: t,
                        els: e,
                    },
                ))
            }
            ast::Expr::For { index, limit, body } => {
                let &id = self
                    .g
                    .get(limit.val)
                    .ok_or_else(|| TypeError::UnknownVar { name: limit.span() })?;
                let i = self.newtype(ir::Ty::Generic { id });
                let code = std::mem::take(&mut self.c);

                let arg = self.newlocal(i);
                self.l.insert(index, arg);
                let elem = self.typecheck(*body)?;
                let body = std::mem::replace(&mut self.c, code).into_boxed_slice();

                let v = self.newtype(ir::Ty::Array {
                    index: i,
                    elem: self.getlocal(elem),
                });
                Ok(self.instr(
                    v,
                    ir::Expr::For {
                        index: i,
                        arg,
                        body,
                        ret: elem,
                    },
                ))
            }
            ast::Expr::Unary { op: _, arg: _ } => todo!(),
            ast::Expr::Binary { op, left, right } => {
                let real = self.newtype(ir::Ty::F64);
                let l = self.typecheck(*left)?;
                let r = self.typecheck(*right)?;
                let (t, binop) = match op {
                    tokens::Binop::Add => (real, ir::Binop::Add),
                    tokens::Binop::Sub => (real, ir::Binop::Sub),
                    tokens::Binop::Mul => (real, ir::Binop::Mul),
                    tokens::Binop::Div => (real, ir::Binop::Div),
                    tokens::Binop::Mod => todo!(),
                    tokens::Binop::Eq => (real, ir::Binop::Eq),
                    tokens::Binop::Neq => (real, ir::Binop::Neq),
                    tokens::Binop::Lt => (real, ir::Binop::Lt),
                    tokens::Binop::Gt => (real, ir::Binop::Gt),
                    tokens::Binop::Leq => (real, ir::Binop::Leq),
                    tokens::Binop::Geq => (real, ir::Binop::Geq),
                    tokens::Binop::And => (real, ir::Binop::And),
                    tokens::Binop::Or => (real, ir::Binop::Or),
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
                let mut typevars = IndexSet::new();
                // TODO: check for duplicate field names
                members.sort_by_key(|&(name, _)| name); // to ignore field order in literals
                let (_, fields) = parse_types(
                    |s| self.types.get(s),
                    &mut typevars,
                    members.iter().map(|&(_, t)| t),
                )?;
                // TODO: check for duplicate type names
                self.types.insert(
                    name,
                    Typedef {
                        types: typevars.into_iter().collect(),
                        fields: members
                            .into_iter()
                            .map(|(_, t)| t.val)
                            .zip(fields)
                            .collect(),
                    },
                );
            }
            ast::Def::Func {
                name,
                params,
                typ,
                body,
            } => {
                let mut typevars = IndexSet::new();
                let (genericnames, mut paramtypes) = parse_types(
                    |s| self.types.get(s),
                    &mut typevars,
                    // TODO: handle return type separately from params w.r.t. generics
                    params.iter().map(|&(_, t)| t).chain([typ]),
                )?;
                let generics =
                    vec![EnumSet::only(ir::Constraint::Index); genericnames.len()].into();
                paramtypes.pop().expect("`parse_types` should preserve len"); // pop off return type
                let args = (0..params.len()).map(id::var).collect();
                let mut ctx = BlockCtx {
                    m: self,
                    g: genericnames,
                    l: HashMap::new(),
                    t: typevars,
                    v: paramtypes,
                    c: vec![],
                };
                for (i, (bind, _)) in params.into_iter().enumerate() {
                    ctx.bind(bind, id::var(i));
                }
                let retvar = ctx.typecheck(body)?; // TODO: ensure this matches `ret`
                let f = ir::Function {
                    generics,
                    types: ctx.t.into_iter().collect(),
                    vars: ctx.v.into(),
                    params: args,
                    ret: retvar,
                    body: ctx.c.into(),
                };
                // TODO: check for duplicate function names
                self.funcs.insert(name, f);
            }
        }
        Ok(())
    }
}

pub fn translate(ast: ast::Module) -> Result<Module, TypeError> {
    let mut m = Module {
        types: IndexMap::new(),
        funcs: IndexMap::new(),
    };
    for def in ast.defs {
        m.define(def)?;
    }
    Ok(m)
}
