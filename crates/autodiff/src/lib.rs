use enumset::EnumSet;
use indexmap::IndexSet;
use rose::{id, Binop, Block, Constraint, Expr, FuncNode, Function, Instr, Ty, Unop};
use std::collections::HashMap;

pub struct Derivative {
    f: Function,
}

pub struct Linear;

/// Guaranteed not to panic if `f` is valid.
pub fn derivative(f: impl FuncNode) -> Derivative {
    Derivative { f: f.def().clone() }
}

struct Forward<'a> {
    generics: Vec<EnumSet<Constraint>>,
    types: IndexSet<Ty>,
    old_types: Vec<(id::Ty, id::Ty)>,
    old_vars: &'a [id::Ty],
    vars: Vec<id::Ty>,
    mapping: Vec<Option<(id::Var, id::Var)>>,
    old_blocks: &'a [Block],
    blocks: Vec<Block>,
    block_mapping: HashMap<id::Block, id::Block>,
}

impl Forward<'_> {
    fn primal(&self, ty: id::Ty) -> id::Ty {
        let (t, _) = self.old_types[ty.ty()];
        t
    }

    fn tangent(&self, ty: id::Ty) -> id::Ty {
        let (_, t) = self.old_types[ty.ty()];
        t
    }

    fn newtype(&mut self, ty: Ty) -> id::Ty {
        let (i, _) = self.types.insert_full(ty);
        id::ty(i)
    }

    fn newvar(&mut self, t: id::Ty) -> id::Var {
        let id = id::var(self.vars.len());
        self.vars.push(t);
        id
    }

    fn unitvar(&mut self) -> id::Var {
        let ty = self.newtype(Ty::Unit);
        self.newvar(ty)
    }

    fn map(&self, var: id::Var) -> (id::Var, id::Var) {
        self.mapping[var.var()].unwrap()
    }

    fn set(&mut self, code: &mut Vec<Instr>, ty: id::Ty, expr: Expr) -> id::Var {
        let var = self.newvar(ty);
        code.push(Instr { var, expr });
        var
    }

    fn expr(
        &mut self,
        code: &mut Vec<Instr>,
        ty: id::Ty,
        tan: id::Ty,
        expr: &Expr,
    ) -> (id::Var, id::Var) {
        match expr {
            Expr::Unit => {
                let x = self.set(code, ty, Expr::Unit);
                (x, x)
            }
            &Expr::Bool { val } => {
                let x = self.set(code, ty, Expr::Bool { val });
                let dx = self.set(code, tan, Expr::Unit);
                (x, dx)
            }
            &Expr::F64 { val } => {
                let x = self.set(code, ty, Expr::F64 { val });
                let dx = self.set(code, ty, Expr::F64 { val: 0. });
                (x, dx)
            }
            &Expr::Fin { val } => {
                let x = self.set(code, ty, Expr::Fin { val });
                let dx = self.set(code, tan, Expr::Unit);
                (x, dx)
            }

            Expr::Array { elems } => {
                let (xs, dxs) = elems.iter().map(|&elem| self.map(elem)).unzip();
                let x = self.set(code, ty, Expr::Array { elems: xs });
                let dx = self.set(code, tan, Expr::Array { elems: dxs });
                (x, dx)
            }
            Expr::Tuple { members } => {
                let (xs, dxs) = members.iter().map(|&member| self.map(member)).unzip();
                let x = self.set(code, ty, Expr::Tuple { members: xs });
                let dx = self.set(code, tan, Expr::Tuple { members: dxs });
                (x, dx)
            }

            &Expr::Index { array, index } => {
                let (xs, dxs) = self.map(array);
                let (i, _) = self.map(index);
                let x = self.set(
                    code,
                    ty,
                    Expr::Index {
                        array: xs,
                        index: i,
                    },
                );
                let dx = self.set(
                    code,
                    tan,
                    Expr::Index {
                        array: dxs,
                        index: i,
                    },
                );
                (x, dx)
            }
            &Expr::Member { tuple, member } => {
                let (z, dz) = self.map(tuple);
                let x = self.set(code, ty, Expr::Member { tuple: z, member });
                let dx = self.set(code, tan, Expr::Member { tuple: dz, member });
                (x, dx)
            }

            Expr::Slice { array: _, index: _ } => todo!(),
            Expr::Field { tuple: _, field: _ } => todo!(),

            &Expr::Unary { op, arg } => {
                let (y, dy) = self.map(arg);
                match op {
                    Unop::Not => {
                        let x = self.set(code, ty, Expr::Unary { op, arg: y });
                        let dx = self.set(code, tan, Expr::Unit);
                        (x, dx)
                    }

                    Unop::Neg => {
                        let x = self.set(code, ty, Expr::Unary { op, arg: y });
                        let dx = self.set(code, ty, Expr::Unary { op, arg: dy });
                        (x, dx)
                    }
                    Unop::Abs => {
                        let x = self.set(code, ty, Expr::Unary { op, arg: y });
                        let sign = self.set(
                            code,
                            ty,
                            Expr::Unary {
                                op: Unop::Sign,
                                arg: y,
                            },
                        );
                        let dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: dy,
                                right: sign,
                            },
                        );
                        (x, dx)
                    }
                    Unop::Sign => {
                        let x = self.set(code, ty, Expr::Unary { op, arg: y });
                        let dx = self.set(code, ty, Expr::F64 { val: 0. });
                        (x, dx)
                    }
                    Unop::Sqrt => {
                        let (y, dy) = self.map(arg);
                        let x = self.set(code, ty, Expr::Unary { op, arg: y });
                        let two = self.set(code, ty, Expr::F64 { val: 2. });
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: two,
                                right: x,
                            },
                        );
                        let dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Div,
                                left: dy,
                                right: z,
                            },
                        );
                        (x, dx)
                    }
                }
            }
            &Expr::Binary { op, left, right } => {
                let (x, dx) = self.map(left);
                let (y, dy) = self.map(right);
                match op {
                    Binop::And
                    | Binop::Or
                    | Binop::Iff
                    | Binop::Xor
                    | Binop::Neq
                    | Binop::Lt
                    | Binop::Leq
                    | Binop::Eq
                    | Binop::Gt
                    | Binop::Geq => {
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: x,
                                right: y,
                            },
                        );
                        let dz = self.set(code, tan, Expr::Unit);
                        (z, dz)
                    }

                    Binop::Add => {
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: x,
                                right: y,
                            },
                        );
                        let dz = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Add,
                                left: dx,
                                right: dy,
                            },
                        );
                        (z, dz)
                    }
                    Binop::Sub => {
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: x,
                                right: y,
                            },
                        );
                        let dz = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Sub,
                                left: dx,
                                right: dy,
                            },
                        );
                        (z, dz)
                    }
                    Binop::Mul => {
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: x,
                                right: y,
                            },
                        );
                        let a = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: dx,
                                right: y,
                            },
                        );
                        let b = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: dy,
                                right: x,
                            },
                        );
                        let dz = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Add,
                                left: a,
                                right: b,
                            },
                        );
                        (z, dz)
                    }
                    Binop::Div => {
                        let z = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: x,
                                right: y,
                            },
                        );
                        let a = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Div,
                                left: dx,
                                right: y,
                            },
                        );
                        let b = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Div,
                                left: z,
                                right: y,
                            },
                        );
                        let c = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: dy,
                                right: b,
                            },
                        );
                        let dz = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Sub,
                                left: a,
                                right: c,
                            },
                        );
                        (z, dz)
                    }
                }
            }

            Expr::Call { func: _, arg: _ } => todo!(),
            &Expr::If { cond, then, els } => {
                let (p, _) = self.map(cond);

                let then_block = &self.old_blocks[then.block()];
                let then_arg = self.unitvar();
                self.mapping[then_block.arg.var()] = Some((then_arg, then_arg));
                let then_id = self.block(then, then_arg, vec![]);

                let els_block = &self.old_blocks[els.block()];
                let els_arg = self.unitvar();
                self.mapping[els_block.arg.var()] = Some((els_arg, els_arg));
                let els_id = self.block(els, els_arg, vec![]);

                let then_ret = self.blocks[then_id.block()].ret;
                let tup_ret = self.vars[then_ret.var()];
                let tup = self.set(
                    code,
                    tup_ret,
                    Expr::If {
                        cond: p,
                        then: then_id,
                        els: els_id,
                    },
                );

                let (_, then_dx) = self.map(then_ret);
                let x = self.set(
                    code,
                    ty,
                    Expr::Member {
                        tuple: tup,
                        member: id::member(0),
                    },
                );
                let dx = self.set(
                    code,
                    self.vars[then_dx.var()],
                    Expr::Member {
                        tuple: tup,
                        member: id::member(1),
                    },
                );
                (x, dx)
            }
            &Expr::For { index: _, body: _ } => todo!(),
            Expr::Accum {
                var: _,
                vector: _,
                body: _,
            } => todo!(),

            Expr::Add {
                accum: _,
                addend: _,
            } => todo!(),
        }
    }

    fn block(&mut self, old_id: id::Block, arg: id::Var, mut code: Vec<Instr>) -> id::Block {
        // check if this block has already been processed
        if let Some(&new_id) = self.block_mapping.get(&old_id) {
            return new_id;
        }

        // otherwise, process the block
        let old = &self.old_blocks[old_id.block()];

        for Instr { var, expr } in &old.code {
            let i = var.var();
            let (ty, tan) = self.old_types[self.old_vars[i].ty()];
            let (x, dx) = self.expr(&mut code, ty, tan, expr);
            self.mapping[i] = Some((x, dx));
        }

        let (x_ret, dx_ret) = self.map(old.ret);
        let t_ret = self.vars[x_ret.var()];
        let tan_ret = self.vars[dx_ret.var()];
        let tup_ret = self.newtype(Ty::Tuple {
            members: vec![t_ret, tan_ret],
        });
        let ret = self.set(
            &mut code,
            tup_ret,
            Expr::Tuple {
                members: vec![x_ret, dx_ret],
            },
        );

        let id = id::block(self.blocks.len());
        self.blocks.push(Block { arg, code, ret });
        self.block_mapping.insert(old_id, id);
        id
    }
}

pub fn forward(f: Derivative) -> Function {
    let Derivative { f } = f;
    let mut g = Forward {
        generics: f.generics,
        types: IndexSet::new(),
        old_types: vec![],
        old_vars: &f.vars,
        vars: vec![],
        mapping: vec![None; f.vars.len()],
        old_blocks: &f.blocks,
        blocks: vec![],
        block_mapping: HashMap::new(),
    };
    for ty in &f.types {
        let primal = match ty {
            Ty::Unit => Ty::Unit,
            Ty::Bool => Ty::Bool,
            Ty::F64 => Ty::F64,
            &Ty::Fin { size } => Ty::Fin { size },
            &Ty::Generic { id } => Ty::Generic { id },
            Ty::Scope { id } => {
                let processed_block = g.block(*id, g.unitvar(), vec![]);
                Ty::Scope {
                    id: processed_block,
                }
            }
            &Ty::Ref { scope: _, inner: _ } => todo!(),
            &Ty::Array { index, elem } => Ty::Array {
                index: g.primal(index),
                elem: g.primal(elem),
            },
            Ty::Tuple { members } => Ty::Tuple {
                members: members.iter().map(|&member| g.primal(member)).collect(),
            },
        };
        let tangent = match ty {
            Ty::Unit | Ty::Bool | Ty::Fin { .. } | Ty::Scope { .. } => Ty::Unit,
            Ty::F64 => Ty::F64,
            Ty::Generic { id: _ } => todo!(),
            Ty::Ref { scope: _, inner: _ } => todo!(),
            &Ty::Array { index, elem } => Ty::Array {
                index: g.primal(index),
                elem: g.tangent(elem),
            },
            Ty::Tuple { members } => Ty::Tuple {
                members: members.iter().map(|&member| g.tangent(member)).collect(),
            },
        };
        let (p, _) = g.types.insert_full(primal);
        let (t, _) = g.types.insert_full(tangent);
        g.old_types.push((id::ty(p), id::ty(t)));
    }

    let old = &f.blocks[f.main.block()];
    let mut code = vec![];
    let (t_arg, tan_arg) = g.old_types[g.old_vars[old.arg.var()].ty()];
    let tup_arg = g.newtype(Ty::Tuple {
        members: vec![t_arg, tan_arg],
    });
    let arg = g.newvar(tup_arg);
    let x_arg = g.set(
        &mut code,
        t_arg,
        Expr::Member {
            tuple: arg,
            member: id::member(0),
        },
    );
    let dx_arg = g.set(
        &mut code,
        tan_arg,
        Expr::Member {
            tuple: arg,
            member: id::member(1),
        },
    );
    g.mapping[old.arg.var()] = Some((x_arg, dx_arg));
    let main = g.block(f.main, arg, code);

    let b = &g.blocks[main.block()];
    Function {
        generics: g.generics,
        types: g.types.into_iter().collect(),
        funcs: f.funcs,
        param: g.vars[b.arg.var()],
        ret: g.vars[b.ret.var()],
        vars: g.vars,
        blocks: g.blocks,
        main,
    }
}

pub fn unzip(f: Derivative) -> (Function, Linear) {
    (f.f, Linear) // TODO
}

pub fn transpose(f: Linear) -> Linear {
    f // TODO
}
