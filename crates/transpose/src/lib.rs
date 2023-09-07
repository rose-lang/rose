use rose::{id, Binop, Constraint, Expr, Func, Instr, Ty, Unop};

struct Transpose<'a> {
    f: &'a Func,
    linear: id::Ty,
    fwd_block: Vec<Instr>,
    intermediate_tuple: id::Var,
    intermediate_member: id::Member,
    bwd_nonlinear: Vec<Instr>,
    bwd_linear: Vec<Instr>,
}

impl Transpose<'_> {
    fn block(&mut self, block: &[Instr]) {
        let mut members = vec![];
        for instr in block.iter() {
            let var = instr.var;
            self.instr(var, &instr.expr);
            let t = self.f.vars[var.var()];
            if t != self.linear && self.f.types[t.ty()] == Ty::F64 {
                members.push(var);
                self.bwd_nonlinear.push(Instr {
                    var,
                    expr: Expr::Member {
                        tuple: self.intermediate_tuple,
                        member: self.intermediate_member,
                    },
                });
                self.intermediate_member = id::member(self.intermediate_member.member() + 1);
            }
        }
        self.fwd_block.push(Instr {
            var: self.intermediate_tuple,
            expr: Expr::Tuple {
                members: members.into(),
            },
        });
    }

    fn instr(&mut self, var: id::Var, expr: &Expr) {
        match expr {
            Expr::Unit => self.fwd_block.push(Instr {
                var,
                expr: Expr::Unit,
            }),
            &Expr::Bool { val } => self.fwd_block.push(Instr {
                var,
                expr: Expr::Bool { val },
            }),
            &Expr::F64 { val } => {
                if self.f.vars[var.var()] == self.linear {
                    self.bwd_linear.push(Instr {
                        var,
                        expr: Expr::F64 { val },
                    })
                } else {
                    self.fwd_block.push(Instr {
                        var,
                        expr: Expr::F64 { val },
                    })
                }
            }
            &Expr::Fin { val } => self.fwd_block.push(Instr {
                var,
                expr: Expr::Fin { val },
            }),

            Expr::Array { elems } => todo!(),
            Expr::Tuple { members } => todo!(),

            Expr::Index { array, index } => todo!(),
            Expr::Member { tuple, member } => todo!(),

            Expr::Slice { array, index } => todo!(),
            Expr::Field { tuple, member } => todo!(),

            Expr::Unary { op, arg } => todo!(),
            Expr::Binary { op, left, right } => todo!(),
            Expr::Select { cond, then, els } => todo!(),

            Expr::Call { id, generics, args } => todo!(),
            Expr::For { arg, body, ret } => todo!(),
            Expr::Read {
                var,
                arg,
                body,
                ret,
            } => todo!(),
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => todo!(),

            Expr::Ask { var } => todo!(),
            Expr::Add { accum, addend } => todo!(),
        }
    }
}

/// Return two functions that make up the transpose of `f`.
///
/// `linear` must be the type index of an `F64` type in `f`.
pub fn transpose(f: &Func, linear: id::Ty) -> (Func, Func) {
    let mut tp = Transpose {
        f,
        linear,
        fwd_block: vec![],
        intermediate_tuple: id::var(0),     // TODO
        intermediate_member: id::member(0), // TODO
        bwd_nonlinear: vec![],
        bwd_linear: vec![],
    };

    tp.block(&f.body);

    let t_f64 = id::ty(f.types.len());

    let mut fwd_types = f.types.to_vec();
    fwd_types[linear.ty()] = Ty::Unit;
    fwd_types.push(Ty::F64);

    let mut bwd_types = f.types.to_vec();
    bwd_types.push(Ty::F64);

    let mut bwd_params: Vec<_> = f
        .params
        .iter()
        .map(|param| match &f.types[f.vars[param.var()].ty()] {
            Ty::Unit => todo!(),
            Ty::Bool => todo!(),
            Ty::F64 => todo!(),
            Ty::Fin { size } => todo!(),
            Ty::Generic { id } => todo!(),
            Ty::Scope { kind, id } => todo!(),
            Ty::Ref { scope, inner } => todo!(),
            Ty::Array { index, elem } => todo!(),
            Ty::Tuple { members } => todo!(),
        })
        .collect();

    let mut fwd_body = vec![];
    let mut bwd_body = vec![];

    let mut intermediates = vec![];

    for instr in f.body.iter() {
        let var = instr.var;
        let t = f.vars[var.var()];
        match &instr.expr {
            Expr::Unit => fwd_body.push(Instr {
                var,
                expr: Expr::Unit,
            }),
            &Expr::Bool { val } => fwd_body.push(Instr {
                var,
                expr: Expr::Bool { val },
            }),
            &Expr::F64 { val } => {
                if t == linear {
                    bwd_body.push(Instr {
                        var,
                        expr: Expr::F64 { val },
                    })
                } else {
                    fwd_body.push(Instr {
                        var,
                        expr: Expr::F64 { val },
                    })
                }
            }
            &Expr::Fin { val } => fwd_body.push(Instr {
                var,
                expr: Expr::Fin { val },
            }),

            Expr::Array { .. } => todo!(),
            Expr::Tuple { members } => todo!(),

            Expr::Index { .. } => todo!(),
            Expr::Member { tuple, member } => todo!(),

            Expr::Slice { .. } => todo!(),
            Expr::Field { .. } => todo!(),

            &Expr::Unary { op, arg } => match op {
                Unop::Not => fwd_body.push(Instr {
                    var,
                    expr: Expr::Unary { op: Unop::Not, arg },
                }),

                Unop::Neg => {
                    if f.vars[instr.var.var()] == linear {
                        bwd_body.push(Instr {
                            var,
                            expr: Expr::Unary { op: Unop::Neg, arg },
                        })
                    } else {
                        fwd_body.push(Instr {
                            var,
                            expr: Expr::Unary { op: Unop::Neg, arg },
                        })
                    }
                }
                Unop::Abs => todo!(),
                Unop::Sign => todo!(),
                Unop::Sqrt => todo!(),
            },
            &Expr::Binary { op, left, right } => match op {
                Binop::And => todo!(),
                Binop::Or => todo!(),
                Binop::Iff => todo!(),
                Binop::Xor => todo!(),

                Binop::Neq => todo!(),
                Binop::Lt => todo!(),
                Binop::Leq => todo!(),
                Binop::Eq => todo!(),
                Binop::Gt => todo!(),
                Binop::Geq => todo!(),

                Binop::Add => todo!(),
                Binop::Sub => todo!(),
                Binop::Mul => todo!(),
                Binop::Div => todo!(),
            },
            Expr::Select { .. } => todo!(),

            Expr::Call { .. } => todo!(),
            Expr::For { .. } => todo!(),
            Expr::Read { .. } => todo!(),
            Expr::Accum { .. } => todo!(),

            Expr::Ask { .. } => todo!(),
            Expr::Add { .. } => todo!(),
        }
        if t != linear && f.types[t.ty()] == Ty::F64 {
            fwd_body.push(Instr {
                var: id::var(fwd_types.len() - 1),
                expr: Expr::F64 { val: 1.0 },
            });
        }
    }

    let t_tuple = fwd_types.len(); // or `bwd_types.len()`, shouldn't matter
    let members = intermediates.into_boxed_slice();
    fwd_types.push(Ty::Tuple {
        members: members.clone(),
    });
    bwd_types.push(Ty::Tuple { members });

    (
        Func {
            generics: f.generics.clone(),
            types: fwd_types.into(),
            vars: [].into(), // TODO
            params: f.params.clone(),
            ret: id::var(0), // TODO
            body: fwd_body.into(),
        },
        Func {
            generics: f
                .generics
                .iter()
                .map(|&before| {
                    let mut after = before;
                    if before.contains(Constraint::Read) {
                        after.remove(Constraint::Read);
                        after.insert(Constraint::Accum);
                    }
                    if before.contains(Constraint::Accum) {
                        after.remove(Constraint::Accum);
                        after.insert(Constraint::Read);
                    }
                    after
                })
                .collect(),
            types: bwd_types.into(),
            vars: [].into(),   // TODO
            params: [].into(), // TODO
            ret: id::var(0),   // TODO
            body: bwd_body.into(),
        },
    )
}
