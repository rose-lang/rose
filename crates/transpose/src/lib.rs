use rose::{id, Binop, Constraint, Expr, Func, Instr, Ty, Unop};

const REAL: id::Ty = id::ty(0);
const DUAL: id::Ty = id::ty(1);

fn is_primitive(t: id::Ty) -> bool {
    t == REAL || t == DUAL
}

enum BwdTy {
    Known(id::Ty),
    Unit,
    Fin(usize),
    Accum,
}

struct Lin {
    acc: id::Var,
    cot: id::Var,
}

struct Transpose<'a> {
    f: &'a Func,
    types: Vec<Ty>,
    fwd_vars: Vec<id::Ty>,
    fwd_block: Vec<Instr>,
    intermediates_tuple: id::Var,
    intermediate_members: Vec<id::Var>,
    bwd_vars: Vec<BwdTy>,
    real_shape: id::Var,
    reals: Box<[Option<id::Var>]>,
    duals: Box<[Option<id::Var>]>,
    accums: Box<[Option<id::Var>]>,
    cotangents: Box<[Option<id::Var>]>,
    bwd_nonlinear: Vec<Instr>,
    bwd_linear: Vec<Instr>,
}

impl Transpose<'_> {
    fn ty(&mut self, ty: Ty) -> id::Ty {
        let t = id::ty(self.types.len());
        self.types.push(ty);
        t
    }

    fn keep(&mut self, var: id::Var) {
        self.bwd_nonlinear.push(Instr {
            var,
            expr: Expr::Member {
                tuple: self.intermediates_tuple,
                member: id::member(self.intermediate_members.len()),
            },
        });
        self.intermediate_members.push(var);
    }

    fn bwd_var(&mut self, t: BwdTy) -> id::Var {
        let var = id::var(self.bwd_vars.len());
        self.bwd_vars.push(t);
        var
    }

    fn accum(&mut self, shape: id::Var) -> Lin {
        let acc = self.bwd_var(BwdTy::Accum);
        let cot = self.bwd_var(BwdTy::Known(self.f.vars[shape.var()]));
        self.bwd_nonlinear.push(Instr {
            var: acc,
            expr: Expr::Accum { shape },
        });
        self.accums[shape.var()] = Some(acc);
        self.cotangents[shape.var()] = Some(cot);
        Lin { acc, cot }
    }

    fn calc(&mut self, tan: id::Var) -> Lin {
        let acc = self.bwd_var(BwdTy::Accum);
        let cot = self.bwd_var(BwdTy::Known(DUAL));
        self.bwd_nonlinear.push(Instr {
            var: acc,
            expr: Expr::Accum {
                shape: self.real_shape,
            },
        });
        self.accums[tan.var()] = Some(acc);
        self.cotangents[tan.var()] = Some(cot);
        Lin { acc, cot }
    }

    fn resolve(&mut self, lin: Lin) {
        self.bwd_linear.push(Instr {
            var: lin.cot,
            expr: Expr::Resolve { var: lin.acc },
        })
    }

    fn block(&mut self, block: &[Instr]) -> id::Ty {
        for instr in block.iter() {
            self.instr(instr.var, &instr.expr);
        }
        let vars = std::mem::take(&mut self.intermediate_members);
        let types = vars.iter().map(|&x| self.fwd_vars[x.var()]).collect();
        self.fwd_block.push(Instr {
            var: self.intermediates_tuple,
            expr: Expr::Tuple {
                members: vars.into(),
            },
        });
        self.ty(Ty::Tuple { members: types })
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
            &Expr::F64 { val } => todo!(),
            &Expr::Fin { val } => {
                self.fwd_block.push(Instr {
                    var,
                    expr: Expr::Fin { val },
                });
                self.bwd_nonlinear.push(Instr {
                    var,
                    expr: Expr::Fin { val },
                });
            }

            Expr::Array { elems } => {
                self.fwd_block.push(Instr {
                    var,
                    expr: Expr::Array {
                        elems: elems.clone(),
                    },
                });
                self.keep(var);
                let lin = self.accum(var);
                for (i, &elem) in elems.iter().enumerate() {
                    if let Some(accum) = self.accums[elem.var()] {
                        let index = self.bwd_var(BwdTy::Fin(elems.len()));
                        let addend = self.bwd_var(BwdTy::Known(self.f.vars[elem.var()]));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add { accum, addend },
                        });
                        self.bwd_linear.push(Instr {
                            var: addend,
                            expr: Expr::Index {
                                array: lin.cot,
                                index,
                            },
                        });
                        self.bwd_linear.push(Instr {
                            var: index,
                            expr: Expr::Fin { val: i },
                        });
                    }
                }
                self.resolve(lin);
            }
            Expr::Tuple { members } => {
                self.fwd_block.push(Instr {
                    var,
                    expr: Expr::Tuple {
                        members: members.clone(),
                    },
                });
                self.keep(var);
                let lin = self.accum(var);
                for (i, &member) in members.iter().enumerate() {
                    if let Some(accum) = self.accums[member.var()] {
                        let addend = self.bwd_var(BwdTy::Known(self.f.vars[member.var()]));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add { accum, addend },
                        });
                        self.bwd_linear.push(Instr {
                            var: addend,
                            expr: Expr::Member {
                                tuple: lin.cot,
                                member: id::member(i),
                            },
                        });
                    }
                }
                self.resolve(lin);
            }

            &Expr::Index { array, index } => {
                self.fwd_block.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                self.bwd_nonlinear.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                let acc = self.bwd_var(BwdTy::Accum);
                self.accums[var.var()] = Some(acc);
                self.bwd_nonlinear.push(Instr {
                    var: acc,
                    expr: Expr::Slice {
                        array: self.accums[array.var()].unwrap(),
                        index,
                    },
                });
            }
            &Expr::Member { tuple, member } => match self.f.vars[var.var()] {
                REAL => self.reals[var.var()] = Some(tuple),
                DUAL => self.duals[var.var()] = Some(tuple),
                _ => {
                    self.fwd_block.push(Instr {
                        var,
                        expr: Expr::Member { tuple, member },
                    });
                    self.bwd_nonlinear.push(Instr {
                        var,
                        expr: Expr::Member { tuple, member },
                    });
                    let acc = self.bwd_var(BwdTy::Accum);
                    self.accums[var.var()] = Some(acc);
                    self.bwd_nonlinear.push(Instr {
                        var: acc,
                        expr: Expr::Field {
                            tuple: self.accums[tuple.var()].unwrap(),
                            member,
                        },
                    });
                }
            },

            &Expr::Slice { array, index } => todo!(),
            &Expr::Field { tuple, member } => todo!(),

            &Expr::Unary { op, arg } => match self.f.vars[var.var()] {
                DUAL => match op {
                    Unop::Not | Unop::Abs | Unop::Sign | Unop::Sqrt => panic!(),
                    Unop::Neg => {
                        let lin = self.calc(var);
                        let res = self.bwd_var(BwdTy::Known(DUAL));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add {
                                accum: self.accums[arg.var()].unwrap(),
                                addend: res,
                            },
                        });
                        self.bwd_linear.push(Instr {
                            var: res,
                            expr: Expr::Unary {
                                op: Unop::Neg,
                                arg: lin.cot,
                            },
                        });
                        self.resolve(lin);
                    }
                },
                _ => {
                    self.fwd_block.push(Instr {
                        var,
                        expr: Expr::Unary { op, arg },
                    });
                    self.keep(var);
                }
            },
            &Expr::Binary { op, left, right } => match self.f.vars[var.var()] {
                DUAL => {
                    let lin = self.calc(var);
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
                        | Binop::Geq => panic!(),
                        Binop::Add => {
                            let a = self.bwd_var(BwdTy::Unit);
                            let b = self.bwd_var(BwdTy::Unit);
                            self.bwd_linear.push(Instr {
                                var: a,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: lin.cot,
                                },
                            });
                            self.bwd_linear.push(Instr {
                                var: b,
                                expr: Expr::Add {
                                    accum: self.accums[right.var()].unwrap(),
                                    addend: lin.cot,
                                },
                            });
                        }
                        Binop::Sub => {
                            let res = self.bwd_var(BwdTy::Known(DUAL));
                            let a = self.bwd_var(BwdTy::Unit);
                            let b = self.bwd_var(BwdTy::Unit);
                            self.bwd_linear.push(Instr {
                                var: a,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: lin.cot,
                                },
                            });
                            self.bwd_linear.push(Instr {
                                var: b,
                                expr: Expr::Add {
                                    accum: self.accums[right.var()].unwrap(),
                                    addend: res,
                                },
                            });
                            self.bwd_linear.push(Instr {
                                var: res,
                                expr: Expr::Unary {
                                    op: Unop::Neg,
                                    arg: lin.cot,
                                },
                            });
                        }
                        Binop::Mul | Binop::Div => {
                            let res = self.bwd_var(BwdTy::Known(DUAL));
                            let unit = self.bwd_var(BwdTy::Unit);
                            self.bwd_linear.push(Instr {
                                var: unit,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: res,
                                },
                            });
                            self.bwd_linear.push(Instr {
                                var: res,
                                expr: Expr::Binary {
                                    op,
                                    left: lin.cot,
                                    right,
                                },
                            });
                        }
                    }
                    self.resolve(lin);
                }
                _ => {
                    self.fwd_block.push(Instr {
                        var,
                        expr: Expr::Binary { op, left, right },
                    });
                    self.keep(var);
                }
            },
            &Expr::Select { cond, then, els } => todo!(),

            Expr::Call { id, generics, args } => todo!(),
            Expr::For { arg, body, ret } => todo!(),

            &Expr::Read { var } => todo!(),
            &Expr::Accum { shape } => todo!(),

            &Expr::Ask { var } => todo!(),
            &Expr::Add { accum, addend } => todo!(),

            &Expr::Resolve { var } => todo!(),
        }
    }
}

/// Return two functions that make up the transpose of `f`.
pub fn transpose(f: &Func) -> (Func, Func) {
    let mut tp = Transpose {
        f,
        types: f
            .types
            .iter()
            .map(|ty| match ty {
                Ty::Unit => Ty::Unit,
                Ty::Bool => Ty::Unit,
                Ty::F64 => Ty::F64,
                &Ty::Fin { size } => Ty::Fin { size },
                &Ty::Generic { id } => Ty::Generic { id },
                &Ty::Scope { kind, id } => Ty::Scope { kind, id },
                &Ty::Ref { scope, inner } => {
                    if is_primitive(inner) {
                        panic!()
                    }
                    Ty::Ref { scope, inner }
                }
                &Ty::Array { index, elem } => Ty::Array { index, elem },
                Ty::Tuple { members } => {
                    if members.iter().any(|&t| is_primitive(t)) {
                        Ty::F64
                    } else {
                        Ty::Tuple {
                            members: members.clone(),
                        }
                    }
                }
            })
            .collect(),
        fwd_vars: f.vars.to_vec(),
        fwd_block: vec![],
        intermediates_tuple: id::var(0), // TODO
        intermediate_members: vec![],
        bwd_vars: f.vars.iter().map(|&t| BwdTy::Known(t)).collect(),
        real_shape: id::var(0), // TODO
        reals: vec![None; f.vars.len()].into(),
        duals: vec![None; f.vars.len()].into(),
        accums: vec![None; f.vars.len()].into(),     // TODO
        cotangents: vec![None; f.vars.len()].into(), // TODO
        bwd_nonlinear: vec![],
        bwd_linear: vec![],
    };
    let t_intermediates = tp.block(&f.body);
    let bwd_types = tp.types.clone();

    let mut fwd_types = tp.types;
    let t_bundle = id::ty(fwd_types.len());
    fwd_types.push(Ty::Tuple {
        members: vec![f.vars[f.ret.var()], t_intermediates].into(),
    });
    let mut fwd_vars = tp.fwd_vars;
    let fwd_ret = id::var(fwd_vars.len());
    fwd_vars.push(t_bundle);
    let mut fwd_body = tp.fwd_block;
    fwd_body.push(Instr {
        var: fwd_ret,
        expr: Expr::Tuple {
            members: vec![f.ret, tp.intermediates_tuple].into(),
        },
    });

    (
        Func {
            generics: f.generics.clone(),
            types: fwd_types.into(),
            vars: fwd_vars.into(),
            params: f.params.clone(),
            ret: fwd_ret,
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
                .collect(), // TODO
            types: bwd_types.into(),
            vars: [].into(),   // TODO
            params: [].into(), // TODO
            ret: id::var(0),   // TODO
            body: [].into(),   // TODO
        },
    )
}
