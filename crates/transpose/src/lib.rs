use rose::{id, Binop, Constraint, Expr, Func, Instr, Ty, Unop};
use std::mem::{swap, take};

const REAL: id::Ty = id::ty(0);
const DUAL: id::Ty = id::ty(1);

fn is_primitive(t: id::Ty) -> bool {
    t == REAL || t == DUAL
}

/// Type ID of a variable while building up the backward pass.
enum BwdTy {
    /// We already know the type ID of this variable.
    ///
    /// Usually this means the variable is directly mapped from a variable in the original function.
    Known(id::Ty),

    /// This variable has the unit type.
    Unit,

    /// This variable is an accumulator.
    ///
    /// After we process the entire function, we need to add a scope type for every new accumulator
    /// variable we've introduced; we don't add these as we go because we don't know ahead of time
    /// how many types we'll need for intermediate values, and we want all those intermediate value
    /// type IDs to be the same between the forward and backward passes.
    Accum(Option<id::Var>, id::Ty),

    /// We don't know the type of this variable yet, but will soon.
    ///
    /// Usually this means the variable is a tuple of intermediate values, and we'll update its type
    /// to something concrete when we reach the end of the block.
    Unknown,
}

/// Linear variables in the backward pass for a variable from the original function.
struct Lin {
    /// Accumulator variable.
    acc: id::Var,

    /// Resolved cotangent variable.
    cot: id::Var,
}

/// A block under construction for both the forward pass and the backward pass.
struct Block {
    /// Instructions in this forward pass block, in order.
    fwd: Vec<Instr>,

    /// Variable IDs for intermediate values to be saved at the end of this forward pass block.
    intermediate_members: Vec<id::Var>,

    /// Variable ID for the intermediate values tuple in this backward pass block.
    intermediates_tuple: id::Var,

    /// Instructions at the beginning of this backward pass block, in order.
    bwd_nonlinear: Vec<Instr>,

    /// Instructions at the end of this backward pass block, in reverse order.
    bwd_linear: Vec<Instr>,
}

/// The forward pass and backward pass of a transposed function under construction.
struct Transpose<'a> {
    /// The function being transposed, which is usually a forward-mode derivative.
    f: &'a Func,

    /// Shared types between the forward and backward passes.
    ///
    /// This starts out the same length as `f.types`, with the only difference being that all dual
    /// number types are replaced with `F64`. Then we add more types only for tuples and arrays of
    /// intermediate values that are shared between the two passes.
    types: Vec<Ty>,

    /// Types of variables in the forward pass.
    ///
    /// This starts out as a clone of `f.vars`, but more variables can be added for dealing with
    /// intermediate values.
    fwd_vars: Vec<id::Ty>,

    /// Types of variables in the backward pass.
    ///
    /// This starts out with the `Known` type of every variable from `f.vars`, but more variables
    /// can be added for intermediate values, accumulators, and cotangents.
    bwd_vars: Vec<BwdTy>,

    /// A variable of type `F64` defined at the beginning of the backward pass.
    ///
    /// Every accumulator must be initialized using a concrete variable to dictate its topology. For
    /// most variables, we keep around the original nonlinear value and use that as the shape, but
    /// this doesn't work for raw linear `F64` variables, which might be part of some lower-level
    /// mathematical calculation that is not clearly attached to any value at the dual number level
    /// or higher. All those values have the same shape, though, so in those cases we just use this
    /// one dummy variable as the shape.
    real_shape: id::Var,

    /// Variables from the original function that are just the real part of a dual number variable.
    reals: Box<[Option<id::Var>]>,

    /// Variables from the original function that are just the dual part of a dual number variable.
    duals: Box<[Option<id::Var>]>,

    /// Accumulator variables for variables from the original function.
    accums: Box<[Option<id::Var>]>,

    /// Cotangent variables for variables from the original function.
    cotangents: Box<[Option<id::Var>]>,

    /// The current block under construction.
    block: Block,
}

impl<'a> Transpose<'a> {
    fn ty(&mut self, ty: Ty) -> id::Ty {
        let t = id::ty(self.types.len());
        self.types.push(ty);
        t
    }

    fn fwd_var(&mut self, t: id::Ty) -> id::Var {
        let var = id::var(self.fwd_vars.len());
        self.fwd_vars.push(t);
        var
    }

    fn bwd_var(&mut self, t: BwdTy) -> id::Var {
        let var = id::var(self.bwd_vars.len());
        self.bwd_vars.push(t);
        var
    }

    fn keep(&mut self, var: id::Var) {
        self.block.bwd_nonlinear.push(Instr {
            var,
            expr: Expr::Member {
                tuple: self.block.intermediates_tuple,
                member: id::member(self.block.intermediate_members.len()),
            },
        });
        self.block.intermediate_members.push(var);
    }

    fn accum(&mut self, shape: id::Var, scope: Option<id::Var>) -> Lin {
        let t = self.f.vars[shape.var()];
        let acc = self.bwd_var(BwdTy::Accum(scope, t));
        let cot = self.bwd_var(BwdTy::Known(t));
        self.block.bwd_nonlinear.push(Instr {
            var: acc,
            expr: Expr::Accum { shape },
        });
        self.accums[shape.var()] = Some(acc);
        self.cotangents[shape.var()] = Some(cot);
        Lin { acc, cot }
    }

    fn calc(&mut self, tan: id::Var) -> Lin {
        let t = self.f.vars[tan.var()];
        let acc = self.bwd_var(BwdTy::Accum(None, t));
        let cot = self.bwd_var(BwdTy::Known(t));
        self.block.bwd_nonlinear.push(Instr {
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
        self.block.bwd_linear.push(Instr {
            var: lin.cot,
            expr: Expr::Resolve { var: lin.acc },
        })
    }

    fn block(&mut self, block: &[Instr]) -> id::Ty {
        for instr in block.iter() {
            self.instr(instr.var, &instr.expr);
        }
        let vars = take(&mut self.block.intermediate_members);
        let t = self.ty(Ty::Tuple {
            members: vars.iter().map(|&x| self.fwd_vars[x.var()]).collect(),
        });
        let var = self.fwd_var(t);
        self.block.fwd.push(Instr {
            var,
            expr: Expr::Tuple {
                members: vars.into(),
            },
        });
        self.bwd_vars[self.block.intermediates_tuple.var()] = BwdTy::Known(t);
        t
    }

    fn instr(&mut self, var: id::Var, expr: &Expr) {
        match expr {
            Expr::Unit => self.block.fwd.push(Instr {
                var,
                expr: Expr::Unit,
            }),
            &Expr::Bool { val } => self.block.fwd.push(Instr {
                var,
                expr: Expr::Bool { val },
            }),
            &Expr::F64 { val } => match self.f.vars[var.var()] {
                DUAL => {
                    let lin = self.calc(var);
                    self.resolve(lin);
                }
                _ => self.block.fwd.push(Instr {
                    var,
                    expr: Expr::F64 { val },
                }),
            },
            &Expr::Fin { val } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Fin { val },
                });
                self.block.bwd_nonlinear.push(Instr {
                    var,
                    expr: Expr::Fin { val },
                });
            }

            Expr::Array { elems } => {
                let t = match self.f.types[self.f.vars[var.var()].ty()] {
                    Ty::Array { index, elem: _ } => index,
                    _ => panic!(),
                };
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Array {
                        elems: elems.clone(),
                    },
                });
                self.keep(var);
                let lin = self.accum(var, None);
                for (i, &elem) in elems.iter().enumerate() {
                    if let Some(accum) = self.accums[elem.var()] {
                        let index = self.bwd_var(BwdTy::Known(t));
                        let addend = self.bwd_var(BwdTy::Known(self.f.vars[elem.var()]));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.block.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add { accum, addend },
                        });
                        self.block.bwd_linear.push(Instr {
                            var: addend,
                            expr: Expr::Index {
                                array: lin.cot,
                                index,
                            },
                        });
                        self.block.bwd_linear.push(Instr {
                            var: index,
                            expr: Expr::Fin { val: i },
                        });
                    }
                }
                self.resolve(lin);
            }
            Expr::Tuple { members } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Tuple {
                        members: members.clone(),
                    },
                });
                self.keep(var);
                let lin = self.accum(var, None);
                for (i, &member) in members.iter().enumerate() {
                    if let Some(accum) = self.accums[member.var()] {
                        let addend = self.bwd_var(BwdTy::Known(self.f.vars[member.var()]));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.block.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add { accum, addend },
                        });
                        self.block.bwd_linear.push(Instr {
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
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                self.block.bwd_nonlinear.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                let arr_acc = self.accums[array.var()].unwrap();
                let acc = self.bwd_var(BwdTy::Accum(Some(arr_acc), self.f.vars[array.var()]));
                self.accums[var.var()] = Some(acc);
                self.block.bwd_nonlinear.push(Instr {
                    var: acc,
                    expr: Expr::Slice {
                        array: arr_acc,
                        index,
                    },
                });
            }
            &Expr::Member { tuple, member } => match self.f.vars[var.var()] {
                REAL => self.reals[var.var()] = Some(tuple),
                DUAL => {
                    let lin = self.accum(var, None); // TODO
                    self.duals[var.var()] = Some(tuple);
                    self.resolve(lin);
                }
                _ => {
                    self.block.fwd.push(Instr {
                        var,
                        expr: Expr::Member { tuple, member },
                    });
                    self.block.bwd_nonlinear.push(Instr {
                        var,
                        expr: Expr::Member { tuple, member },
                    });
                    let tup_acc = self.accums[tuple.var()].unwrap();
                    let acc = self.bwd_var(BwdTy::Accum(Some(tup_acc), self.f.vars[tuple.var()]));
                    self.accums[var.var()] = Some(acc);
                    self.block.bwd_nonlinear.push(Instr {
                        var: acc,
                        expr: Expr::Field {
                            tuple: tup_acc,
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
                        self.block.bwd_linear.push(Instr {
                            var: unit,
                            expr: Expr::Add {
                                accum: self.accums[arg.var()].unwrap(),
                                addend: res,
                            },
                        });
                        self.block.bwd_linear.push(Instr {
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
                    self.block.fwd.push(Instr {
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
                            self.block.bwd_linear.push(Instr {
                                var: a,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: lin.cot,
                                },
                            });
                            self.block.bwd_linear.push(Instr {
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
                            self.block.bwd_linear.push(Instr {
                                var: a,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: lin.cot,
                                },
                            });
                            self.block.bwd_linear.push(Instr {
                                var: b,
                                expr: Expr::Add {
                                    accum: self.accums[right.var()].unwrap(),
                                    addend: res,
                                },
                            });
                            self.block.bwd_linear.push(Instr {
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
                            self.block.bwd_linear.push(Instr {
                                var: unit,
                                expr: Expr::Add {
                                    accum: self.accums[left.var()].unwrap(),
                                    addend: res,
                                },
                            });
                            self.block.bwd_linear.push(Instr {
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
                    self.block.fwd.push(Instr {
                        var,
                        expr: Expr::Binary { op, left, right },
                    });
                    self.keep(var);
                }
            },
            &Expr::Select { cond, then, els } => todo!(),

            Expr::Call { id, generics, args } => todo!(),
            Expr::For { arg, body, ret } => {
                let mut block = Block {
                    fwd: vec![],
                    intermediate_members: vec![],
                    intermediates_tuple: self.bwd_var(BwdTy::Unknown),
                    bwd_nonlinear: vec![],
                    bwd_linear: vec![],
                };
                swap(&mut self.block, &mut block);
                self.block(body);
                swap(&mut self.block, &mut block);
            }

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
    let mut bwd_vars: Vec<_> = f.vars.iter().map(|&t| BwdTy::Known(t)).collect();
    let real_shape = id::var(bwd_vars.len());
    bwd_vars.push(BwdTy::Known(DUAL));
    let intermediates_tuple = id::var(bwd_vars.len());
    bwd_vars.push(BwdTy::Unknown);
    let mut tp = Transpose {
        f,
        types: f
            .types
            .iter()
            .enumerate()
            .map(|(i, ty)| match ty {
                Ty::Unit => Ty::Unit,
                Ty::Bool => Ty::Unit,
                Ty::F64 => {
                    if !is_primitive(id::ty(i)) {
                        panic!()
                    }
                    Ty::F64
                }
                &Ty::Fin { size } => Ty::Fin { size },
                &Ty::Generic { id } => Ty::Generic { id },
                &Ty::Scope { kind, id } => Ty::Scope { kind, id },
                &Ty::Ref { scope, inner } => {
                    if is_primitive(inner) {
                        panic!()
                    }
                    Ty::Ref { scope, inner }
                }
                &Ty::Array { index, elem } => {
                    if is_primitive(elem) {
                        panic!()
                    }
                    Ty::Array { index, elem }
                }
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
        bwd_vars,
        real_shape,
        reals: vec![None; f.vars.len()].into(),
        duals: vec![None; f.vars.len()].into(),
        accums: vec![None; f.vars.len()].into(),     // TODO
        cotangents: vec![None; f.vars.len()].into(), // TODO
        block: Block {
            fwd: vec![],
            intermediates_tuple,
            intermediate_members: vec![],
            bwd_nonlinear: vec![],
            bwd_linear: vec![],
        },
    };

    let t_intermediates = tp.block(&f.body);
    let mut bwd_types = tp.types.clone();

    let mut fwd_types = tp.types;
    let t_bundle = id::ty(fwd_types.len());
    fwd_types.push(Ty::Tuple {
        members: vec![f.vars[f.ret.var()], t_intermediates].into(),
    });
    let mut fwd_vars = tp.fwd_vars;
    let fwd_ret = id::var(fwd_vars.len());
    fwd_vars.push(t_bundle);
    let mut fwd_body = tp.block.fwd;
    fwd_body.push(Instr {
        var: fwd_ret,
        expr: Expr::Tuple {
            members: vec![f.ret, tp.block.intermediates_tuple].into(),
        },
    });

    let t_unit = id::ty(bwd_types.len());
    bwd_types.push(Ty::Unit);
    let mut bwd_vars: Vec<_> = tp
        .bwd_vars
        .into_iter()
        .enumerate()
        .map(|(i, t)| match t {
            BwdTy::Known(t) => t,
            BwdTy::Unit => t_unit,
            BwdTy::Accum(scope, inner) => {
                let scope = id::ty(bwd_types.len());
                bwd_types.push(Ty::Scope {
                    kind: Constraint::Accum,
                    id: id::var(i),
                });
                let t = id::ty(bwd_types.len());
                bwd_types.push(Ty::Ref { scope, inner });
                t
            }
            BwdTy::Unknown => panic!(),
        })
        .collect();
    let bwd_ret = id::var(bwd_vars.len());
    bwd_vars.push(t_unit);
    let mut bwd_body = vec![Instr {
        var: tp.real_shape,
        expr: Expr::F64 { val: 0. },
    }];
    bwd_body.append(&mut tp.block.bwd_nonlinear);
    let mut bwd_linear = tp.block.bwd_linear;
    bwd_linear.reverse();
    bwd_body.append(&mut bwd_linear);
    bwd_body.push(Instr {
        var: bwd_ret,
        expr: Expr::Unit,
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
            vars: bwd_vars.into(),
            params: f.params.clone(), // TODO
            ret: bwd_ret,
            body: bwd_body.into(),
        },
    )
}
