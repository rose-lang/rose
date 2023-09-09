use enumset::EnumSet;
use rose::{id, Binop, Constraint, Expr, Func, Instr, Ty, Unop};
use std::mem::{swap, take};

const REAL: id::Ty = id::ty(0);
const DUAL: id::Ty = id::ty(1);

fn is_primitive(t: id::Ty) -> bool {
    t == REAL || t == DUAL
}

enum Scope {
    Generic(id::Generic),
    Derived(id::Var),
    Original,
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
    Accum(Scope, id::Ty),

    /// We don't know the type of this variable yet, but will soon.
    ///
    /// Usually this means the variable is a tuple of intermediate values, and we'll update its type
    /// to something concrete when we reach the end of the block.
    Unknown,
}

/// The source of a primitive variable.
#[derive(Clone, Copy)]
enum Src {
    /// This variable is original.
    Original,

    /// This variable is an alias of an original primitive variable of the same type.
    Alias(id::Var),

    /// This variable is a component of an original dual number variable.
    Projection(id::Var),
}

impl Src {
    fn prim(self, x: id::Var) -> Self {
        match self {
            Self::Original => Self::Alias(x),
            _ => self,
        }
    }

    fn dual(self, x: id::Var) -> Self {
        match self {
            Self::Original => Self::Projection(x),
            _ => self,
        }
    }
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
    inter_mem: Vec<id::Var>,

    /// Variable ID for the intermediate values tuple in this backward pass block.
    inter_tup: id::Var,

    /// Instructions at the beginning of this backward pass block, in order.
    bwd_nonlin: Vec<Instr>,

    /// Instructions at the end of this backward pass block, in reverse order.
    bwd_lin: Vec<Instr>,
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

    /// Sources of primitive variables from the original function.
    prims: Box<[Option<Src>]>,

    /// Sources for dual number variables from the original function.
    duals: Box<[Option<(Src, Src)>]>,

    /// Accumulator variables for variables from the original function.
    accums: Box<[Option<id::Var>]>,

    /// Cotangent variables for variables from the original function.
    cotans: Box<[Option<id::Var>]>,

    /// The current block under construction.
    block: Block,
}

impl<'a> Transpose<'a> {
    fn re(&self, x: id::Var) -> Src {
        let (src, _) = self.duals[x.var()].unwrap();
        src.dual(x)
    }

    fn du(&self, x: id::Var) -> Src {
        let (_, src) = self.duals[x.var()].unwrap();
        src.dual(x)
    }

    fn get_prim(&self, x: id::Var) -> id::Var {
        match self.prims[x.var()].unwrap() {
            Src::Original => x,
            Src::Alias(y) | Src::Projection(y) => y,
        }
    }

    fn get_re(&self, x: id::Var) -> id::Var {
        match self.duals[x.var()] {
            None | Some((Src::Original, _)) => x,
            Some((Src::Alias(y), _)) | Some((Src::Projection(y), _)) => y,
        }
    }

    fn get_du(&self, x: id::Var) -> id::Var {
        match self.duals[x.var()] {
            None | Some((_, Src::Original)) => x,
            Some((_, Src::Alias(y))) | Some((_, Src::Projection(y))) => y,
        }
    }

    fn get_prim_accum(&self, x: id::Var) -> id::Var {
        self.accums[self.get_prim(x).var()].unwrap()
    }

    fn get_dual_accum(&self, x: id::Var) -> Option<id::Var> {
        self.accums[self.get_du(x).var()]
    }

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
        self.block.bwd_nonlin.push(Instr {
            var,
            expr: Expr::Member {
                tuple: self.block.inter_tup,
                member: id::member(self.block.inter_mem.len()),
            },
        });
        self.block.inter_mem.push(var);
    }

    fn accum(&mut self, shape: id::Var, scope: Scope) -> Lin {
        let t = self.f.vars[shape.var()];
        let acc = self.bwd_var(BwdTy::Accum(scope, t));
        let cot = self.bwd_var(BwdTy::Known(t));
        self.block.bwd_nonlin.push(Instr {
            var: acc,
            expr: Expr::Accum { shape },
        });
        self.accums[shape.var()] = Some(acc);
        self.cotans[shape.var()] = Some(cot);
        Lin { acc, cot }
    }

    fn calc(&mut self, tan: id::Var) -> Lin {
        let t = self.f.vars[tan.var()];
        let acc = self.bwd_var(BwdTy::Accum(Scope::Original, t));
        let cot = self.bwd_var(BwdTy::Known(t));
        self.block.bwd_nonlin.push(Instr {
            var: acc,
            expr: Expr::Accum {
                shape: self.real_shape,
            },
        });
        self.accums[tan.var()] = Some(acc);
        self.cotans[tan.var()] = Some(cot);
        Lin { acc, cot }
    }

    fn resolve(&mut self, lin: Lin) {
        self.block.bwd_lin.push(Instr {
            var: lin.cot,
            expr: Expr::Resolve { var: lin.acc },
        })
    }

    fn block(&mut self, block: &[Instr]) -> (id::Ty, id::Var) {
        for instr in block.iter() {
            self.instr(instr.var, &instr.expr);
        }
        let vars = take(&mut self.block.inter_mem);
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
        self.bwd_vars[self.block.inter_tup.var()] = BwdTy::Known(t);
        (t, var)
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
            &Expr::F64 { val } => {
                match self.f.vars[var.var()] {
                    DUAL => {
                        let lin = self.calc(var);
                        self.resolve(lin);
                    }
                    _ => self.block.fwd.push(Instr {
                        var,
                        expr: Expr::F64 { val },
                    }),
                }
                self.prims[var.var()] = Some(Src::Original);
            }
            &Expr::Fin { val } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Fin { val },
                });
                self.block.bwd_nonlin.push(Instr {
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
                        elems: elems.iter().map(|&elem| self.get_re(elem)).collect(),
                    },
                });
                self.keep(var);
                let lin = self.accum(var, Scope::Original);
                for (i, &elem) in elems.iter().enumerate() {
                    if let Some(accum) = self.get_dual_accum(elem) {
                        let index = self.bwd_var(BwdTy::Known(t));
                        let addend = self.bwd_var(BwdTy::Known(self.f.vars[elem.var()]));
                        let unit = self.bwd_var(BwdTy::Unit);
                        self.block.bwd_lin.push(Instr {
                            var: unit,
                            expr: Expr::Add { accum, addend },
                        });
                        self.block.bwd_lin.push(Instr {
                            var: addend,
                            expr: Expr::Index {
                                array: lin.cot,
                                index,
                            },
                        });
                        self.block.bwd_lin.push(Instr {
                            var: index,
                            expr: Expr::Fin { val: i },
                        });
                    }
                }
                self.resolve(lin);
            }
            Expr::Tuple { members } => match self.types[self.f.vars[var.var()].ty()] {
                Ty::F64 => {
                    let x = members[1];
                    let dx = members[0];
                    self.duals[var.var()] = Some((
                        self.prims[x.var()].unwrap().prim(x),
                        self.prims[dx.var()].unwrap().prim(dx),
                    ));
                }
                _ => {
                    self.block.fwd.push(Instr {
                        var,
                        expr: Expr::Tuple {
                            members: members.iter().map(|&member| self.get_re(member)).collect(),
                        },
                    });
                    self.keep(var);
                    let lin = self.accum(var, Scope::Original);
                    for (i, &member) in members.iter().enumerate() {
                        if let Some(accum) = self.get_dual_accum(member) {
                            let addend = self.bwd_var(BwdTy::Known(self.f.vars[member.var()]));
                            let unit = self.bwd_var(BwdTy::Unit);
                            self.block.bwd_lin.push(Instr {
                                var: unit,
                                expr: Expr::Add { accum, addend },
                            });
                            self.block.bwd_lin.push(Instr {
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
            },

            &Expr::Index { array, index } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                self.block.bwd_nonlin.push(Instr {
                    var,
                    expr: Expr::Index { array, index },
                });
                let arr_acc = self.accums[array.var()].unwrap();
                let acc = self.bwd_var(BwdTy::Accum(
                    Scope::Derived(arr_acc),
                    self.f.vars[array.var()],
                ));
                self.accums[var.var()] = Some(acc);
                self.block.bwd_nonlin.push(Instr {
                    var: acc,
                    expr: Expr::Slice {
                        array: arr_acc,
                        index,
                    },
                });
                if let Ty::F64 = self.types[self.f.vars[var.var()].ty()] {
                    self.duals[var.var()] = Some((Src::Original, Src::Original));
                }
            }
            &Expr::Member { tuple, member } => {
                let t = self.f.vars[var.var()];
                match t {
                    REAL => self.prims[var.var()] = Some(self.re(tuple)),
                    DUAL => self.prims[var.var()] = Some(self.du(tuple)),
                    _ => {
                        self.block.fwd.push(Instr {
                            var,
                            expr: Expr::Member { tuple, member },
                        });
                        self.block.bwd_nonlin.push(Instr {
                            var,
                            expr: Expr::Member { tuple, member },
                        });
                        let tup_acc = self.accums[tuple.var()].unwrap();
                        let acc = self.bwd_var(BwdTy::Accum(
                            Scope::Derived(tup_acc),
                            self.f.vars[tuple.var()],
                        ));
                        self.accums[var.var()] = Some(acc);
                        self.block.bwd_nonlin.push(Instr {
                            var: acc,
                            expr: Expr::Field {
                                tuple: tup_acc,
                                member,
                            },
                        });
                        if let Ty::F64 = self.types[t.ty()] {
                            self.duals[var.var()] = Some((Src::Original, Src::Original));
                        }
                    }
                }
            }

            &Expr::Slice { array, index } => todo!(),
            &Expr::Field { tuple, member } => todo!(),

            &Expr::Unary { op, arg } => {
                match self.f.vars[var.var()] {
                    DUAL => match op {
                        Unop::Not | Unop::Abs | Unop::Sign | Unop::Sqrt => panic!(),
                        Unop::Neg => {
                            let lin = self.calc(var);
                            let res = self.bwd_var(BwdTy::Known(DUAL));
                            let unit = self.bwd_var(BwdTy::Unit);
                            self.block.bwd_lin.push(Instr {
                                var: unit,
                                expr: Expr::Add {
                                    accum: self.get_prim_accum(arg),
                                    addend: res,
                                },
                            });
                            self.block.bwd_lin.push(Instr {
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
                            expr: Expr::Unary {
                                op,
                                arg: self.get_prim(arg),
                            },
                        });
                        self.keep(var);
                    }
                }
                self.prims[var.var()] = Some(Src::Original);
            }
            &Expr::Binary { op, left, right } => {
                match self.f.vars[var.var()] {
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
                                self.block.bwd_lin.push(Instr {
                                    var: a,
                                    expr: Expr::Add {
                                        accum: self.get_prim_accum(left),
                                        addend: lin.cot,
                                    },
                                });
                                self.block.bwd_lin.push(Instr {
                                    var: b,
                                    expr: Expr::Add {
                                        accum: self.get_prim_accum(right),
                                        addend: lin.cot,
                                    },
                                });
                            }
                            Binop::Sub => {
                                let res = self.bwd_var(BwdTy::Known(DUAL));
                                let a = self.bwd_var(BwdTy::Unit);
                                let b = self.bwd_var(BwdTy::Unit);
                                self.block.bwd_lin.push(Instr {
                                    var: a,
                                    expr: Expr::Add {
                                        accum: self.get_prim_accum(left),
                                        addend: lin.cot,
                                    },
                                });
                                self.block.bwd_lin.push(Instr {
                                    var: b,
                                    expr: Expr::Add {
                                        accum: self.get_prim_accum(right),
                                        addend: res,
                                    },
                                });
                                self.block.bwd_lin.push(Instr {
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
                                self.block.bwd_lin.push(Instr {
                                    var: unit,
                                    expr: Expr::Add {
                                        accum: self.get_prim_accum(left),
                                        addend: res,
                                    },
                                });
                                self.block.bwd_lin.push(Instr {
                                    var: res,
                                    expr: Expr::Binary {
                                        op,
                                        left: lin.cot,
                                        right: self.get_prim(right),
                                    },
                                });
                            }
                        }
                        self.resolve(lin);
                    }
                    _ => {
                        self.block.fwd.push(Instr {
                            var,
                            expr: Expr::Binary {
                                op,
                                left: self.get_prim(left),
                                right: self.get_prim(right),
                            },
                        });
                        self.keep(var);
                    }
                }
                self.prims[var.var()] = Some(Src::Original);
            }
            &Expr::Select { cond, then, els } => todo!(),

            Expr::Call { id, generics, args } => todo!(),
            Expr::For { arg, body, ret } => {
                let mut block = Block {
                    fwd: vec![],
                    inter_mem: vec![],
                    inter_tup: self.bwd_var(BwdTy::Unknown),
                    bwd_nonlin: vec![],
                    bwd_lin: vec![],
                };
                swap(&mut self.block, &mut block);
                self.block(body); // TODO
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
    let types: Vec<_> = f
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
        .collect();

    let mut bwd_generics: Vec<_> = f
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
        .collect();

    let mut bwd_vars: Vec<_> = f.vars.iter().map(|&t| BwdTy::Known(t)).collect();
    let real_shape = id::var(bwd_vars.len());
    bwd_vars.push(BwdTy::Known(DUAL));
    let inter_tup = id::var(bwd_vars.len());
    bwd_vars.push(BwdTy::Unknown);

    let mut duals = vec![None; f.vars.len()].into_boxed_slice();
    let mut accums = vec![None; f.vars.len()].into_boxed_slice();

    let mut inter_mem = vec![];
    let mut bwd_nonlin = vec![];

    let mut bwd_params: Vec<_> = f
        .params
        .iter()
        .map(|&param| {
            let g = id::generic(bwd_generics.len());
            bwd_generics.push(EnumSet::only(Constraint::Accum));
            let t = f.vars[param.var()];
            bwd_nonlin.push(Instr {
                var: param,
                expr: Expr::Member {
                    tuple: inter_tup,
                    member: id::member(inter_mem.len()),
                },
            });
            inter_mem.push(param);
            let acc = id::var(bwd_vars.len());
            bwd_vars.push(BwdTy::Accum(Scope::Generic(g), t));
            if let Ty::F64 = types[t.ty()] {
                duals[param.var()] = Some((Src::Original, Src::Original));
            }
            accums[param.var()] = Some(acc);
            acc
        })
        .collect();

    let mut tp = Transpose {
        f,
        types,
        fwd_vars: f.vars.to_vec(),
        bwd_vars,
        real_shape,
        prims: vec![None; f.vars.len()].into(),
        duals,
        accums,
        cotans: vec![None; f.vars.len()].into(),
        block: Block {
            fwd: vec![],
            inter_tup,
            inter_mem,
            bwd_nonlin,
            bwd_lin: vec![],
        },
    };

    let (t_intermediates, fwd_inter) = tp.block(&f.body);
    let fwd_ret = tp.get_re(f.ret);
    let bwd_acc = tp.get_dual_accum(f.ret).unwrap();
    let mut bwd_types = tp.types.clone();

    let mut fwd_types = tp.types;
    let t_bundle = id::ty(fwd_types.len());
    fwd_types.push(Ty::Tuple {
        members: vec![f.vars[f.ret.var()], t_intermediates].into(),
    });
    let mut fwd_vars = tp.fwd_vars;
    let fwd_bundle = id::var(fwd_vars.len());
    fwd_vars.push(t_bundle);
    let mut fwd_body = tp.block.fwd;
    fwd_body.push(Instr {
        var: fwd_bundle,
        expr: Expr::Tuple {
            members: vec![fwd_ret, fwd_inter].into(),
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
    let bwd_cot = id::var(bwd_vars.len());
    bwd_vars.push(f.vars[f.ret.var()]);
    let bwd_unit = id::var(bwd_vars.len());
    bwd_vars.push(t_unit);
    bwd_params.push(bwd_cot);
    bwd_params.push(tp.block.inter_tup);
    let mut bwd_body = vec![Instr {
        var: tp.real_shape,
        expr: Expr::F64 { val: 0. },
    }];
    bwd_body.append(&mut tp.block.bwd_nonlin);
    let mut bwd_linear = tp.block.bwd_lin;
    bwd_linear.push(Instr {
        var: bwd_unit,
        expr: Expr::Add {
            accum: bwd_acc,
            addend: bwd_cot,
        },
    });
    bwd_linear.reverse();
    bwd_body.append(&mut bwd_linear);

    (
        Func {
            generics: f.generics.clone(),
            types: fwd_types.into(),
            vars: fwd_vars.into(),
            params: f.params.clone(),
            ret: fwd_bundle,
            body: fwd_body.into(),
        },
        Func {
            generics: bwd_generics.into(),
            types: bwd_types.into(),
            vars: bwd_vars.into(),
            params: bwd_params.into(),
            ret: bwd_unit,
            body: bwd_body.into(),
        },
    )
}
