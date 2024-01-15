use indexmap::{indexset, IndexSet};
use rose::{id, Binop, Expr, Func, Instr, Ty, Unop};
use std::mem::{replace, swap, take};

/// By convention, the first type in a function to be transposed must be the nonlinear `F64`.
const REAL: id::Ty = id::ty(0);

/// By convention, the first type in a function to be transposed must be the linear `F64`.
const DUAL: id::Ty = id::ty(1);

/// Return true iff `t` is the type ID of a linear type in a function to be transposed.
///
/// In this module, "primitive" specifically means a linear or nonlinear `F64` type, and
/// specifically excludes other types that might be considered primitive, such as `Unit` or `Bool`.
fn is_primitive(t: id::Ty) -> bool {
    t == REAL || t == DUAL
}

/// By convention, the first member in a type for the dual numbers must be the linear part.
const DU: id::Member = id::member(0);

/// By convention, the second member in a type for the dual numbers must be the nonlinear part.
const RE: id::Member = id::member(1);

/// The source of a primitive variable or a component of a dual number variable.
///
/// The value `None` means that this is the original source, whereas `Some` means that it is an
/// alias of a the given primitive variable or a component of the given dual number variable.
#[derive(Clone, Copy)]
struct Src(Option<id::Var>);

impl Src {
    /// Return the source for a variable derived from `self`.
    ///
    /// The variable ID `x` represents `self`, not the new source being returned.
    fn derive(self, x: id::Var) -> Self {
        match self.0 {
            None => Self(Some(x)),
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

    deps: &'a [(&'a [Ty], id::Ty)],

    /// Mapped versions of `f.types`.
    ///
    /// The only reason this is useful is to easily check whether a type is the dual number type by
    /// looking it up here to see if it's equal to `F64`.
    mapped_types: Vec<Ty>,

    /// Additional types, shared between the forward and backward passes.
    ///
    /// This starts out empty: at first we only have `mapped_types`, but later we'll add more types
    /// for tuples and arrays of intermediate values that are shared between the two passes, and
    /// also for new reference types that are only used in the backward pass. These `types` will
    /// later all be appended onto `mapped_types`, so any type indices referencing them should be
    /// offset by `mapped_types.len()`.
    types: IndexSet<Ty>,

    /// Type ID for `Unit`.
    ///
    /// We could get this every time by looking up in `types`, but it's easier to just always put it
    /// in at the beginning to save ourselves the repeated hash lookups.
    unit: id::Ty,

    /// Types of variables in the forward pass.
    ///
    /// This starts out as a clone of `f.vars`, but more variables can be added for dealing with
    /// intermediate values.
    fwd_vars: Vec<id::Ty>,

    /// Types of variables in the backward pass.
    ///
    /// This starts out with `Some` type for every variable from `f.vars`, but more variables can be
    /// added for intermediate values, accumulators, and cotangents. The only time a variable's type
    /// here is `None` is for tuples of intermediate values; see the `inter_tup` field in `Block`.
    bwd_vars: Vec<Option<id::Ty>>,

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
    ///
    /// The sources are for the nonlinear part and the linear part, respectively; note that this
    /// disagrees with the order for tuple members dictated by `DU` and `RE`.
    duals: Box<[Option<(Src, Src)>]>,

    /// Accumulator variables for variables from the original function.
    accums: Box<[Option<id::Var>]>,

    /// Cotangent variables for variables from the original function.
    cotans: Box<[Option<id::Var>]>,

    /// Stack of pending unreversed instructions for the backward pass.
    ///
    /// In general, we keep track of reversed instructions in the `bwd_lin` field of `block`; those
    /// will go after the unreversed `bwd_nonlin` instructions. When we enter a new scope via
    /// `Expr::For`, we start an entirely new `block`, so even though those inner `bwd_nonlin`
    /// instructions may end up interleaved with our current `bwd_lin` instructions, that's fine
    /// because they're going in a separate instruction list anyway.
    ///
    /// But for `Expr::Accum` and `Expr::Resolve`, we're introducing a new scope without actually
    /// starting a new `block`. In that case, we still need for all the instructions we put in
    /// `bwd_nonlin` during this scope to go before all our `bwd_lin` instructions from the scope,
    /// but we also need them to go after any `bwd_lin` instructions we add after the scope ends.
    /// So, what we do is push `bwd_nonlin` onto this `stack` when we enter the scope via
    /// `Expr::Accum`, and then when we exit the scope via `Expr::Resolve`, we pop it off, reverse
    /// it, and append it to `bwd_lin`. Then when we finally finish the actual block, the stack
    /// should be empty, so we just reverse `bwd_lin` and append it to `bwd_nonlin` as normal.
    stack: Vec<Vec<Instr>>,

    /// The current block under construction.
    block: Block,
}

impl<'a> Transpose<'a> {
    /// Return the ID for `ty`, adding it to `types` if it isn't already there.
    fn ty(&mut self, ty: Ty) -> id::Ty {
        let (i, _) = self.types.insert_full(ty);
        id::ty(self.f.types.len() + i)
    }

    fn translate(&mut self, generics: &[id::Ty], types: &[id::Ty], ty: &rose::Ty) -> id::Ty {
        self.ty(match ty {
            Ty::Unit => Ty::Unit,
            Ty::Bool => Ty::Bool,
            Ty::F64 => Ty::F64,
            &Ty::Fin { size } => Ty::Fin { size },
            Ty::Generic { id } => return generics[id.generic()],
            Ty::Ref { inner } => Ty::Ref {
                inner: types[inner.ty()],
            },
            Ty::Array { index, elem } => Ty::Array {
                index: types[index.ty()],
                elem: types[elem.ty()],
            },
            Ty::Tuple { members } => Ty::Tuple {
                members: members.iter().map(|&member| types[member.ty()]).collect(),
            },
        })
    }

    /// Return the source of a variable that is the nonlinear part of `x`.
    fn re(&self, x: id::Var) -> Src {
        let (src, _) = self.duals[x.var()].unwrap();
        src.derive(x)
    }

    /// Return the source of a variable that is the linear part of `x`.
    fn du(&self, x: id::Var) -> Src {
        let (_, src) = self.duals[x.var()].unwrap();
        src.derive(x)
    }

    /// Return the source variable for `x`, which has a primitive type.
    fn get_prim(&self, x: id::Var) -> id::Var {
        match self.prims[x.var()].unwrap() {
            Src(None) => x,
            Src(Some(y)) => y,
        }
    }

    /// Return the source variable for the nonlinear part of `x`, which has a non-primitive type.
    ///
    /// Every non-primitive variable whose type is not the dual numbers is considered original.
    fn get_re(&self, x: id::Var) -> id::Var {
        match self.duals[x.var()] {
            None | Some((Src(None), _)) => x,
            Some((Src(Some(y)), _)) => y,
        }
    }

    /// Return the source variable for the linear part of `x`, which has a non-primitive type.
    ///
    /// Every non-primitive variable whose type is not the dual numbers is considered original.
    fn get_du(&self, x: id::Var) -> id::Var {
        match self.duals[x.var()] {
            None | Some((_, Src(None))) => x,
            Some((_, Src(Some(y)))) => y,
        }
    }

    /// Return the accumulator variable for `x`, which has a primitive type.
    fn get_prim_accum(&self, x: id::Var) -> id::Var {
        self.accums[self.get_prim(x).var()].unwrap()
    }

    /// Return the accumulator variable for the linear part of `x`, which has a non-primitive type.
    fn get_accum(&self, x: id::Var) -> id::Var {
        self.accums[self.get_du(x).var()].unwrap()
    }

    /// Return the cotangent variable for the linear part of `x`, which has a non-primitive type.
    fn get_cotan(&self, x: id::Var) -> id::Var {
        self.cotans[self.get_du(x).var()].unwrap()
    }

    /// Return the ID for a new variable with type ID `t` in the forward pass.
    fn fwd_var(&mut self, t: id::Ty) -> id::Var {
        let var = id::var(self.fwd_vars.len());
        self.fwd_vars.push(t);
        var
    }

    /// Return the ID for a new variable with type ID `t` in the backward pass.
    ///
    /// `t` should be `None` iff it is a tuple of intermediate values.
    fn bwd_var(&mut self, t: Option<id::Ty>) -> id::Var {
        let var = id::var(self.bwd_vars.len());
        self.bwd_vars.push(t);
        var
    }

    /// Include `var` in the intermediate values tuple for the current block.
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

    /// Create a non-primitive accumulator for `shape`; return it along with its eventual cotangent.
    fn accum(&mut self, shape: id::Var) -> Lin {
        let t_cot = self.f.vars[shape.var()];
        let t_acc = self.ty(Ty::Ref { inner: t_cot });
        let acc = self.bwd_var(Some(t_acc));
        let cot = self.bwd_var(Some(t_cot));
        self.block.bwd_nonlin.push(Instr {
            var: acc,
            expr: Expr::Accum { shape },
        });
        self.accums[shape.var()] = Some(acc);
        self.cotans[shape.var()] = Some(cot);
        Lin { acc, cot }
    }

    /// Create a primitive accumulator for the given `tangent`, using `self.real_shape`.
    fn calc(&mut self, tangent: id::Var) -> Lin {
        let t_cot = self.f.vars[tangent.var()];
        let t_acc = self.ty(Ty::Ref { inner: t_cot });
        let acc = self.bwd_var(Some(t_acc));
        let cot = self.bwd_var(Some(t_cot));
        self.block.bwd_nonlin.push(Instr {
            var: acc,
            expr: Expr::Accum {
                shape: self.real_shape,
            },
        });
        self.accums[tangent.var()] = Some(acc);
        self.cotans[tangent.var()] = Some(cot);
        Lin { acc, cot }
    }

    /// Resolve the given accumulator.
    fn resolve(&mut self, lin: Lin) {
        self.block.bwd_lin.push(Instr {
            var: lin.cot,
            expr: Expr::Resolve { var: lin.acc },
        })
    }

    /// Process `block` and return the type and forward variable for the intermediate values tuple.
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
        self.bwd_vars[self.block.inter_tup.var()] = Some(t);
        (t, var)
    }

    /// Process the instruction with the given `var` and `expr`.
    fn instr(&mut self, var: id::Var, expr: &Expr) {
        match expr {
            Expr::Unit => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Unit,
                });
                self.block.bwd_nonlin.push(Instr {
                    var,
                    expr: Expr::Unit,
                });
                let lin = self.accum(var);
                self.resolve(lin);
            }
            &Expr::Bool { val } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Bool { val },
                });
                self.block.bwd_nonlin.push(Instr {
                    var,
                    expr: Expr::Bool { val },
                });
                let lin = self.accum(var);
                self.resolve(lin);
            }
            &Expr::F64 { val } => {
                match self.f.vars[var.var()] {
                    DUAL => {
                        let lin = self.calc(var);
                        self.resolve(lin);
                    }
                    _ => {
                        self.block.fwd.push(Instr {
                            var,
                            expr: Expr::F64 { val },
                        });
                        self.block.bwd_nonlin.push(Instr {
                            var,
                            expr: Expr::F64 { val },
                        });
                        let lin = self.accum(var);
                        self.resolve(lin);
                    }
                }
                self.prims[var.var()] = Some(Src(None));
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
                let lin = self.accum(var);
                self.resolve(lin);
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
                let lin = self.accum(var);
                for (i, &elem) in elems.iter().enumerate() {
                    let accum = self.get_accum(elem);
                    let index = self.bwd_var(Some(t));
                    let addend = self.bwd_var(Some(self.f.vars[elem.var()]));
                    let unit = self.bwd_var(Some(self.unit));
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
                self.resolve(lin);
            }
            Expr::Tuple { members } => match self.mapped_types[self.f.vars[var.var()].ty()] {
                Ty::F64 => {
                    let x = members[RE.member()];
                    let dx = members[DU.member()];
                    self.duals[var.var()] = Some((
                        self.prims[x.var()].unwrap().derive(x),
                        self.prims[dx.var()].unwrap().derive(dx),
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
                    let lin = self.accum(var);
                    for (i, &member) in members.iter().enumerate() {
                        let accum = self.get_accum(member);
                        let addend = self.bwd_var(Some(self.f.vars[member.var()]));
                        let unit = self.bwd_var(Some(self.unit));
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
                let arr_acc = self.get_accum(array);
                let t_acc = self.ty(Ty::Ref {
                    inner: self.f.vars[var.var()],
                });
                let acc = self.bwd_var(Some(t_acc));
                self.accums[var.var()] = Some(acc);
                self.block.bwd_nonlin.push(Instr {
                    var: acc,
                    expr: Expr::Slice {
                        array: arr_acc,
                        index,
                    },
                });
                if let Ty::F64 = self.mapped_types[self.f.vars[var.var()].ty()] {
                    self.duals[var.var()] = Some((Src(None), Src(None)));
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
                        let tup_acc = self.get_accum(tuple);
                        let t_acc = self.ty(Ty::Ref {
                            inner: self.f.vars[var.var()],
                        });
                        let acc = self.bwd_var(Some(t_acc));
                        self.accums[var.var()] = Some(acc);
                        self.block.bwd_nonlin.push(Instr {
                            var: acc,
                            expr: Expr::Field {
                                tuple: tup_acc,
                                member,
                            },
                        });
                        if let Ty::F64 = self.mapped_types[t.ty()] {
                            self.duals[var.var()] = Some((Src(None), Src(None)));
                        }
                    }
                }
            }

            &Expr::Slice { array, index } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Slice { array, index },
                });

                let t_cot = match &self.f.types[self.f.vars[var.var()].ty()] {
                    &Ty::Ref { inner } => inner,
                    _ => panic!(),
                };
                let cot = self.bwd_var(Some(t_cot));
                self.block.bwd_nonlin.push(Instr {
                    var: cot,
                    expr: Expr::Index {
                        array: self.get_cotan(array),
                        index,
                    },
                });
                self.cotans[var.var()] = Some(cot);
                if let Ty::F64 = self.mapped_types[self.f.vars[var.var()].ty()] {
                    self.duals[var.var()] = Some((Src(None), Src(None)));
                }
            }
            &Expr::Field { tuple, member } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Field { tuple, member },
                });

                let t_cot = match &self.f.types[self.f.vars[var.var()].ty()] {
                    &Ty::Ref { inner } => inner,
                    _ => panic!(),
                };
                let cot = self.bwd_var(Some(t_cot));
                self.block.bwd_nonlin.push(Instr {
                    var: cot,
                    expr: Expr::Member {
                        tuple: self.get_cotan(tuple),
                        member,
                    },
                });
                self.cotans[var.var()] = Some(cot);
                if let Ty::F64 = self.mapped_types[self.f.vars[var.var()].ty()] {
                    self.duals[var.var()] = Some((Src(None), Src(None)));
                }
            }

            &Expr::Unary { op, arg } => {
                match self.f.vars[var.var()] {
                    DUAL => match op {
                        Unop::Not
                        | Unop::Abs
                        | Unop::Sign
                        | Unop::Ceil
                        | Unop::Floor
                        | Unop::Trunc
                        | Unop::Sqrt => panic!(),
                        Unop::Neg => {
                            let lin = self.calc(var);
                            let res = self.bwd_var(Some(DUAL));
                            let unit = self.bwd_var(Some(self.unit));
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
                        let x = match op {
                            Unop::Not => arg,
                            Unop::Neg
                            | Unop::Abs
                            | Unop::Sign
                            | Unop::Ceil
                            | Unop::Floor
                            | Unop::Trunc
                            | Unop::Sqrt => self.get_prim(arg),
                        };
                        self.block.fwd.push(Instr {
                            var,
                            expr: Expr::Unary { op, arg: x },
                        });
                        self.keep(var);
                        let lin = self.accum(var);
                        self.resolve(lin);
                    }
                }
                self.prims[var.var()] = Some(Src(None));
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
                            | Binop::INeq
                            | Binop::ILt
                            | Binop::ILeq
                            | Binop::IEq
                            | Binop::IGt
                            | Binop::IGeq
                            | Binop::Neq
                            | Binop::Lt
                            | Binop::Leq
                            | Binop::Eq
                            | Binop::Gt
                            | Binop::Geq => panic!(),
                            Binop::Add => {
                                let a = self.bwd_var(Some(self.unit));
                                let b = self.bwd_var(Some(self.unit));
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
                                let res = self.bwd_var(Some(DUAL));
                                let a = self.bwd_var(Some(self.unit));
                                let b = self.bwd_var(Some(self.unit));
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
                                let res = self.bwd_var(Some(DUAL));
                                let unit = self.bwd_var(Some(self.unit));
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
                        let (a, b) = match op {
                            Binop::And
                            | Binop::Or
                            | Binop::Iff
                            | Binop::Xor
                            | Binop::INeq
                            | Binop::ILt
                            | Binop::ILeq
                            | Binop::IEq
                            | Binop::IGt
                            | Binop::IGeq => (left, right),
                            Binop::Neq
                            | Binop::Lt
                            | Binop::Leq
                            | Binop::Eq
                            | Binop::Gt
                            | Binop::Geq
                            | Binop::Add
                            | Binop::Sub
                            | Binop::Mul
                            | Binop::Div => (self.get_prim(left), self.get_prim(right)),
                        };
                        self.block.fwd.push(Instr {
                            var,
                            expr: Expr::Binary {
                                op,
                                left: a,
                                right: b,
                            },
                        });
                        self.keep(var);
                        let lin = self.accum(var);
                        self.resolve(lin);
                    }
                }
                self.prims[var.var()] = Some(Src(None));
            }
            &Expr::Select { cond, then, els } => {
                let t = self.f.vars[var.var()];

                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Select {
                        cond,
                        then: self.get_re(then),
                        els: self.get_re(els),
                    },
                });

                match &self.f.types[t.ty()] {
                    &Ty::Ref { inner } => {
                        let cot = self.bwd_var(Some(inner));
                        self.block.bwd_nonlin.push(Instr {
                            var: cot,
                            expr: Expr::Select {
                                cond,
                                then: self.get_cotan(then),
                                els: self.get_cotan(els),
                            },
                        });
                        self.cotans[var.var()] = Some(cot);
                    }
                    _ => {
                        self.keep(var);
                        if t == REAL {
                            self.prims[var.var()] = Some(Src(None));
                        } else {
                            let lin = self.accum(var);
                            let acc_then = self.get_accum(then);
                            let acc_els = self.get_accum(els);
                            let t_acc = self.ty(Ty::Ref {
                                inner: self.f.vars[var.var()],
                            });
                            let acc = self.bwd_var(Some(t_acc));
                            let unit = self.bwd_var(Some(self.unit));
                            self.block.bwd_lin.push(Instr {
                                var: unit,
                                expr: Expr::Add {
                                    accum: acc,
                                    addend: lin.cot,
                                },
                            });
                            self.block.bwd_lin.push(Instr {
                                var: acc,
                                expr: Expr::Select {
                                    cond,
                                    then: acc_then,
                                    els: acc_els,
                                },
                            });
                            self.resolve(lin);
                        }
                    }
                }

                if let Ty::F64 = self.mapped_types[t.ty()] {
                    self.duals[var.var()] = Some((Src(None), Src(None)));
                }
            }

            Expr::Call { id, generics, args } => match self.f.vars[var.var()] {
                REAL => {
                    self.block.fwd.push(Instr {
                        var,
                        expr: Expr::Call {
                            id: *id,
                            generics: generics.clone(),
                            args: args.iter().map(|&arg| self.get_prim(arg)).collect(),
                        },
                    });
                    self.keep(var);
                    self.prims[var.var()] = Some(Src(None));
                }
                _ => {
                    let (dep_types, t) = self.deps[id.func()];
                    let mut types = vec![];
                    for ty in dep_types {
                        types.push(self.translate(generics, &types, ty));
                    }
                    let t_tup = types[t.ty()];

                    let t_bundle = self.ty(Ty::Tuple {
                        members: [self.f.vars[var.var()], t_tup].into(),
                    });
                    let bundle = self.fwd_var(t_bundle);
                    self.block.fwd.push(Instr {
                        var: bundle,
                        expr: Expr::Call {
                            id: *id,
                            generics: generics.clone(),
                            args: args.iter().map(|&arg| self.get_re(arg)).collect(),
                        },
                    });

                    self.block.fwd.push(Instr {
                        var,
                        expr: Expr::Member {
                            tuple: bundle,
                            member: id::member(0),
                        },
                    });
                    self.keep(var);

                    let inter_fwd = self.fwd_var(t_tup);
                    let inter_bwd = self.bwd_var(Some(t_tup));
                    self.block.fwd.push(Instr {
                        var: inter_fwd,
                        expr: Expr::Member {
                            tuple: bundle,
                            member: id::member(1),
                        },
                    });
                    self.block.bwd_nonlin.push(Instr {
                        var: inter_bwd,
                        expr: Expr::Member {
                            tuple: self.block.inter_tup,
                            member: id::member(self.block.inter_mem.len()),
                        },
                    });
                    self.block.inter_mem.push(inter_fwd);

                    let lin = self.accum(var);
                    let unit = self.bwd_var(Some(self.unit));
                    let mut args: Vec<_> = args
                        .iter()
                        .map(|&arg| match self.f.types[self.f.vars[arg.var()].ty()] {
                            Ty::Ref { .. } => self.get_cotan(arg),
                            _ => self.get_accum(arg),
                        })
                        .collect();
                    args.push(lin.cot);
                    args.push(inter_bwd);
                    self.block.bwd_lin.push(Instr {
                        var: unit,
                        expr: Expr::Call {
                            id: *id,
                            generics: generics.clone(),
                            args: args.into(),
                        },
                    });
                    self.resolve(lin);

                    if let Ty::F64 = self.mapped_types[self.f.vars[var.var()].ty()] {
                        self.duals[var.var()] = Some((Src(None), Src(None)));
                    }
                }
            },
            Expr::For { arg, body, ret } => {
                let t_index = self.f.vars[arg.var()];
                let t_elem = self.f.vars[ret.var()];

                let mut block = Block {
                    fwd: vec![],
                    inter_mem: vec![],
                    inter_tup: self.bwd_var(None),
                    bwd_nonlin: vec![],
                    bwd_lin: vec![],
                };
                swap(&mut self.block, &mut block);
                let (t_inter, fwd_inter) = self.block(body);
                swap(&mut self.block, &mut block);

                let t_bundle = self.ty(Ty::Tuple {
                    members: [t_elem, t_inter].into(),
                });
                let bundle = self.fwd_var(t_bundle);
                block.fwd.push(Instr {
                    var: bundle,
                    expr: Expr::Tuple {
                        members: [self.get_re(*ret), fwd_inter].into(),
                    },
                });
                let t_arr_bundle = self.ty(Ty::Array {
                    index: t_index,
                    elem: t_bundle,
                });
                let arr_bundle = self.fwd_var(t_arr_bundle);
                self.block.fwd.push(Instr {
                    var: arr_bundle,
                    expr: Expr::For {
                        arg: *arg,
                        body: block.fwd.into(),
                        ret: bundle,
                    },
                });
                let fst_index = self.fwd_var(t_index);
                let fst_bundle = self.fwd_var(t_bundle);
                let elem = self.fwd_var(t_elem);
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::For {
                        arg: fst_index,
                        body: [
                            Instr {
                                var: fst_bundle,
                                expr: Expr::Index {
                                    array: arr_bundle,
                                    index: fst_index,
                                },
                            },
                            Instr {
                                var: elem,
                                expr: Expr::Member {
                                    tuple: fst_bundle,
                                    member: id::member(0),
                                },
                            },
                        ]
                        .into(),
                        ret: elem,
                    },
                });
                self.keep(var);
                let t_arr_inter = self.ty(Ty::Array {
                    index: t_index,
                    elem: t_inter,
                });
                let arr_inter = self.fwd_var(t_arr_inter);
                let snd_index = self.fwd_var(t_index);
                let snd_bundle = self.fwd_var(t_bundle);
                let inter = self.fwd_var(t_inter);
                self.block.fwd.push(Instr {
                    var: arr_inter,
                    expr: Expr::For {
                        arg: snd_index,
                        body: [
                            Instr {
                                var: snd_bundle,
                                expr: Expr::Index {
                                    array: arr_bundle,
                                    index: snd_index,
                                },
                            },
                            Instr {
                                var: inter,
                                expr: Expr::Member {
                                    tuple: snd_bundle,
                                    member: id::member(1),
                                },
                            },
                        ]
                        .into(),
                        ret: inter,
                    },
                });

                let arr_inter_bwd = self.bwd_var(Some(t_arr_inter));
                self.block.bwd_nonlin.push(Instr {
                    var: arr_inter_bwd,
                    expr: Expr::Member {
                        tuple: self.block.inter_tup,
                        member: id::member(self.block.inter_mem.len()),
                    },
                });
                self.block.inter_mem.push(arr_inter);

                let lin = self.accum(var);
                let bwd_acc = self.get_accum(*ret);
                let bwd_cot = self.bwd_var(Some(t_elem));
                let mut bwd_body = vec![
                    Instr {
                        var: bwd_cot,
                        expr: Expr::Index {
                            array: lin.cot,
                            index: *arg,
                        },
                    },
                    Instr {
                        var: block.inter_tup,
                        expr: Expr::Index {
                            array: arr_inter_bwd,
                            index: *arg,
                        },
                    },
                ];
                bwd_body.append(&mut block.bwd_nonlin);
                let unit = self.bwd_var(Some(self.unit));
                bwd_body.push(Instr {
                    var: unit,
                    expr: Expr::Add {
                        accum: bwd_acc,
                        addend: bwd_cot,
                    },
                });
                block.bwd_lin.reverse();
                bwd_body.append(&mut block.bwd_lin);
                let bwd_ret = self.bwd_var(Some(self.unit));
                bwd_body.push(Instr {
                    var: bwd_ret,
                    expr: Expr::Unit,
                });
                let t_arr_unit = self.ty(Ty::Array {
                    index: t_index,
                    elem: self.unit,
                });
                let arr_unit = self.bwd_var(Some(t_arr_unit));
                self.block.bwd_lin.push(Instr {
                    var: arr_unit,
                    expr: Expr::For {
                        arg: *arg,
                        body: bwd_body.into(),
                        ret: bwd_ret,
                    },
                });
                self.resolve(lin);
            }

            &Expr::Accum { shape } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Accum {
                        shape: self.get_re(shape),
                    },
                });

                let cot = self.bwd_var(Some(self.f.vars[shape.var()]));
                self.cotans[var.var()] = Some(cot);
                self.stack.push(take(&mut self.block.bwd_nonlin));
            }

            &Expr::Add { accum, addend } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Add {
                        accum,
                        addend: self.get_re(addend),
                    },
                });

                self.block.bwd_nonlin.push(Instr {
                    var,
                    expr: Expr::Unit,
                });
                let lin = self.accum(var);
                let unit = self.bwd_var(Some(self.unit));
                self.block.bwd_lin.push(Instr {
                    var: unit,
                    expr: Expr::Add {
                        accum: self.get_accum(addend),
                        addend: self.get_cotan(accum),
                    },
                });
                self.resolve(lin);
            }

            &Expr::Resolve { var: accum } => {
                self.block.fwd.push(Instr {
                    var,
                    expr: Expr::Resolve { var: accum },
                });

                let mut bwd_nonlin = replace(&mut self.block.bwd_nonlin, self.stack.pop().unwrap());
                bwd_nonlin.reverse();
                self.block.bwd_lin.append(&mut bwd_nonlin);
                let acc = self.bwd_var(Some(self.f.vars[accum.var()]));
                self.block.bwd_lin.push(Instr {
                    var: self.get_cotan(accum),
                    expr: Expr::Resolve { var: acc },
                });
                self.keep(var);
                self.block.bwd_nonlin.push(Instr {
                    var: acc,
                    expr: Expr::Accum { shape: var },
                });
                self.accums[var.var()] = Some(acc);
                if let Ty::F64 = self.mapped_types[self.f.vars[var.var()].ty()] {
                    self.duals[var.var()] = Some((Src(None), Src(None)));
                }
            }
        }
    }
}

/// Return the forward and backward pass for the transpose of `f`.
pub fn transpose(f: &Func, deps: &[(&[Ty], id::Ty)]) -> (Func, Func) {
    let mut bwd_vars: Vec<_> = f.vars.iter().map(|&t| Some(t)).collect();
    let real_shape = id::var(bwd_vars.len());
    bwd_vars.push(Some(DUAL));
    let inter_tup = id::var(bwd_vars.len());
    bwd_vars.push(None);

    let mut tp = Transpose {
        f,
        deps,
        mapped_types: f
            .types
            .iter()
            .enumerate()
            .map(|(i, ty)| match ty {
                Ty::Unit => Ty::Unit,
                Ty::Bool => Ty::Bool,
                Ty::F64 => {
                    if !is_primitive(id::ty(i)) {
                        panic!()
                    }
                    Ty::F64
                }
                &Ty::Fin { size } => Ty::Fin { size },
                &Ty::Generic { id } => Ty::Generic { id },
                &Ty::Ref { inner } => {
                    if is_primitive(inner) {
                        panic!()
                    }
                    Ty::Ref { inner }
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
        types: indexset! { Ty::Unit },
        unit: id::ty(f.types.len()),
        fwd_vars: f.vars.to_vec(),
        bwd_vars,
        real_shape,
        prims: vec![None; f.vars.len()].into(),
        duals: vec![None; f.vars.len()].into(),
        accums: vec![None; f.vars.len()].into(),
        cotans: vec![None; f.vars.len()].into(),
        stack: vec![],
        block: Block {
            fwd: vec![],
            inter_tup,
            inter_mem: vec![],
            bwd_nonlin: vec![],
            bwd_lin: vec![],
        },
    };

    let mut bwd_params: Vec<_> = f
        .params
        .iter()
        .map(|&param| {
            let t = f.vars[param.var()];
            match &f.types[t.ty()] {
                &Ty::Ref { inner } => {
                    let cot = tp.bwd_var(Some(inner));
                    tp.cotans[param.var()] = Some(cot);
                    cot
                }
                _ => {
                    let t_acc = tp.ty(Ty::Ref { inner: t });
                    tp.keep(param);
                    let acc = tp.bwd_var(Some(t_acc));
                    if let Ty::F64 = tp.mapped_types[t.ty()] {
                        tp.duals[param.var()] = Some((Src(None), Src(None)));
                    }
                    tp.accums[param.var()] = Some(acc);
                    acc
                }
            }
        })
        .collect();

    let (t_intermediates, fwd_inter) = tp.block(&f.body);
    let fwd_ret = tp.get_re(f.ret);
    let bwd_acc = tp.get_accum(f.ret);

    let mut bwd_types = tp.mapped_types;
    bwd_types.extend(tp.types.into_iter());

    let mut fwd_types = bwd_types.clone();
    let t_bundle = id::ty(fwd_types.len());
    fwd_types.push(Ty::Tuple {
        members: [f.vars[f.ret.var()], t_intermediates].into(),
    });
    let mut fwd_vars = tp.fwd_vars;
    let fwd_bundle = id::var(fwd_vars.len());
    fwd_vars.push(t_bundle);
    let mut fwd_body = tp.block.fwd;
    fwd_body.push(Instr {
        var: fwd_bundle,
        expr: Expr::Tuple {
            members: [fwd_ret, fwd_inter].into(),
        },
    });

    let mut bwd_vars: Vec<_> = tp.bwd_vars.into_iter().map(|t| t.unwrap()).collect();
    let bwd_cot = id::var(bwd_vars.len());
    bwd_vars.push(f.vars[f.ret.var()]);
    let bwd_unit = id::var(bwd_vars.len());
    bwd_vars.push(tp.unit);
    bwd_params.push(bwd_cot);
    bwd_params.push(tp.block.inter_tup);
    let mut bwd_body = vec![Instr {
        var: tp.real_shape,
        expr: Expr::F64 { val: 0. },
    }];
    bwd_body.append(&mut tp.block.bwd_nonlin);
    bwd_body.push(Instr {
        var: bwd_unit,
        expr: Expr::Add {
            accum: bwd_acc,
            addend: bwd_cot,
        },
    });
    let mut bwd_lin = tp.block.bwd_lin;
    bwd_lin.reverse();
    bwd_body.append(&mut bwd_lin);
    let bwd_ret = id::var(bwd_vars.len()); // separate var, because `bwd_unit` might not be in scope
    bwd_vars.push(tp.unit);
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
            ret: fwd_bundle,
            body: fwd_body.into(),
        },
        Func {
            generics: f.generics.clone(),
            types: bwd_types.into(),
            vars: bwd_vars.into(),
            params: bwd_params.into(),
            ret: bwd_ret,
            body: bwd_body.into(),
        },
    )
}
