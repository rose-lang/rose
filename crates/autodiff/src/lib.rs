use rose::{id, Binop, Expr, Func, Instr, Ty, Unop};

// see docstring of `pub fn jvp` below

const REAL: id::Ty = id::ty(0);
const DUAL: id::Ty = id::ty(1);

// JS frontend requires field names to be alphabetized, and `"du"` comes before `"re"`
const DU: id::Member = id::member(0);
const RE: id::Member = id::member(1);

fn map(t: id::Ty) -> id::Ty {
    id::ty(t.ty() + 2)
}

struct Autodiff<'a> {
    old_types: &'a [Ty],
    old_vars: &'a [id::Ty],
    new_vars: &'a mut Vec<id::Ty>,
    unpacked: &'a mut [Option<(id::Var, id::Var)>],
    dual_zero: id::Var,
    code: Vec<Instr>,
}

impl Autodiff<'_> {
    fn set(&mut self, t: id::Ty, expr: Expr) -> id::Var {
        let var = id::var(self.new_vars.len());
        self.new_vars.push(t);
        self.code.push(Instr { var, expr });
        var
    }

    fn real(&mut self, expr: Expr) -> id::Var {
        self.set(REAL, expr)
    }

    fn dual(&mut self, expr: Expr) -> id::Var {
        self.set(DUAL, expr)
    }

    fn unpack(&mut self, var: id::Var) {
        let i = var.var();
        if self.unpacked[i].is_none() {
            if let Ty::F64 = self.old_types[self.old_vars[i].ty()] {
                let x = self.real(Expr::Member {
                    tuple: var,
                    member: RE,
                });
                let dx = self.dual(Expr::Member {
                    tuple: var,
                    member: DU,
                });
                self.unpacked[i] = Some((x, dx))
            }
        }
    }

    fn get(&self, var: id::Var) -> (id::Var, id::Var) {
        self.unpacked[var.var()].unwrap()
    }

    fn pack(&mut self, var: id::Var, x: id::Var, dx: id::Var) {
        self.unpacked[var.var()] = Some((x, dx));
        self.code.push(Instr {
            var,
            expr: Expr::Tuple {
                members: [dx, x].into(), // alphabetical order
            },
        })
    }

    fn child(&mut self, orig: &[Instr]) -> Box<[Instr]> {
        Autodiff {
            old_types: self.old_types,
            old_vars: self.old_vars,
            new_vars: self.new_vars,
            unpacked: self.unpacked,
            dual_zero: self.dual_zero,
            code: vec![],
        }
        .block(orig)
    }

    fn block(mut self, orig: &[Instr]) -> Box<[Instr]> {
        for Instr { var, expr } in orig {
            self.instr(*var, expr);
            self.unpack(*var);
        }
        self.code.into()
    }

    fn instr(&mut self, var: id::Var, expr: &Expr) {
        match expr {
            // boring cases
            Expr::Unit => self.code.push(Instr {
                var,
                expr: Expr::Unit,
            }),
            &Expr::Bool { val } => self.code.push(Instr {
                var,
                expr: Expr::Bool { val },
            }),
            &Expr::Fin { val } => self.code.push(Instr {
                var,
                expr: Expr::Fin { val },
            }),
            Expr::Array { elems } => self.code.push(Instr {
                var,
                expr: Expr::Array {
                    elems: elems.clone(),
                },
            }),
            Expr::Tuple { members } => self.code.push(Instr {
                var,
                expr: Expr::Tuple {
                    members: members.clone(),
                },
            }),
            &Expr::Index { array, index } => self.code.push(Instr {
                var,
                expr: Expr::Index { array, index },
            }),
            &Expr::Member { tuple, member } => self.code.push(Instr {
                var,
                expr: Expr::Member { tuple, member },
            }),
            &Expr::Slice { array, index } => self.code.push(Instr {
                var,
                expr: Expr::Slice { array, index },
            }),
            &Expr::Field { tuple, member } => self.code.push(Instr {
                var,
                expr: Expr::Field { tuple, member },
            }),
            &Expr::Select { cond, then, els } => self.code.push(Instr {
                var,
                expr: Expr::Select { cond, then, els },
            }),
            &Expr::Accum { shape } => self.code.push(Instr {
                var,
                expr: Expr::Accum { shape },
            }),
            &Expr::Add { accum, addend } => self.code.push(Instr {
                var,
                expr: Expr::Add { accum, addend },
            }),
            &Expr::Resolve { var: container } => self.code.push(Instr {
                var,
                expr: Expr::Resolve { var: container },
            }),

            // less boring cases
            Expr::Call { id, generics, args } => self.code.push(Instr {
                var,
                expr: Expr::Call {
                    id: *id,
                    generics: generics.iter().copied().map(map).collect(),
                    args: args.clone(),
                },
            }),
            Expr::For { arg, body, ret } => {
                let body = self.child(body);
                self.code.push(Instr {
                    var,
                    expr: Expr::For {
                        arg: *arg,
                        body,
                        ret: *ret,
                    },
                })
            }

            // interesting cases
            &Expr::F64 { val } => {
                let x = self.real(Expr::F64 { val });
                let dx = self.dual_zero;
                self.pack(var, x, dx)
            }
            &Expr::Unary { op, arg } => match op {
                // boring case
                Unop::Not => self.code.push(Instr {
                    var,
                    expr: Expr::Unary { op: Unop::Not, arg },
                }),

                // interesting cases
                Unop::Neg => {
                    let (x, dx) = self.get(arg);
                    let y = self.real(Expr::Unary {
                        op: Unop::Neg,
                        arg: x,
                    });
                    let dy = self.dual(Expr::Unary {
                        op: Unop::Neg,
                        arg: dx,
                    });
                    self.pack(var, y, dy)
                }
                Unop::Abs => {
                    let (x, dx) = self.get(arg);
                    let y = self.real(Expr::Unary {
                        op: Unop::Abs,
                        arg: x,
                    });
                    let sign = self.real(Expr::Unary {
                        op: Unop::Sign,
                        arg: x,
                    });
                    let dy = self.dual(Expr::Binary {
                        op: Binop::Mul,
                        left: dx,
                        right: sign,
                    });
                    self.pack(var, y, dy)
                }
                Unop::Sign | Unop::Ceil | Unop::Floor | Unop::Trunc => {
                    let (x, _) = self.get(arg);
                    let y = self.real(Expr::Unary { op, arg: x });
                    let dy = self.dual_zero;
                    self.pack(var, y, dy)
                }
                Unop::Sqrt => {
                    let (x, dx) = self.get(arg);
                    let y = self.real(Expr::Unary {
                        op: Unop::Sqrt,
                        arg: x,
                    });
                    let z = self.real(Expr::Binary {
                        op: Binop::Add,
                        left: y,
                        right: y,
                    });
                    let dy = self.dual(Expr::Binary {
                        op: Binop::Div,
                        left: dx,
                        right: z,
                    });
                    self.pack(var, y, dy)
                }
            },
            &Expr::Binary { op, left, right } => match op {
                // boring cases
                Binop::And
                | Binop::Or
                | Binop::Iff
                | Binop::Xor
                | Binop::INeq
                | Binop::ILt
                | Binop::ILeq
                | Binop::IEq
                | Binop::IGt
                | Binop::IGeq => self.code.push(Instr {
                    var,
                    expr: Expr::Binary { op, left, right },
                }),

                // less boring cases
                Binop::Neq | Binop::Lt | Binop::Leq | Binop::Eq | Binop::Gt | Binop::Geq => {
                    let (x, _) = self.get(left);
                    let (y, _) = self.get(right);
                    self.code.push(Instr {
                        var,
                        expr: Expr::Binary {
                            op,
                            left: x,
                            right: y,
                        },
                    })
                }

                // interesting cases
                Binop::Add => {
                    let (x, dx) = self.get(left);
                    let (y, dy) = self.get(right);
                    let z = self.real(Expr::Binary {
                        op: Binop::Add,
                        left: x,
                        right: y,
                    });
                    let dz = self.dual(Expr::Binary {
                        op: Binop::Add,
                        left: dx,
                        right: dy,
                    });
                    self.pack(var, z, dz)
                }
                Binop::Sub => {
                    let (x, dx) = self.get(left);
                    let (y, dy) = self.get(right);
                    let z = self.real(Expr::Binary {
                        op: Binop::Sub,
                        left: x,
                        right: y,
                    });
                    let dz = self.dual(Expr::Binary {
                        op: Binop::Sub,
                        left: dx,
                        right: dy,
                    });
                    self.pack(var, z, dz)
                }
                Binop::Mul => {
                    let (x, dx) = self.get(left);
                    let (y, dy) = self.get(right);
                    let z = self.real(Expr::Binary {
                        op: Binop::Mul,
                        left: x,
                        right: y,
                    });
                    let a = self.dual(Expr::Binary {
                        op: Binop::Mul,
                        left: dx,
                        right: y,
                    });
                    let b = self.dual(Expr::Binary {
                        op: Binop::Mul,
                        left: dy,
                        right: x,
                    });
                    let dz = self.dual(Expr::Binary {
                        op: Binop::Add,
                        left: a,
                        right: b,
                    });
                    self.pack(var, z, dz)
                }
                Binop::Div => {
                    let (x, dx) = self.get(left);
                    let (y, dy) = self.get(right);
                    let z = self.real(Expr::Binary {
                        op: Binop::Div,
                        left: x,
                        right: y,
                    });
                    let a = self.real(Expr::Binary {
                        op: Binop::Div,
                        left: z,
                        right: y,
                    });
                    let b = self.dual(Expr::Binary {
                        op: Binop::Div,
                        left: dx,
                        right: y,
                    });
                    let c = self.dual(Expr::Binary {
                        op: Binop::Mul,
                        left: dy,
                        right: a,
                    });
                    let dz = self.dual(Expr::Binary {
                        op: Binop::Sub,
                        left: b,
                        right: c,
                    });
                    self.pack(var, z, dz)
                }
            },
        }
    }
}

/// Return a function that computes the Jacobian-vector product of this function.
///
/// The first two types in the new function are the nonlinear and linear `F64` types, respectively.
/// Every type from the original function is then mapped over directly in a one-to-one fashion, with
/// indices shifted by two as necessary. Instances of the `F64` type from the original function are
/// replaced with a `Tuple` type whose members are the linear and nonlinear `F64` types,
/// respectively (note that this member order does not match the order of the types themselves).
pub fn jvp(f: &Func) -> Func {
    let mut types = vec![Ty::F64, Ty::F64];
    types.extend(f.types.iter().map(|ty| match ty {
        // boring cases
        Ty::Unit => Ty::Unit,
        Ty::Bool => Ty::Bool,
        &Ty::Fin { size } => Ty::Fin { size },
        &Ty::Generic { id } => Ty::Generic { id },

        // less boring cases
        &Ty::Ref { inner } => Ty::Ref { inner: map(inner) },
        &Ty::Array { index, elem } => Ty::Array {
            index: map(index),
            elem: map(elem),
        },
        Ty::Tuple { members } => Ty::Tuple {
            members: members.iter().copied().map(map).collect(),
        },

        // interesting case
        Ty::F64 => Ty::Tuple {
            members: [DUAL, REAL].into(), // alphabetical order
        },
    }));
    let mut vars: Vec<_> = f.vars.iter().copied().map(map).collect();
    let dual_zero = id::var(vars.len());
    vars.push(DUAL);
    let mut ad = Autodiff {
        old_types: &f.types,
        old_vars: &f.vars,
        new_vars: &mut vars,
        unpacked: &mut vec![None; f.vars.len()],
        dual_zero,
        code: vec![Instr {
            var: dual_zero,
            expr: Expr::F64 { val: 0. },
        }],
    };
    for &param in f.params.iter() {
        ad.unpack(param);
    }
    let body = ad.block(&f.body);
    Func {
        generics: f.generics.clone(),
        types: types.into(),
        vars: vars.into(),
        params: f.params.clone(),
        ret: f.ret,
        body,
    }
}
