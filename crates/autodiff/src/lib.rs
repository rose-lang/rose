use rose::{id, Binop, Expr, Func, Instr, Ty, Unop};

const REAL: id::Ty = id::ty(0);
const DUAL: id::Ty = id::ty(1);

// alphabetical order
const DU: id::Member = id::member(0);
const RE: id::Member = id::member(1);

fn map(t: id::Ty) -> id::Ty {
    id::ty(t.ty() + 2)
}

struct Autodiff<'a> {
    vars: &'a mut Vec<id::Ty>,
    code: Vec<Instr>,
}

impl Autodiff<'_> {
    fn set(&mut self, t: id::Ty, expr: Expr) -> id::Var {
        let var = id::var(self.vars.len());
        self.vars.push(t);
        self.code.push(Instr { var, expr });
        var
    }

    fn real(&mut self, expr: Expr) -> id::Var {
        self.set(REAL, expr)
    }

    fn dual(&mut self, expr: Expr) -> id::Var {
        self.set(DUAL, expr)
    }

    fn re(&mut self, var: id::Var) -> id::Var {
        self.real(Expr::Member {
            tuple: var,
            member: RE,
        })
    }

    fn du(&mut self, var: id::Var) -> id::Var {
        self.real(Expr::Member {
            tuple: var,
            member: DU,
        })
    }

    fn unpack(&mut self, var: id::Var) -> (id::Var, id::Var) {
        let x = self.re(var);
        let dx = self.du(var);
        (x, dx)
    }

    fn pack(&mut self, var: id::Var, x: id::Var, dx: id::Var) {
        self.code.push(Instr {
            var,
            expr: Expr::Tuple {
                members: [dx, x].into(), // alphabetical order
            },
        })
    }

    fn child(&mut self, orig: &[Instr]) -> Box<[Instr]> {
        Autodiff {
            vars: self.vars,
            code: vec![],
        }
        .block(orig)
    }

    fn block(mut self, orig: &[Instr]) -> Box<[Instr]> {
        for Instr { var, expr } in orig {
            self.instr(*var, expr);
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
            &Expr::Ask { var } => self.code.push(Instr {
                var,
                expr: Expr::Ask { var },
            }),
            &Expr::Add { accum, addend } => self.code.push(Instr {
                var,
                expr: Expr::Add { accum, addend },
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
            Expr::Read {
                var: orig,
                arg,
                body,
                ret,
            } => {
                let body = self.child(body);
                self.code.push(Instr {
                    var,
                    expr: Expr::Read {
                        var: *orig,
                        arg: *arg,
                        body,
                        ret: *ret,
                    },
                })
            }
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => {
                let body = self.child(body);
                self.code.push(Instr {
                    var,
                    expr: Expr::Accum {
                        shape: *shape,
                        arg: *arg,
                        body,
                        ret: *ret,
                    },
                })
            }

            // interesting cases
            &Expr::F64 { val } => {
                let x = self.real(Expr::F64 { val });
                let dx = self.dual(Expr::F64 { val: 0. });
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
                    let (x, dx) = self.unpack(arg);
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
                    let (x, dx) = self.unpack(arg);
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
                Unop::Sign => {
                    let x = self.re(arg);
                    let y = self.real(Expr::Unary {
                        op: Unop::Sign,
                        arg: x,
                    });
                    let dy = self.dual(Expr::F64 { val: 0. });
                    self.pack(var, y, dy)
                }
                Unop::Sqrt => {
                    let (x, dx) = self.unpack(arg);
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
                Binop::And | Binop::Or | Binop::Iff | Binop::Xor => self.code.push(Instr {
                    var,
                    expr: Expr::Binary { op, left, right },
                }),

                // less boring cases
                Binop::Neq | Binop::Lt | Binop::Leq | Binop::Eq | Binop::Gt | Binop::Geq => {
                    let x = self.re(left);
                    let y = self.re(right);
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
                    let (x, dx) = self.unpack(left);
                    let (y, dy) = self.unpack(right);
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
                    let (x, dx) = self.unpack(left);
                    let (y, dy) = self.unpack(right);
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
                    let (x, dx) = self.unpack(left);
                    let (y, dy) = self.unpack(right);
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
                    let (x, dx) = self.unpack(left);
                    let (y, dy) = self.unpack(right);
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

pub fn jvp(f: &Func) -> Func {
    let mut types = vec![Ty::F64, Ty::F64];
    types.extend(f.types.iter().map(|ty| match ty {
        // boring cases
        Ty::Unit => Ty::Unit,
        Ty::Bool => Ty::Bool,
        &Ty::Fin { size } => Ty::Fin { size },
        &Ty::Generic { id } => Ty::Generic { id },
        &Ty::Scope { kind, id } => Ty::Scope { kind, id },

        // less boring cases
        &Ty::Ref { scope, inner } => Ty::Ref {
            scope: map(scope),
            inner: map(inner),
        },
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
    let mut vars = f.vars.iter().copied().map(map).collect();
    let body = Autodiff {
        vars: &mut vars,
        code: vec![],
    }
    .block(&f.body);
    Func {
        generics: f.generics.clone(),
        types: types.into(),
        vars: vars.into(),
        params: f.params.clone(),
        ret: f.ret,
        body,
    }
}
