use enumset::EnumSet;
use indexmap::IndexSet;
use rose::{id, Binop, Constraint, Expr, Func, Instr, Ty, Unop};

struct Forward<'a> {
    generics: Vec<EnumSet<Constraint>>,
    types: IndexSet<Ty>,
    old_types: Vec<(id::Ty, id::Ty)>,
    old_vars: &'a [id::Ty],
    vars: Vec<id::Ty>,
    mapping: Vec<Option<(id::Var, id::Var)>>,
    var_mapping: Vec<Option<id::Var>>,
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
        // check if this var has a mapping from the old function
        let id = id::var(self.vars.len());

        if let Some(new_var) = self.var_mapping[id.var()] {
            self.vars.push(self.vars[new_var.var()]);
        } else {
            self.vars.push(t);
            self.var_mapping[id.var()] = Some(id);
        }
        id
    }

    // F64s get differentiated as dual numbers
    fn extract(
        &mut self,
        left: id::Var,
        right: id::Var,
        code: &mut Vec<Instr>,
        ty: id::Ty,
    ) -> (id::Var, id::Var, id::Var, id::Var) {
        // extract the components of the left tuple
        let left_x = self.set(
            code,
            ty,
            Expr::Member {
                tuple: left,
                member: id::member(0),
            },
        );
        let left_dx = self.set(
            code,
            ty,
            Expr::Member {
                tuple: left,
                member: id::member(1),
            },
        );

        // extract the components of the right tuple
        let right_x = self.set(
            code,
            ty,
            Expr::Member {
                tuple: right,
                member: id::member(0),
            },
        );
        let right_dx = self.set(
            code,
            ty,
            Expr::Member {
                tuple: right,
                member: id::member(1),
            },
        );

        (left_x, left_dx, right_x, right_dx)
    }

    fn unitvar(&mut self) -> id::Var {
        let ty = self.newtype(Ty::Unit);
        self.newvar(ty)
    }

    fn map(&mut self, var: id::Var) -> (id::Var, id::Var) {
        if let Some((x, dx)) = self.mapping[var.var()] {
            (x, dx)
        } else {
            let ty = self.old_vars[var.var()];
            let dx = self.newvar(self.tangent(ty));
            let x = self.newvar(self.primal(ty));
            (x, dx)
        }
    }

    fn set(&mut self, code: &mut Vec<Instr>, ty: id::Ty, expr: Expr) -> id::Var {
        let var = self.newvar(ty);
        code.push(Instr { var, expr });
        var
    }

    fn expr(&mut self, code: &mut Vec<Instr>, ty: id::Ty, expr: &Expr) -> id::Var {
        match expr {
            Expr::Unit => {
                let x = self.set(code, ty, Expr::Unit);
                x
            }
            &Expr::Bool { val } => {
                let x = self.set(code, ty, Expr::Bool { val });
                x
            }
            &Expr::F64 { val } => {
                // return a dual number of the form (x, dx)
                let tuple: Box<[id::Var]> = vec![
                    self.set(code, ty, Expr::F64 { val }),
                    self.set(code, ty, Expr::F64 { val: 0. }),
                ]
                .into_boxed_slice();
                let dual = self.set(code, ty, Expr::Tuple { members: tuple });
                dual
            }
            &Expr::Fin { val } => {
                let x = self.set(code, ty, Expr::Fin { val });
                x
            }

            Expr::Array { elems } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Array {
                        elems: elems.clone(),
                    },
                );
                x
            }
            Expr::Tuple { members } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Tuple {
                        members: members.clone(),
                    },
                );
                x
            }
            &Expr::Index { array, index } => {
                let x = self.set(code, ty, Expr::Index { array, index });
                x
            }
            &Expr::Member { tuple, member } => {
                let x = self.set(code, ty, Expr::Member { tuple, member });
                x
            }

            Expr::Slice { array, index } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Slice {
                        array: *array,
                        index: *index,
                    },
                );
                x
            }
            Expr::Field { tuple, member } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Field {
                        tuple: *tuple,
                        member: *member,
                    },
                );
                x
            }

            &Expr::Unary { op, arg } => match op {
                Unop::Not | Unop::Neg => {
                    let x = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(0),
                        },
                    );
                    let dx = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(1),
                        },
                    );

                    let z_val = self.set(code, ty, Expr::Unary { op, arg: x });
                    let z_dx = self.set(code, ty, Expr::Unary { op, arg: dx });

                    let dual: Box<[id::Var]> = vec![z_val, z_dx].into_boxed_slice();
                    let z = self.set(code, ty, Expr::Tuple { members: dual });
                    z
                }
                Unop::Abs => {
                    // Unpack F64 Tuple
                    let x = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(0),
                        },
                    );
                    let dx = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(1),
                        },
                    );

                    // Compute the absolute value of the primal and tangent components
                    let x_abs = self.set(
                        code,
                        ty,
                        Expr::Unary {
                            op: Unop::Abs,
                            arg: x,
                        },
                    );

                    let sign = self.set(
                        code,
                        ty,
                        Expr::Unary {
                            op: Unop::Sign,
                            arg: x,
                        },
                    );

                    let dx_abs = self.set(
                        code,
                        ty,
                        Expr::Binary {
                            op: Binop::Mul,
                            left: dx,
                            right: sign,
                        },
                    );
                    let dual: Box<[id::Var]> = vec![x_abs, dx_abs].into_boxed_slice();
                    let z = self.set(code, ty, Expr::Tuple { members: dual });
                    z
                }
                Unop::Sign => {
                    // Extract the components of the tuple
                    let x = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(0),
                        },
                    );
                    let dx = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(1),
                        },
                    );
                    let z = self.set(
                        code,
                        ty,
                        Expr::Unary {
                            op: Unop::Sign,
                            arg: x,
                        },
                    );

                    let z_dx = self.set(code, ty, Expr::F64 { val: 0. });
                    let dual: Box<[id::Var]> = vec![z, z_dx].into_boxed_slice();
                    let z = self.set(code, ty, Expr::Tuple { members: dual });
                    z
                }
                Unop::Sqrt => {
                    let x = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: arg,
                            member: id::member(0),
                        },
                    );
                    let dx = self.set(
                        code,
                        ty,
                        Expr::Member {
                            tuple: x,
                            member: id::member(1),
                        },
                    );

                    let z = self.set(
                        code,
                        ty,
                        Expr::Unary {
                            op: Unop::Sqrt,
                            arg: x,
                        },
                    );

                    // Calculate 2 * z for later use
                    let two = self.set(code, ty, Expr::F64 { val: 2. });
                    let z_two = self.set(
                        code,
                        ty,
                        Expr::Binary {
                            op: Binop::Mul,
                            left: two,
                            right: z,
                        },
                    );

                    // dx = dy / (2 * sqrt(x))
                    let z_dx = self.set(
                        code,
                        ty,
                        Expr::Binary {
                            op: Binop::Div,
                            left: dx,
                            right: z_two,
                        },
                    );
                    let dual: Box<[id::Var]> = vec![z, z_dx].into_boxed_slice();
                    let z = self.set(code, ty, Expr::Tuple { members: dual });
                    z
                }
            },
            &Expr::Binary { op, left, right } => {
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
                        let z = self.set(code, ty, Expr::Binary { op, left, right });
                        z
                    }

                    Binop::Add | Binop::Sub => {
                        let (left_x, left_dx, right_x, right_dx) =
                            self.extract(left, right, code, ty);

                        // add/subtract the components
                        let z_x = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: left_x,
                                right: right_x,
                            },
                        );
                        let z_dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: left_dx,
                                right: right_dx,
                            },
                        );

                        // create a new tuple containing the sum of the tuples
                        let dual: Box<[id::Var]> = vec![z_x, z_dx].into_boxed_slice();
                        let z = self.set(code, ty, Expr::Tuple { members: dual });
                        z
                    }
                    Binop::Mul => {
                        let (left_x, left_dx, right_x, right_dx) =
                            self.extract(left, right, code, ty);

                        let z_x = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: left_x,
                                right: right_x,
                            },
                        );
                        let a = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: left_dx,
                                right: right_x,
                            },
                        );
                        let b = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: right_dx,
                                right: left_x,
                            },
                        );

                        // sum the tangent components
                        let z_dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Add,
                                left: a,
                                right: b,
                            },
                        );

                        let dual: Box<[id::Var]> = vec![z_x, z_dx].into_boxed_slice();
                        let z = self.set(code, ty, Expr::Tuple { members: dual });
                        z
                    }
                    Binop::Div => {
                        let (left_x, left_dx, right_x, right_dx) =
                            self.extract(left, right, code, ty);

                        let z_x = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op,
                                left: left_x,
                                right: right_x,
                            },
                        );
                        let a = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: left_dx,
                                right: right_x,
                            },
                        );
                        let b = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: right_dx,
                                right: left_x,
                            },
                        );
                        let c = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Mul,
                                left: right_x,
                                right: right_x,
                            },
                        );

                        // calculate intermediate values to avoid borrowing issues
                        let diff_result = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Sub,
                                left: a,
                                right: b,
                            },
                        );

                        let z_dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Div,
                                left: diff_result,
                                right: c,
                            },
                        );

                        let dual: Box<[id::Var]> = vec![z_x, z_dx].into_boxed_slice();
                        let z = self.set(code, ty, Expr::Tuple { members: dual });
                        z
                    }
                }
            }

            Expr::Call { id, generics, args } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Call {
                        id: *id,
                        generics: generics.clone(),
                        args: args.clone(),
                    },
                );
                x
            }
            &Expr::Select { cond, then, els } => {
                let x = self.set(code, ty, Expr::Select { cond, then, els });
                x
            }

            // recursively call expr on the blocks in the for loop...this isn't working yet though because of Rust borrowing rules :(
            Expr::For { arg, body, ret } => {
                let processed_body: Vec<_> = body
                    .iter()
                    .map(|block| {
                        let processed_var = self.expr(code, ty, &block.expr);
                        Instr {
                            var: processed_var,
                            expr: block.expr.clone(),
                        }
                    })
                    .collect();

                // create a new For expression with the processed instructions
                let x = self.set(
                    code,
                    ty,
                    todo!(),
                    // Expr::For {
                    //     arg,
                    //     body: processed_body.into_boxed_slice(),
                    //     ret,
                    // },
                );
                x
            }
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => {
                let processed_body: Vec<_> = body
                    .iter()
                    .map(|block| {
                        let processed_var = self.expr(code, ty, &block.expr);
                        Instr {
                            var: processed_var,
                            expr: block.expr.clone(),
                        }
                    })
                    .collect();

                let x = self.set(
                    code,
                    ty,
                    Expr::Accum {
                        shape: *shape,
                        arg: *arg,
                        body: processed_body.into_boxed_slice(),
                        ret: *ret,
                    },
                );
                x
            }

            Expr::Add { accum, addend } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Add {
                        accum: *accum,
                        addend: *addend,
                    },
                );
                x
            }
            Expr::Read {
                var,
                arg,
                body,
                ret,
            } => {
                let processed_body: Vec<_> = body
                    .iter()
                    .map(|block| {
                        let processed_var = self.expr(code, ty, &block.expr);
                        Instr {
                            var: processed_var,
                            expr: block.expr.clone(),
                        }
                    })
                    .collect();

                let x = self.set(
                    code,
                    ty,
                    Expr::Read {
                        var: *var,
                        arg: *arg,
                        body: processed_body.into_boxed_slice(),
                        ret: *ret,
                    },
                );
                x
            }
            Expr::Ask { var } => {
                let x = self.set(code, ty, Expr::Ask { var: *var });
                x
            }
        }
    }
}

pub fn jvp(f: &Func) -> Func {
    let mut g = Forward {
        generics: f.generics.to_vec(),
        types: IndexSet::new(),
        old_types: vec![],
        old_vars: &f.vars,
        vars: vec![],
        mapping: vec![None; f.vars.len()],
        var_mapping: vec![None; f.vars.len()],
    };

    // create a mapping from the old scope IDs to the new scope IDs
    for (var_idx, _) in f.vars.iter().enumerate() {
        g.var_mapping.push(Some(id::var(var_idx)));
    }

    for ty in &f.types.to_vec() {
        let primal = match ty {
            Ty::Unit => Ty::Unit,
            Ty::Bool => Ty::Bool,
            Ty::F64 => Ty::F64,
            &Ty::Fin { size } => Ty::Fin { size },
            &Ty::Generic { id } => Ty::Generic { id },
            &Ty::Scope { kind, id } => Ty::Scope { kind, id },
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

    let mut code = vec![];

    // loop over each parameter in the function and handle them
    for param in &f.params.to_vec() {
        let var_idx = param.var();
        let t_arg = g.old_types[g.old_vars[var_idx].ty()].0;
        let tan_arg = g.old_types[g.old_vars[var_idx].ty()].1;
        let tup_arg = g.newtype(Ty::Tuple {
            members: [t_arg, tan_arg].into(),
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
        g.mapping[var_idx] = Some((x_arg, dx_arg));
    }

    Func {
        generics: g.generics.into(),
        types: g.types.into_iter().collect(),
        vars: g.vars.into(),
        params: f.params.clone(),
        ret: f.ret,
        body: code.into(),
    }
}
