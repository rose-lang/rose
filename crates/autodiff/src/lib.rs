use enumset::EnumSet;
use indexmap::IndexSet;
use rose::{id, Binop, Constraint, Expr, FuncNode, Function, Instr, Ty, Unop};

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

            Expr::Slice { array: _, index: _ } => todo!(),
            Expr::Field {
                tuple: _,
                member: _,
            } => todo!(),

            &Expr::Unary { op, arg } => match op {
                Unop::Not => {
                    let x = self.set(code, ty, Expr::Unary { op, arg });
                    x
                }

                Unop::Neg => {
                    let x = self.set(code, ty, Expr::Unary { op, arg });
                    x
                }
                Unop::Abs => {
                    let x = self.set(code, ty, Expr::Unary { op, arg });
                    let sign = self.set(
                        code,
                        ty,
                        Expr::Unary {
                            op: Unop::Sign,
                            arg,
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

                        // sum the tangent components
                        let z_dx = self.set(
                            code,
                            ty,
                            Expr::Binary {
                                op: Binop::Div,
                                left: self.set(
                                    code,
                                    ty,
                                    Expr::Binary {
                                        op: Binop::Sub,
                                        left: a,
                                        right: b,
                                    },
                                ),
                                right: c,
                            },
                        );

                        let dual: Box<[id::Var]> = vec![z_x, z_dx].into_boxed_slice();
                        let z = self.set(code, ty, Expr::Tuple { members: dual });
                        z
                    }
                }
            }

            Expr::Call {
                id: _,
                generics: _,
                args: _,
            } => todo!(),
            &Expr::Select { cond, then, els } => {
                let x = self.set(code, ty, Expr::Select { cond, then, els });
                x
            }

            // recursively call expr on the blocks in the for loop
            &Expr::For { arg, body, ret } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::For {
                        arg,
                        body: body
                            .iter()
                            .map(|&block| self.expr(code, ty, &block.expr))
                            .collect(),
                        ret,
                    },
                );
                x
            }
            Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Accum {
                        shape: *shape,
                        arg: *arg,
                        body: body
                            .iter()
                            .map(|&block| self.expr(code, ty, &block.expr))
                            .collect(),
                        ret: *ret,
                    },
                );
                x
            }

            Expr::Add {
                accum: _,
                addend: _,
            } => todo!(),
            Expr::Read {
                var,
                arg,
                body,
                ret,
            } => {
                let x = self.set(
                    code,
                    ty,
                    Expr::Read {
                        var: *var,
                        arg: *arg,
                        body: body
                            .iter()
                            .map(|&block| self.expr(code, ty, &block.expr))
                            .collect(),
                        ret: *ret,
                    },
                );
                x
            }
            Expr::Ask { var: _ } => todo!(),
        }
    }
}

pub fn forward(f: Derivative) -> Function {
    let Derivative { f } = f;
    let mut g = Forward {
        generics: f.generics.to_vec(),
        types: IndexSet::new(),
        old_types: vec![],
        old_vars: &f.vars,
        vars: vec![],
        mapping: vec![None; f.vars.len()],
        var_mapping: vec![None; f.vars.len()],
    };
    let unitvar = g.unitvar();

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
        g.mapping[var_idx] = Some((x_arg, dx_arg));
    }

    Function {
        generics: g.generics.into(),
        types: g.types.into_iter().collect(),
        vars: g.vars.into(),
        params: f.params.into(),
        ret: f.ret,
        body: code.into(),
    }
}

pub fn unzip(f: Derivative) -> (Function, Linear) {
    (f.f, Linear) // TODO
}

pub fn transpose(f: Linear) -> Linear {
    f // TODO
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug)]
    struct TestFuncNode {
        f: Function,
    }

    impl rose::FuncNode for TestFuncNode {
        fn def(&self) -> &rose::Function {
            &self.f
        }

        fn get(&self, _id: id::Function) -> Option<Self> {
            None
        }
    }

    fn func1() -> Function {
        Function {
            generics: vec![].into(),
            types: vec![Ty::F64].into(),
            vars: vec![id::ty(0), id::ty(2)].into(),
            params: vec![].into(),
            ret: id::var(0),
            body: vec![
                Instr {
                    var: id::var(0),
                    expr: Expr::F64 { val: 42. },
                },
                Instr {
                    var: id::var(2),
                    expr: Expr::Accum {
                        shape: id::var(0),
                        arg: id::var(1),
                        body: Box::new([]),
                        ret: id::var(2),
                    },
                },
            ]
            .into(),
        }
    }

    #[test]
    fn test_scope_mapping() {
        // get funcs
        let og_func = TestFuncNode { f: func1() };
        let cloned_func = og_func.f.clone();
        let derivative = derivative(og_func);
        let new_func = forward(derivative);

        // extract types and vars
        let old_types = &cloned_func.types;
        let new_types = &new_func.types;
        let old_vars = &cloned_func.vars;
        let new_vars = &new_func.vars;

        // check if the variable indices of Scope types match up
        for (i, var) in old_vars.iter().enumerate() {
            if let Ty::Scope { id, .. } = old_types[i] {
                let old_id = id.var();
                if let Some(&new_id) = scope_mapping.get(&old_id) {
                    // check if the new var ID matches the new scope ID
                    assert_eq!(new_id.var(), var.ty());
                }
            }
        }

        // check if the new block index is strictly greater than the number of scopes in the original function
    }
}
