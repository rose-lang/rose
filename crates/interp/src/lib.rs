use rose::{id, Binop, Block, Expr, Func, Function, Instr, Type, Typexpr, Unop};
use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

/// A resolved type.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Typeval {
    Unit,
    Bool,
    F64,
    Fin {
        size: usize,
    },
    Scope, // we erase scope info in the interpreter
    Ref {
        inner: Rc<Typeval>,
    },
    Array {
        index: Rc<Typeval>,
        elem: Rc<Typeval>,
    },
    Tuple {
        members: Rc<Vec<Typeval>>,
    },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Unit,
    Bool(bool),
    F64(f64),
    Fin(usize),
    // TODO: support `Ref`
    Array(Rc<Vec<Val>>), // assume all indices are `Fin`
    Tuple(Rc<Vec<Val>>),
}

struct Interpreter<'a> {
    f: &'a Function,
    generics: &'a [Typeval],
    vars: Vec<Option<Val>>,
}

impl<'a> Interpreter<'a> {
    fn pop(&mut self) -> Result<Val, Error> {
        self.stack.pop().ok_or(Error::EmptyStack)
    }

    fn pop_bool(&mut self) -> Result<bool, Error> {
        match self.pop()? {
            Val::Bool(x) => Ok(x),
            x => Err(type_error(Type::Bool, &x)),
        }
    }

    fn pop_f64(&mut self) -> Result<f64, Error> {
        match self.pop()? {
            Val::F64(x) => Ok(x),
            x => Err(type_error(Type::F64, &x)),
        }
    }

    fn unary_bool(&mut self, f: impl Fn(bool) -> bool) -> Result<(), Error> {
        let a = self.pop_bool()?;
        self.stack.push(Val::Bool(f(a)));
        Ok(())
    }

    fn unary_f64(&mut self, f: impl Fn(f64) -> f64) -> Result<(), Error> {
        let a = self.pop_f64()?;
        self.stack.push(Val::F64(f(a)));
        Ok(())
    }

    fn unary(&mut self, op: Unop) -> Result<(), Error> {
        use Unop::*;
        match op {
            Not => self.unary_bool(|a| !a),
            NegReal => self.unary_f64(|a| -a),
            AbsReal => self.unary_f64(|a| a.abs()),
            Sqrt => self.unary_f64(|a| a.sqrt()),
        }
    }

    fn logic(&mut self, f: impl Fn(bool, bool) -> bool) -> Result<(), Error> {
        let b = self.pop_bool()?;
        let a = self.pop_bool()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn comp_f64(&mut self, f: impl Fn(f64, f64) -> bool) -> Result<(), Error> {
        let b = self.pop_f64()?;
        let a = self.pop_f64()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn bin_f64(&mut self, f: impl Fn(f64, f64) -> f64) -> Result<(), Error> {
        let b = self.pop_f64()?;
        let a = self.pop_f64()?;
        self.stack.push(Val::F64(f(a, b)));
        Ok(())
    }

    fn binary(&mut self, op: Binop) -> Result<(), Error> {
        use Binop::*;
        match op {
            And => self.logic(|a, b| a && b),
            Or => self.logic(|a, b| a || b),
            EqBool => self.logic(|a, b| a == b),
            NeqBool => self.logic(|a, b| a != b),
            NeqReal => self.comp_f64(|a, b| a != b),
            LtReal => self.comp_f64(|a, b| a < b),
            LeqReal => self.comp_f64(|a, b| a <= b),
            EqReal => self.comp_f64(|a, b| a == b),
            GtReal => self.comp_f64(|a, b| a > b),
            GeqReal => self.comp_f64(|a, b| a >= b),
            AddReal => self.bin_f64(|a, b| a + b),
            SubReal => self.bin_f64(|a, b| a - b),
            MulReal => self.bin_f64(|a, b| a * b),
            DivReal => self.bin_f64(|a, b| a / b),
        }
    }

    fn expr(&mut self, expr: &Expr) -> Val {
        use Expr::*;
        match expr {
            Unit => Val::Unit,
            Bool { val } => Val::Bool(*val),
            F64 { val } => Val::F64(*val),
            Fin { val } => Val::Fin(*val),

            Get { id } => {
                let val = self.locals.get(id.local()).ok_or(Error::BadLocal {
                    num: self.f.def.locals.len(),
                    id,
                })?;
                self.stack
                    .push(val.as_ref().ok_or(Error::UnsetLocal { id })?.clone());
                Ok(())
            }
            Set { id } => {
                let val = self.pop()?;
                let local = self.locals.get_mut(id.local()).ok_or(Error::BadLocal {
                    num: self.f.def.locals.len(),
                    id,
                })?;
                *local = Some(val);
                Ok(())
            }
            Int { val } => {
                self.stack.push(Val::I32(
                    val.try_into().map_err(|_| Error::U32TooBig { val })?,
                ));
                Ok(())
            }
            Real { val } => {
                self.stack.push(Val::F64(val));
                Ok(())
            }
            Vector { id: _ } => todo!(),
            Tuple { id: _ } => todo!(),
            Index => todo!(),
            Member { id: _ } => todo!(),
            Call { id } => {
                let func = self.f.def.funcs.get(id.func()).ok_or(Error::BadFunc {
                    num: self.f.def.funcs.len(),
                    id,
                })?;
                let generics: Result<Vec<usize>, Error> = func
                    .params
                    .iter()
                    .map(|&size| match size {
                        Size::Const { val } => Ok(val),
                        Size::Generic { id } => self.get_generic(id),
                    })
                    .collect();
                let g = func.def.as_ref();
                let args = self
                    .stack
                    .drain(self.stack.len() - g.def.params.len()..) // TODO: don't panic
                    .collect();
                self.stack.append(&mut interp(g, &generics?, args)?);
                Ok(())
            }
            Unary { op } => self.unary(op),
            Binary { op } => self.binary(op),
            If => todo!(),
            Else => todo!(),
            End => todo!(),
            For { limit: _ } => todo!(),
        }
    }

    fn block(&mut self, b: id::Block, arg: Val) -> Val {
        let block = &self.f.blocks()[b.block()];
        self.vars[block.arg.var()] = Some(arg);
        for instr in &block.code {
            self.vars[instr.var.var()] = Some(self.expr(&instr.expr));
        }
        self.vars[block.ret.var()].unwrap()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {}

/// Guaranteed not to panic if `f` is valid.
pub fn interp(f: &Function, generics: &[Typeval], arg: Val) -> Result<Val, Error> {
    // TODO: check that `generics` and `arg` are valid
    let mut interpreter = Interpreter {
        f,
        generics,
        vars: vec![None; f.vars().len()],
    };
    Ok(interpreter.block(f.main(), arg))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_two_plus_two() {
        let f = Function {
            generics: vec![],
            types: vec![Typexpr::Tuple {
                members: vec![Type::F64, Type::F64],
            }],
            funcs: vec![],
            param: Type::Expr { id: id::typexpr(0) },
            ret: Type::F64,
            vars: vec![
                Type::Expr { id: id::typexpr(0) },
                Type::F64,
                Type::F64,
                Type::F64,
            ],
            blocks: vec![Block {
                arg: id::var(0),
                code: vec![
                    Instr {
                        var: id::var(1),
                        expr: Expr::Member {
                            tuple: id::var(0),
                            member: id::member(0),
                        },
                    },
                    Instr {
                        var: id::var(2),
                        expr: Expr::Member {
                            tuple: id::var(0),
                            member: id::member(1),
                        },
                    },
                    Instr {
                        var: id::var(3),
                        expr: Expr::Binary {
                            op: Binop::Add,
                            left: id::var(1),
                            right: id::var(2),
                        },
                    },
                ],
                ret: id::var(3),
            }],
            main: id::block(0),
        };
        let answer = interp(
            &f,
            &[],
            Val::Tuple(Rc::new(vec![Val::F64(2.), Val::F64(2.)])),
        )
        .unwrap();
        assert_eq!(answer, Val::F64(4.));
    }

    #[test]
    fn test_nested_call() {
        let f = Rc::new(Function {
            generics: vec![],
            types: vec![],
            funcs: vec![],
            param: Type::Unit,
            ret: Type::F64,
            vars: vec![Type::Unit, Type::F64],
            blocks: vec![Block {
                arg: id::var(0),
                code: vec![Instr {
                    var: id::var(1),
                    expr: Expr::F64 { val: 42. },
                }],
                ret: id::var(1),
            }],
            main: id::block(0),
        });
        let g = Function {
            generics: vec![],
            types: vec![],
            funcs: vec![Func {
                def: f,
                generics: vec![],
            }],
            param: Type::Unit,
            ret: Type::F64,
            vars: vec![Type::Unit, Type::F64, Type::F64],
            blocks: vec![Block {
                arg: id::var(0),
                code: vec![
                    Instr {
                        var: id::var(1),
                        expr: Expr::Call {
                            func: id::func(0),
                            arg: id::var(0),
                        },
                    },
                    Instr {
                        var: id::var(2),
                        expr: Expr::Binary {
                            op: Binop::Mul,
                            left: id::var(1),
                            right: id::var(1),
                        },
                    },
                ],
                ret: id::var(2),
            }],
            main: id::block(0),
        };
        let answer = interp(&g, &[], Val::Unit).unwrap();
        assert_eq!(answer, Val::F64(1764.));
    }
}
