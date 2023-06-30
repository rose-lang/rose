use rose::{id, Binop, Block, Expr, Func, Function, Instr, Type, Typexpr, Unop};
use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Unit,
    Bool(bool),
    F64(f64),
    Tuple(Rc<Vec<Val>>),
    Vector(Rc<Vec<Val>>),
}

fn type_of(x: &Val) -> Type {
    match x {
        Val::Unit => Type::Unit,
        Val::Bool(_) => Type::Bool,
        Val::F64(_) => Type::F64,
        Val::Tuple(_) => todo!(),
        Val::Vector(_) => todo!(),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {expected} generics, got {actual}")]
    WrongGenerics { expected: usize, actual: usize },

    #[error("expected {expected} args, got {actual}")]
    WrongArgs { expected: usize, actual: usize },

    #[error("empty stack")]
    EmptyStack,

    #[error("have {num} generics, {id:?} out of range")]
    BadGeneric { num: usize, id: id::Generic },

    #[error("have {num} locals, {id:?} out of range")]
    BadLocal { num: usize, id: id::Var },

    #[error("have {num} function references, {id:?} out of range")]
    BadFunc { num: usize, id: id::Func },

    #[error("local {id:?} is unset")]
    UnsetLocal { id: id::Var },

    #[error("expected primitive type {expected:?}, got {actual:?}")]
    ExpectedPrimitive { expected: Type, actual: Type },

    #[error("expected vector, got type {actual:?}")]
    ExpectedVector { actual: Type },

    #[error("u32 {val} too big for i32")]
    U32TooBig { val: u32 },

    #[error("usize {val} too big for i32")]
    UsizeTooBig { val: usize },
}

fn type_error(t: Type, x: &Val) -> Error {
    Error::ExpectedPrimitive {
        expected: t,
        actual: type_of(x),
    }
}

struct Interpreter<'a> {
    f: &'a Function,
    generics: &'a [usize],
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

    fn instr(&mut self, instr: &Instr) -> Result<(), Error> {
        use Expr::*;
        match instr.expr {
            Generic { id } => {
                let val = self.get_generic(id)?;
                self.stack.push(Val::I32(
                    val.try_into().map_err(|_| Error::UsizeTooBig { val })?,
                ));
                Ok(())
            }
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
            Bool { val } => {
                self.stack.push(Val::Bool(val));
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
}

// TODO: return a stack trace with instruction indices, instead of just the error kind
pub fn interp(f: &Function, generics: &[usize], args: Vec<Val>) -> Result<Vec<Val>, Error> {
    if generics.len() != f.generics {
        return Err(Error::WrongGenerics {
            expected: f.generics,
            actual: generics.len(),
        });
    }
    if args.len() != f.def.params.len() {
        return Err(Error::WrongArgs {
            expected: f.def.params.len(),
            actual: args.len(),
        });
    }
    let mut interpreter = Interpreter {
        f,
        generics,
        locals: vec![None; f.def.locals.len()],
        stack: args,
    };
    for &instr in &f.def.body {
        interpreter.instr(instr)?;
    }
    Ok(interpreter.stack)
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
        let answer = interp(&f, &[], vec![Val::F64(2.), Val::F64(2.)]).unwrap();
        assert_eq!(answer, vec![Val::F64(4.)]);
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
        let answer = interp(&g, &[], vec![]).unwrap();
        assert_eq!(answer, vec![Val::F64(1764.)]);
    }
}
