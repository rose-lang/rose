use rose::{Binop, Def, Func, Function, Generic, Instr, Local, Size, Type, Unop};
use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(test)]
use ts_rs::TS;

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Bool(bool),
    I32(i32),
    F64(f64),
    Tuple(Rc<Vec<Val>>),
    Vector(Rc<Vec<Val>>),
}

fn type_of(x: &Val) -> Type {
    match x {
        Val::Bool(_) => Type::Bool,
        Val::I32(_) => Type::Int,
        Val::F64(_) => Type::Real,
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
    BadGeneric { num: usize, id: Generic },

    #[error("have {num} locals, {id:?} out of range")]
    BadLocal { num: usize, id: Local },

    #[error("have {num} function references, {id:?} out of range")]
    BadFunc { num: usize, id: Func },

    #[error("local {id:?} is unset")]
    UnsetLocal { id: Local },

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
    f: &'a Def<Function>,
    generics: &'a [usize],
    locals: Vec<Option<Val>>,
    stack: Vec<Val>,
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

    fn pop_i32(&mut self) -> Result<i32, Error> {
        match self.pop()? {
            Val::I32(x) => Ok(x),
            x => Err(type_error(Type::Int, &x)),
        }
    }

    fn pop_f64(&mut self) -> Result<f64, Error> {
        match self.pop()? {
            Val::F64(x) => Ok(x),
            x => Err(type_error(Type::Int, &x)),
        }
    }

    fn unary_bool(&mut self, f: impl Fn(bool) -> bool) -> Result<(), Error> {
        let a = self.pop_bool()?;
        self.stack.push(Val::Bool(f(a)));
        Ok(())
    }

    fn unary_i32(&mut self, f: impl Fn(i32) -> i32) -> Result<(), Error> {
        let a = self.pop_i32()?;
        self.stack.push(Val::I32(f(a)));
        Ok(())
    }

    fn unary_f64(&mut self, f: impl Fn(f64) -> f64) -> Result<(), Error> {
        let a = self.pop_f64()?;
        self.stack.push(Val::F64(f(a)));
        Ok(())
    }

    fn fold_i32(&mut self, init: i32, f: impl Fn(i32, i32) -> i32) -> Result<(), Error> {
        match self.pop()? {
            Val::Vector(v) => {
                let mut acc = init;
                for val in v.as_ref() {
                    match val {
                        Val::I32(x) => acc = f(acc, *x),
                        x => return Err(type_error(Type::Int, x)),
                    }
                }
                self.stack.push(Val::I32(acc));
                Ok(())
            }
            x => Err(Error::ExpectedVector {
                actual: type_of(&x),
            }),
        }
    }

    fn fold_f64(&mut self, init: f64, f: impl Fn(f64, f64) -> f64) -> Result<(), Error> {
        match self.pop()? {
            Val::Vector(v) => {
                let mut acc = init;
                for val in v.as_ref() {
                    match val {
                        Val::F64(x) => acc = f(acc, *x),
                        x => return Err(type_error(Type::Real, x)),
                    }
                }
                self.stack.push(Val::F64(acc));
                Ok(())
            }
            x => Err(Error::ExpectedVector {
                actual: type_of(&x),
            }),
        }
    }

    fn unary(&mut self, op: Unop) -> Result<(), Error> {
        use Unop::*;
        match op {
            Not => self.unary_bool(|a| !a),
            NegInt => self.unary_i32(|a| -a),
            AbsInt => self.unary_i32(|a| a.abs()),
            NegReal => self.unary_f64(|a| -a),
            AbsReal => self.unary_f64(|a| a.abs()),
            Sqrt => self.unary_f64(|a| a.sqrt()),
            SumInt => self.fold_i32(0, |a, b| a + b),
            ProdInt => self.fold_i32(1, |a, b| a * b),
            MaxInt => self.fold_i32(i32::MIN, |a, b| a.max(b)),
            MinInt => self.fold_i32(i32::MAX, |a, b| a.min(b)),
            SumReal => self.fold_f64(0., |a, b| a + b),
            ProdReal => self.fold_f64(1., |a, b| a * b),
            MaxReal => self.fold_f64(f64::NEG_INFINITY, |a, b| a.max(b)),
            MinReal => self.fold_f64(f64::INFINITY, |a, b| a.min(b)),
        }
    }

    fn logic(&mut self, f: impl Fn(bool, bool) -> bool) -> Result<(), Error> {
        let b = self.pop_bool()?;
        let a = self.pop_bool()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn comp_i32(&mut self, f: impl Fn(i32, i32) -> bool) -> Result<(), Error> {
        let b = self.pop_i32()?;
        let a = self.pop_i32()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn comp_f64(&mut self, f: impl Fn(f64, f64) -> bool) -> Result<(), Error> {
        let b = self.pop_f64()?;
        let a = self.pop_f64()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn bin_i32(&mut self, f: impl Fn(i32, i32) -> i32) -> Result<(), Error> {
        let b = self.pop_i32()?;
        let a = self.pop_i32()?;
        self.stack.push(Val::I32(f(a, b)));
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
            NeqInt => self.comp_i32(|a, b| a != b),
            LtInt => self.comp_i32(|a, b| a < b),
            LeqInt => self.comp_i32(|a, b| a <= b),
            EqInt => self.comp_i32(|a, b| a == b),
            GtInt => self.comp_i32(|a, b| a > b),
            GeqInt => self.comp_i32(|a, b| a >= b),
            NeqReal => self.comp_f64(|a, b| a != b),
            LtReal => self.comp_f64(|a, b| a < b),
            LeqReal => self.comp_f64(|a, b| a <= b),
            EqReal => self.comp_f64(|a, b| a == b),
            GtReal => self.comp_f64(|a, b| a > b),
            GeqReal => self.comp_f64(|a, b| a >= b),
            AddInt => self.bin_i32(|a, b| a + b), // TODO: handle overflow
            SubInt => self.bin_i32(|a, b| a - b),
            MulInt => self.bin_i32(|a, b| a * b),
            DivInt => self.bin_i32(|a, b| a / b),
            Mod => self.bin_i32(|a, b| a % b),
            AddReal => self.bin_f64(|a, b| a + b),
            SubReal => self.bin_f64(|a, b| a - b),
            MulReal => self.bin_f64(|a, b| a * b),
            DivReal => self.bin_f64(|a, b| a / b),
        }
    }

    fn get_generic(&self, id: Generic) -> Result<usize, Error> {
        self.generics.get(id.0).copied().ok_or(Error::BadGeneric {
            num: self.f.generics,
            id,
        })
    }

    fn instr(&mut self, instr: Instr) -> Result<(), Error> {
        use Instr::*;
        match instr {
            Generic { id } => {
                let val = self.get_generic(id)?;
                self.stack.push(Val::I32(
                    val.try_into().map_err(|_| Error::UsizeTooBig { val })?,
                ));
                Ok(())
            }
            Get { id } => {
                let val = self.locals.get(id.0).ok_or(Error::BadLocal {
                    num: self.f.def.locals.len(),
                    id,
                })?;
                self.stack
                    .push(val.as_ref().ok_or(Error::UnsetLocal { id })?.clone());
                Ok(())
            }
            Set { id } => {
                let val = self.pop()?;
                let local = self.locals.get_mut(id.0).ok_or(Error::BadLocal {
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
                let func = self.f.def.funcs.get(id.0).ok_or(Error::BadFunc {
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
pub fn interp(f: &Def<Function>, generics: &[usize], args: Vec<Val>) -> Result<Vec<Val>, Error> {
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
    use rose::{Inst, Typexpr, Var};

    #[test]
    fn test_two_plus_two() {
        let f = Def {
            generics: 0,
            types: vec![],
            def: Function {
                params: vec![Type::Real, Type::Real],
                ret: vec![Type::Real],
                locals: vec![],
                funcs: vec![],
                body: vec![Instr::Binary { op: Binop::AddReal }],
            },
        };
        let answer = interp(&f, &[], vec![Val::F64(2.), Val::F64(2.)]).unwrap();
        assert_eq!(answer, vec![Val::F64(4.)]);
    }

    #[test]
    fn test_nested_call() {
        let f = Rc::new(Def {
            generics: 0,
            types: vec![],
            def: Function {
                params: vec![],
                ret: vec![Type::Real],
                locals: vec![],
                funcs: vec![],
                body: vec![Instr::Real { val: 42. }],
            },
        });
        let g = Def {
            generics: 0,
            types: vec![],
            def: Function {
                params: vec![],
                ret: vec![Type::Real],
                locals: vec![Type::Real],
                funcs: vec![Inst {
                    def: f,
                    params: vec![],
                }],
                body: vec![
                    Instr::Call { id: Func(0) },
                    Instr::Set { id: Local(0) },
                    Instr::Get { id: Local(0) },
                    Instr::Get { id: Local(0) },
                    Instr::Binary { op: Binop::MulReal },
                ],
            },
        };
        let answer = interp(&g, &[], vec![]).unwrap();
        assert_eq!(answer, vec![Val::F64(1764.)]);
    }

    #[test]
    fn test_sum_int() {
        let f = Def {
            generics: 1,
            types: vec![Typexpr::Vector {
                elem: Type::Int,
                size: Size::Generic { id: Generic(0) },
            }],
            def: Function {
                params: vec![Type::Var { id: Var(0) }],
                ret: vec![Type::Int],
                locals: vec![],
                funcs: vec![],
                body: vec![Instr::Unary { op: Unop::SumInt }],
            },
        };
        let answer = interp(
            &f,
            &[3],
            vec![Val::Vector(Rc::new(vec![
                Val::I32(1),
                Val::I32(2),
                Val::I32(3),
            ]))],
        )
        .unwrap();
        assert_eq!(answer, vec![Val::I32(6)]);
    }

    #[test]
    fn test_prod_real() {
        let f = Def {
            generics: 1,
            types: vec![Typexpr::Vector {
                elem: Type::Real,
                size: Size::Generic { id: Generic(0) },
            }],
            def: Function {
                params: vec![Type::Var { id: Var(0) }],
                ret: vec![Type::Real],
                locals: vec![],
                funcs: vec![],
                body: vec![Instr::Unary { op: Unop::ProdReal }],
            },
        };
        let answer = interp(
            &f,
            &[3],
            vec![Val::Vector(Rc::new(vec![
                Val::F64(2.),
                Val::F64(3.),
                Val::F64(4.),
            ]))],
        )
        .unwrap();
        assert_eq!(answer, vec![Val::F64(24.)]);
    }
}
