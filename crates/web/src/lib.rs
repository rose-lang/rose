use enumset::EnumSet;
use indexmap::IndexSet;
use rose::id;
use serde::Serialize;
use std::rc::Rc;
use wasm_bindgen::prelude::{wasm_bindgen, JsError, JsValue};

#[cfg(feature = "debug")]
#[wasm_bindgen]
pub fn initialize() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

fn to_js_value(value: &impl Serialize) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}

// for regression testing purposes only
#[cfg(feature = "debug")]
#[wasm_bindgen]
pub fn layouts() -> Result<JsValue, serde_wasm_bindgen::Error> {
    #[derive(Serialize)]
    struct Layout {
        size: usize,
        align: usize,
    }

    fn layout<T>() -> Layout {
        Layout {
            size: std::mem::size_of::<T>(),
            align: std::mem::align_of::<T>(),
        }
    }

    to_js_value(&[
        ("Expr", layout::<rose::Expr>()),
        ("Function", layout::<rose::Function>()),
        ("Instr", layout::<rose::Instr>()),
        ("Ty", layout::<rose::Ty>()),
    ])
}

/// A node in a reference-counted acyclic digraph of functions.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct Func {
    rc: Rc<(Vec<Func>, rose::Function)>,
}

impl<'a> rose::FuncNode for &'a Func {
    fn def(&self) -> &rose::Function {
        let (_, def) = self.rc.as_ref();
        def
    }

    fn get(&self, id: id::Function) -> Option<Self> {
        let (funcs, _) = self.rc.as_ref();
        funcs.get(id.function())
    }
}

#[cfg(feature = "debug")]
#[wasm_bindgen]
pub fn pprint(f: &Func) -> Result<String, JsError> {
    use std::fmt::Write as _; // see https://doc.rust-lang.org/std/macro.write.html

    fn print_instr(
        mut s: &mut String,
        def: &rose::Function,
        spaces: usize,
        instr: &rose::Instr,
    ) -> Result<(), JsError> {
        for _ in 0..spaces {
            write!(&mut s, " ")?;
        }
        let x = instr.var.var();
        write!(&mut s, "x{}: T{} = ", x, def.vars[x].ty())?;
        match &instr.expr {
            rose::Expr::Unit => writeln!(&mut s, "unit")?,
            rose::Expr::Bool { val } => writeln!(&mut s, "{val}")?,
            rose::Expr::F64 { val } => writeln!(&mut s, "{val}")?,
            rose::Expr::Fin { val } => writeln!(&mut s, "{val}")?,
            rose::Expr::Array { elems } => {
                write!(&mut s, "[")?;
                print_elems(s, 'x', elems.iter().map(|elem| elem.var()))?;
                writeln!(&mut s, "]")?;
            }
            rose::Expr::Tuple { members } => {
                write!(&mut s, "(")?;
                print_elems(s, 'x', members.iter().map(|member| member.var()))?;
                writeln!(&mut s, ")")?;
            }
            rose::Expr::Index { array, index } => {
                writeln!(&mut s, "x{}[x{}]", array.var(), index.var())?
            }
            rose::Expr::Member { tuple, member } => {
                writeln!(&mut s, "x{}.{}", tuple.var(), member.member())?
            }
            rose::Expr::Slice { array, index } => {
                writeln!(&mut s, "x{}![x{}]", array.var(), index.var())?
            }
            rose::Expr::Field { tuple, field } => {
                writeln!(&mut s, "x{}!.{}", tuple.var(), field.member())?
            }
            rose::Expr::Unary { op, arg } => match op {
                rose::Unop::Not => writeln!(&mut s, "not x{}", arg.var())?,
                rose::Unop::Neg => writeln!(&mut s, "-x{}", arg.var())?,
                rose::Unop::Abs => writeln!(&mut s, "|x{}|", arg.var())?,
                rose::Unop::Sqrt => writeln!(&mut s, "sqrt(x{})", arg.var())?,
            },
            rose::Expr::Binary { op, left, right } => match op {
                rose::Binop::And => writeln!(&mut s, "x{} and x{}", left.var(), right.var())?,
                rose::Binop::Or => writeln!(&mut s, "x{} or x{}", left.var(), right.var())?,
                rose::Binop::Iff => writeln!(&mut s, "x{} iff x{}", left.var(), right.var())?,
                rose::Binop::Xor => writeln!(&mut s, "x{} xor x{}", left.var(), right.var())?,
                rose::Binop::Neq => writeln!(&mut s, "x{} != x{}", left.var(), right.var())?,
                rose::Binop::Lt => writeln!(&mut s, "x{} < x{}", left.var(), right.var())?,
                rose::Binop::Leq => writeln!(&mut s, "x{} <= x{}", left.var(), right.var())?,
                rose::Binop::Eq => writeln!(&mut s, "x{} == x{}", left.var(), right.var())?,
                rose::Binop::Gt => writeln!(&mut s, "x{} > x{}", left.var(), right.var())?,
                rose::Binop::Geq => writeln!(&mut s, "x{} >= x{}", left.var(), right.var())?,
                rose::Binop::Add => writeln!(&mut s, "x{} + x{}", left.var(), right.var())?,
                rose::Binop::Sub => writeln!(&mut s, "x{} - x{}", left.var(), right.var())?,
                rose::Binop::Mul => writeln!(&mut s, "x{} * x{}", left.var(), right.var())?,
                rose::Binop::Div => writeln!(&mut s, "x{} / x{}", left.var(), right.var())?,
            },
            rose::Expr::Select { cond, then, els } => {
                writeln!(&mut s, "x{} ? x{} : x{}", cond.var(), then.var(), els.var())?
            }
            rose::Expr::Call { id, generics, args } => {
                write!(&mut s, "f{}<", id.function())?;
                print_elems(s, 'T', generics.iter().map(|generic| generic.ty()))?;
                write!(&mut s, ">(")?;
                print_elems(s, 'x', args.iter().map(|arg| arg.var()))?;
                writeln!(&mut s, ")")?;
            }
            rose::Expr::For {
                index,
                arg,
                body,
                ret,
            } => {
                writeln!(&mut s, "for x{}: T{} {{", arg.var(), index.ty())?;
                print_block(s, def, spaces + 2, body, *ret)?;
                for _ in 0..spaces {
                    write!(&mut s, " ")?;
                }
                writeln!(&mut s, "}}")?
            }
            rose::Expr::Read {
                var,
                arg,
                body,
                ret,
            } => {
                writeln!(&mut s, "read x{} {{", var.var())?;
                for _ in 0..spaces {
                    write!(&mut s, " ")?;
                }
                let x = arg.var();
                writeln!(&mut s, "  x{x}: T{}", def.vars[x].ty())?;
                print_block(s, def, spaces + 2, body, *ret)?;
                for _ in 0..spaces {
                    write!(&mut s, " ")?;
                }
                writeln!(&mut s, "}}")?
            }
            rose::Expr::Accum {
                shape,
                arg,
                body,
                ret,
            } => {
                writeln!(&mut s, "accum x{} {{", shape.var())?;
                for _ in 0..spaces {
                    write!(&mut s, " ")?;
                }
                let x = arg.var();
                writeln!(&mut s, "  x{x}: T{}", def.vars[x].ty())?;
                print_block(s, def, spaces + 2, body, *ret)?;
                for _ in 0..spaces {
                    write!(&mut s, " ")?;
                }
                writeln!(&mut s, "}}")?
            }
            rose::Expr::Ask { var } => writeln!(&mut s, "ask x{}", var.var())?,
            rose::Expr::Add { accum, addend } => {
                writeln!(&mut s, "x{} += x{}", accum.var(), addend.var())?
            }
        }
        Ok(())
    }

    fn print_block(
        mut s: &mut String,
        def: &rose::Function,
        spaces: usize,
        body: &[rose::Instr],
        ret: id::Var,
    ) -> Result<(), JsError> {
        for instr in body.iter() {
            print_instr(s, def, spaces, instr)?;
        }
        for _ in 0..spaces {
            write!(&mut s, " ")?;
        }
        writeln!(&mut s, "x{}", ret.var())?;
        Ok(())
    }

    fn print_elems(
        s: &mut String,
        prefix: char,
        items: impl Iterator<Item = usize>,
    ) -> std::fmt::Result {
        let mut first = true;
        for item in items {
            if first {
                first = false;
            } else {
                write!(s, ", ")?;
            }
            write!(s, "{}{}", prefix, item)?;
        }
        Ok(())
    }

    let mut s = String::new();
    let (_, def) = f.rc.as_ref();

    for (i, constraints) in def.generics.iter().enumerate() {
        write!(&mut s, "G{i} = ")?;
        let mut first = true;
        for constraint in constraints.iter() {
            if first {
                first = false;
            } else {
                write!(&mut s, " + ")?;
            }
            write!(&mut s, "{constraint:?}")?;
        }
        writeln!(&mut s)?;
    }
    for (i, ty) in def.types.iter().enumerate() {
        write!(&mut s, "T{i} = ")?;
        match ty {
            rose::Ty::Unit | rose::Ty::Bool | rose::Ty::F64 => writeln!(&mut s, "{ty:?}")?,
            rose::Ty::Fin { size } => writeln!(&mut s, "{size}")?,
            rose::Ty::Generic { id } => writeln!(&mut s, "G{}", id.generic())?,
            rose::Ty::Scope { kind, id } => writeln!(&mut s, "B{}: {kind:?}", id.var())?,
            rose::Ty::Ref { scope, inner } => {
                writeln!(&mut s, "Ref T{} T{}", scope.ty(), inner.ty())?
            }
            rose::Ty::Array { index, elem } => {
                writeln!(&mut s, "[T{}; T{}]", elem.ty(), index.ty())?
            }
            rose::Ty::Tuple { members } => {
                write!(&mut s, "(")?;
                print_elems(&mut s, 'T', members.iter().map(|member| member.ty()))?;
                writeln!(&mut s, ")")?;
            }
        }
    }
    write!(&mut s, "(")?;
    let mut first = true;
    for param in def.params.iter() {
        if first {
            first = false;
        } else {
            write!(&mut s, ", ")?;
        }
        write!(&mut s, "x{}: T{}", param.var(), def.vars[param.var()].ty())?;
    }
    writeln!(&mut s, ") -> T{} {{", def.vars[def.ret.var()].ty())?;
    for instr in def.body.iter() {
        print_instr(&mut s, def, 2, instr)?;
    }
    writeln!(&mut s, "  x{}", def.ret.var())?;
    writeln!(&mut s, "}}")?;

    Ok(s)
}

/// A function under construction.
#[wasm_bindgen]
pub struct Context {
    functions: Vec<Func>,
    generics: Vec<EnumSet<rose::Constraint>>,
    types: IndexSet<rose::Ty>,
    vars: Vec<id::Ty>,
    params: Vec<id::Var>,
}

#[wasm_bindgen]
pub fn bake(ctx: Context, out: usize, main: Block) -> Func {
    let Context {
        functions,
        generics,
        types,
        params,
        vars,
    } = ctx;
    Func {
        rc: Rc::new((
            functions,
            rose::Function {
                generics: generics.into(),
                types: types.into_iter().collect(),
                params: params.into(),
                ret: id::var(out),
                vars: vars.into(),
                body: main.code.into(),
            },
        )),
    }
}

/// A block under construction. Implicitly refers to the current `Context`.
#[wasm_bindgen]
pub struct Block {
    code: Vec<rose::Instr>,
}

#[wasm_bindgen]
impl Block {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { code: vec![] }
    }
}

// just to appease Clippy
impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

// just an ephemeral struct to return several things which we then unpack on the JS side
#[wasm_bindgen]
pub struct Body {
    ctx: Option<Context>,
    main: Option<Block>,
}

#[wasm_bindgen]
impl Body {
    #[wasm_bindgen]
    pub fn ctx(&mut self) -> Result<Context, JsError> {
        self.ctx
            .take()
            .ok_or_else(|| JsError::new("context already taken"))
    }

    #[wasm_bindgen]
    pub fn main(&mut self) -> Result<Block, JsError> {
        self.main
            .take()
            .ok_or_else(|| JsError::new("block already taken"))
    }
}

/// The `types` argument is Serde-converted to `indexmap::IndexSet<rose::Ty>`.
#[wasm_bindgen]
pub fn make(generics: usize, types: JsValue, params: &[usize]) -> Result<Body, JsError> {
    let types: IndexSet<rose::Ty> = serde_wasm_bindgen::from_value(types)?;
    let ctx = Context {
        functions: vec![],
        generics: vec![EnumSet::only(rose::Constraint::Index); generics],
        types,
        vars: params.iter().map(|&id| id::ty(id)).collect(),
        params: (0..params.len()).map(id::var).collect(),
    };
    Ok(Body {
        ctx: Some(ctx),
        main: Some(Block { code: vec![] }),
    })
}

/// Resolve `ty` via `generics` and `types`, then return its ID in `typemap`, inserting if need be.
///
/// This is meant to be used to pull all the types from a callee into a broader context. The
/// `generics` are the IDs of all the types provided as generic type parameters for the callee. The
/// `types are the IDs of all the types that have been pulled in so far.
///
/// The element type of `types`, and the return type of this function, use `Option` because some
/// types from the callee may be ignored, specifically those that reference internal scopes. These
/// cannot appear in the callee's signature if it is a valid function, so we can simply turn them
/// into `None` and propagate those through if we find further types that depend on them.
fn resolve(
    typemap: &mut IndexSet<rose::Ty>,
    generics: &[usize],
    types: &[Option<id::Ty>],
    ty: &rose::Ty,
) -> Option<id::Ty> {
    let resolved = match ty {
        // inner scopes cannot appear in the return type, which is all we care about here
        rose::Ty::Scope { kind: _, id: _ } => return None,
        rose::Ty::Generic { id } => return Some(id::ty(generics[id.generic()])),

        rose::Ty::Unit => rose::Ty::Unit,
        rose::Ty::Bool => rose::Ty::Bool,
        rose::Ty::F64 => rose::Ty::F64,
        &rose::Ty::Fin { size } => rose::Ty::Fin { size },

        rose::Ty::Ref { scope, inner } => rose::Ty::Ref {
            scope: types[scope.ty()]?,
            inner: types[inner.ty()]?,
        },
        rose::Ty::Array { index, elem } => rose::Ty::Array {
            index: types[index.ty()]?,
            elem: types[elem.ty()]?,
        },
        rose::Ty::Tuple { members } => rose::Ty::Tuple {
            members: members
                .iter()
                .map(|&x| types[x.ty()])
                .collect::<Option<_>>()?,
        },
    };
    let (i, _) = typemap.insert_full(resolved);
    Some(id::ty(i))
}

// TODO: catch invalid user-given indices instead of panicking
#[wasm_bindgen]
impl Context {
    fn get(&self, var: id::Var) -> id::Ty {
        self.vars[var.var()]
    }

    fn var(&mut self, t: id::Ty) -> id::Var {
        let id = self.vars.len();
        self.vars.push(t);
        id::var(id)
    }

    fn ty(&mut self, ty: rose::Ty) -> id::Ty {
        let (i, _) = self.types.insert_full(ty);
        id::ty(i)
    }

    // for `If`
    #[wasm_bindgen(js_name = "varUnit")]
    pub fn var_unit(&mut self) -> usize {
        let ty = self.ty(rose::Ty::Unit);
        self.var(ty).var()
    }

    fn instr(&mut self, b: &mut Block, t: id::Ty, expr: rose::Expr) -> usize {
        let var = self.var(t);
        b.code.push(rose::Instr { var, expr });
        var.var()
    }

    #[wasm_bindgen]
    pub fn unit(&mut self, b: &mut Block) -> usize {
        let ty = self.ty(rose::Ty::Unit);
        self.instr(b, ty, rose::Expr::Unit)
    }

    #[wasm_bindgen]
    pub fn bool(&mut self, b: &mut Block, val: bool) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        self.instr(b, ty, rose::Expr::Bool { val })
    }

    #[wasm_bindgen]
    pub fn f64(&mut self, b: &mut Block, val: f64) -> usize {
        let ty = self.ty(rose::Ty::F64);
        self.instr(b, ty, rose::Expr::F64 { val })
    }

    #[wasm_bindgen]
    pub fn fin(&mut self, b: &mut Block, size: usize, val: usize) -> usize {
        let ty = self.ty(rose::Ty::Fin { size });
        self.instr(b, ty, rose::Expr::Fin { val })
    }

    #[wasm_bindgen]
    pub fn array(&mut self, b: &mut Block, elems: &[usize]) -> Result<usize, JsError> {
        let xs: Vec<id::Var> = elems.iter().map(|&x| id::var(x)).collect();
        let &x = xs.get(0).ok_or_else(|| JsError::new("empty array"))?;
        let index = self.ty(rose::Ty::Fin { size: xs.len() });
        let ty = self.ty(rose::Ty::Array {
            index,
            elem: self.get(x),
        });
        let expr = rose::Expr::Array { elems: xs.into() };
        Ok(self.instr(b, ty, expr))
    }

    #[wasm_bindgen]
    pub fn tuple(&mut self, b: &mut Block, members: &[usize]) -> usize {
        let xs: Vec<id::Var> = members.iter().map(|&x| id::var(x)).collect();
        let types = xs.iter().map(|&x| self.get(x)).collect();
        let ty = self.ty(rose::Ty::Tuple { members: types });
        let expr = rose::Expr::Tuple { members: xs.into() };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn index(&mut self, b: &mut Block, arr: usize, idx: usize) -> Result<usize, JsError> {
        let array = id::var(arr);
        let index = id::var(idx);
        let ty = match self.types[self.get(array).ty()] {
            rose::Ty::Array { index: _, elem } => elem,
            _ => return Err(JsError::new("not an array")),
        };
        Ok(self.instr(b, ty, rose::Expr::Index { array, index }))
    }

    #[wasm_bindgen]
    pub fn member(&mut self, b: &mut Block, tup: usize, mem: usize) -> Result<usize, JsError> {
        let tuple = id::var(tup);
        let member = id::member(mem);
        let ty = match &self.types[self.get(tuple).ty()] {
            rose::Ty::Tuple { members } => members[mem],
            _ => return Err(JsError::new("not a tuple")),
        };
        Ok(self.instr(b, ty, rose::Expr::Member { tuple, member }))
    }

    // no `Expr::Slice` or `Expr::Field` here, because we don't currently expose mutation to JS

    // unary

    #[wasm_bindgen]
    pub fn not(&mut self, b: &mut Block, arg: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Unary {
            op: rose::Unop::Not,
            arg: id::var(arg),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn neg(&mut self, b: &mut Block, arg: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Unary {
            op: rose::Unop::Neg,
            arg: id::var(arg),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn abs(&mut self, b: &mut Block, arg: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Unary {
            op: rose::Unop::Abs,
            arg: id::var(arg),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn sqrt(&mut self, b: &mut Block, arg: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Unary {
            op: rose::Unop::Sqrt,
            arg: id::var(arg),
        };
        self.instr(b, ty, expr)
    }

    // end of unary

    // binary

    #[wasm_bindgen]
    pub fn and(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::And,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn or(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Or,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn iff(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Iff,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn xor(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Xor,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn neq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Neq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn lt(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Lt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn leq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Leq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn eq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Eq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn gt(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Gt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn geq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::Bool);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Geq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn add(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Add,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn sub(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Sub,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn mul(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Mul,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    #[wasm_bindgen]
    pub fn div(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let ty = self.ty(rose::Ty::F64);
        let expr = rose::Expr::Binary {
            op: rose::Binop::Div,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, ty, expr)
    }

    // end of binary

    #[wasm_bindgen]
    pub fn call(
        &mut self,
        b: &mut Block,
        f: &Func,
        generics: &[usize],
        args: &[usize],
    ) -> Result<usize, JsError> {
        let mut types = vec![];
        let (_, def) = f.rc.as_ref();
        // push a corresponding type onto our own `types` for each type in the callee
        for callee_type in def.types.iter() {
            types.push(resolve(&mut self.types, generics, &types, callee_type));
        }

        // add the function reference to the callee
        let id = id::function(self.functions.len());
        self.functions.push(f.clone());

        let ty = types[def.vars[def.ret.var()].ty()].unwrap();
        let expr = rose::Expr::Call {
            id,
            generics: generics.iter().map(|&i| id::ty(i)).collect(),
            args: args.iter().map(|&x| id::var(x)).collect(),
        };
        Ok(self.instr(b, ty, expr))
    }

    #[wasm_bindgen]
    pub fn select(&mut self, b: &mut Block, cond: usize, then: usize, els: usize) -> usize {
        let then = id::var(then);
        let els = id::var(els);
        let t = self.get(then); // arbitrary; could have used `els` instead
        let expr = rose::Expr::Select {
            cond: id::var(cond),
            then,
            els,
        };
        self.instr(b, t, expr)
    }

    // `rose::Expr::For`
    #[wasm_bindgen]
    pub fn arr(
        &mut self,
        b: &mut Block,
        index: usize,
        arg: usize,
        body: Block,
        out: usize,
    ) -> usize {
        let arg = id::var(arg);
        let ret = id::var(out);
        let ty = self.ty(rose::Ty::Array {
            index: self.get(arg),
            elem: self.get(ret),
        });
        let expr = rose::Expr::For {
            index: id::ty(index),
            arg,
            body: body.code.into(),
            ret,
        };
        self.instr(b, ty, expr)
    }
}

/// Interpret a function with the given arguments.
///
/// The `types` are Serde-converted to `indexmap::IndexSet<rose::Ty>`, the `args` are
/// Serde-converted to `Vec<rose_interp::Val>`, and the return value is Serde-converted from
/// `rose_interp::Val`.
#[wasm_bindgen]
pub fn interp(
    f: &Func,
    types: JsValue,
    generics: &[usize],
    args: JsValue,
) -> Result<JsValue, JsError> {
    let types: IndexSet<rose::Ty> = serde_wasm_bindgen::from_value(types)?;
    let args: Vec<rose_interp::Val> = serde_wasm_bindgen::from_value(args)?;
    let generics: Vec<id::Ty> = generics.iter().map(|&i| id::ty(i)).collect();
    let ret = rose_interp::interp(f, types, &generics, args.into_iter())?;
    Ok(to_js_value(&ret)?)
}
