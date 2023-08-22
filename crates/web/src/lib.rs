use enumset::EnumSet;
use indexmap::{IndexMap, IndexSet};
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
    rc: Rc<(Box<[Func]>, rose::Function)>,
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
            rose::Expr::Field { tuple, member } => {
                writeln!(&mut s, "x{}!.{}", tuple.var(), member.member())?
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
            rose::Expr::For { arg, body, ret } => {
                writeln!(
                    &mut s,
                    "for x{}: T{} {{",
                    arg.var(),
                    def.vars[arg.var()].ty()
                )?;
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

/// Metadata about a variable while its containing function is still under construction.
enum Extra {
    /// Does not depend on any of the function's parameters.
    Constant,

    /// Part of the main function body; these are definitions for variables that depend on it.
    Parent(Vec<rose::Instr>),

    /// Depends on another variable; others can depend on it only indirectly through its parent.
    Child(id::Var),

    /// Is no longer in scope.
    Expired,
}

struct Var {
    ty: id::Ty,
    extra: Extra,
}

/// A function under construction.
#[wasm_bindgen]
pub struct FuncBuilder {
    functions: Vec<Func>,
    generics: Box<[EnumSet<rose::Constraint>]>,
    types: IndexMap<rose::Ty, EnumSet<rose::Constraint>>,
    vars: Vec<Var>,
    params: Vec<id::Var>,
    constants: Vec<rose::Instr>,
}

#[wasm_bindgen]
impl FuncBuilder {
    /// Start building a function with the given number of `generics`, all constrained as `Index`.
    #[wasm_bindgen(constructor)]
    pub fn new(generics: usize) -> Self {
        Self {
            functions: vec![],
            generics: vec![EnumSet::only(rose::Constraint::Index); generics].into(),
            types: IndexMap::new(),
            vars: vec![],
            params: vec![],
            constants: vec![],
        }
    }

    /// Assemble this function with return variable `out` and the given `body`.
    #[wasm_bindgen]
    pub fn finish(mut self, out: usize, body: Block) -> Func {
        // We replace `self.constants` with an empty vec because we need to satisfy the borrow
        // checker when we pass `self` to `body.finish` below; this is OK though, because
        // `Block::finish` is guaranteed not to use `self.constants`.
        let mut code = std::mem::take(&mut self.constants);
        body.finish(&mut self, &mut code);
        Func {
            rc: Rc::new((
                self.functions.into(),
                rose::Function {
                    generics: self.generics,
                    types: self.types.into_keys().collect(),
                    vars: self.vars.into_iter().map(|x| x.ty).collect(),
                    params: self.params.into(),
                    ret: id::var(out),
                    body: code.into(),
                },
            )),
        }
    }

    /// Finalize `x`, appending its dependencies onto `code` and marking it and them as expired.
    ///
    /// Must not use `self.constants`.
    fn extra(&mut self, x: id::Var, code: &mut Vec<rose::Instr>) {
        match std::mem::replace(&mut self.vars[x.var()].extra, Extra::Expired) {
            Extra::Parent(extra) => {
                for instr in extra.iter() {
                    self.vars[instr.var.var()].extra = Extra::Expired;
                }
                code.extend(extra);
            }
            Extra::Constant | Extra::Child(_) | Extra::Expired => unreachable!(),
        }
    }

    /// Should the type with ID `t` be represented as a JavaScript `Symbol`?
    ///
    /// Values of index types must be symbols so that we can use the standard JavaScript indexing
    /// notation (along with `Proxy`, see below) to generate array accessing code.
    #[wasm_bindgen(js_name = "isSymbol")]
    pub fn is_symbol(&self, t: usize) -> bool {
        let (ty, _) = self.types.get_index(t).unwrap();
        // Because the JS API only allows constructing generics that are index types, we don't need
        // to check for `rose::Constraint::Index` here. If that changes, this code must change too.
        matches!(ty, rose::Ty::Fin { .. } | rose::Ty::Generic { .. })
    }

    /// Should the type with ID `t` be represented as a JavaScript `Proxy`?
    ///
    /// Values of array types must be proxies so that we can use the standard JavaScript indexing
    /// notation (along with `Symbol`, see above) to generate array accessing code.
    #[wasm_bindgen(js_name = "isProxy")]
    pub fn is_proxy(&self, t: usize) -> bool {
        let (ty, _) = self.types.get_index(t).unwrap();
        matches!(ty, rose::Ty::Array { .. }) // should check for `Tuple` too once we allow structs
    }

    /// Return a reference to the type with ID `t` if it exists, `Err` otherwise.
    ///
    /// This returns a `Result` with `JsError` because it is a helper method meant to be used by
    /// `pub` methods exposed to JavaScript; don't prefer it for more Rusty stuff, because `JsError`
    /// doesn't implement `Debug` so you can't easily call `Result::unwrap` here.
    fn ty(&self, t: usize) -> Result<&rose::Ty, JsError> {
        match self.types.get_index(t) {
            None => Err(JsError::new("type does not exist")),
            Some((ty, _)) => Ok(ty),
        }
    }

    /// Return the ID of the index type for the array type with ID `t`.
    ///
    /// `Err` if `t` is out of range or does not represent an array type.
    #[wasm_bindgen]
    pub fn index(&self, t: usize) -> Result<usize, JsError> {
        match self.ty(t)? {
            &rose::Ty::Array { index, elem: _ } => Ok(index.ty()),
            _ => Err(JsError::new("type is not an array")),
        }
    }

    /// Return the ID of the element type for the array type with ID `t`.
    ///
    /// `Err` if `t` is out of range or does not represent an array type.
    #[wasm_bindgen]
    pub fn elem(&self, t: usize) -> Result<usize, JsError> {
        match self.ty(t)? {
            &rose::Ty::Array { index: _, elem } => Ok(elem.ty()),
            _ => Err(JsError::new("type is not an array")),
        }
    }

    /// Return `x` if it exists, is in scope, and has type ID `t`; `Err` otherwise.
    #[wasm_bindgen]
    pub fn expect(&self, t: usize, x: usize) -> Result<usize, JsError> {
        match self.vars.get(x) {
            None => Err(JsError::new("variable does not exist")),
            Some(var) => match var.extra {
                Extra::Expired => Err(JsError::new("variable is out of scope")),
                _ => {
                    if var.ty == id::ty(t) {
                        Ok(x)
                    } else {
                        Err(JsError::new("variable type mismatch"))
                    }
                }
            },
        }
    }

    /// Return the type ID for `ty`, creating if needed, and marking its constraints as `constrs`.
    fn newtype(&mut self, ty: rose::Ty, constrs: EnumSet<rose::Constraint>) -> usize {
        let (i, _) = self.types.insert_full(ty, constrs);
        i
    }

    /// Create a new non-constant, non-child variable with type ID `t`, and return its ID.
    ///
    /// This method should only be used for variables that are about to be directly defined as part
    /// of a `Block`, not for constants or any literals that get attached to other variables.
    fn newvar(&mut self, t: id::Ty) -> id::Var {
        let id = self.vars.len();
        self.vars.push(Var {
            ty: t,
            extra: Extra::Parent(vec![]),
        });
        id::var(id)
    }

    /// Return the ID for the unit type, creating if needed.
    #[wasm_bindgen(js_name = "tyUnit")]
    pub fn ty_unit(&mut self) -> usize {
        self.newtype(rose::Ty::Unit, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the boolean type, creating if needed.
    #[wasm_bindgen(js_name = "tyBool")]
    pub fn ty_bool(&mut self) -> usize {
        self.newtype(rose::Ty::Bool, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the 64-bit floating-point type, creating if needed.
    #[wasm_bindgen(js_name = "tyF64")]
    pub fn ty_f64(&mut self) -> usize {
        self.newtype(rose::Ty::F64, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the type of nonnegative integers less than `size`, creating if needed.
    #[wasm_bindgen(js_name = "tyFin")]
    pub fn ty_fin(&mut self, size: usize) -> usize {
        self.newtype(
            rose::Ty::Fin { size },
            rose::Constraint::Value | rose::Constraint::Index,
        )
    }

    #[wasm_bindgen(js_name = "tyGeneric")]
    pub fn ty_generic(&mut self, id: usize) -> usize {
        self.newtype(
            rose::Ty::Generic {
                id: id::generic(id),
            },
            self.generics[id],
        )
    }

    #[wasm_bindgen(js_name = "tyArray")]
    pub fn ty_array(&mut self, index: usize, elem: usize) -> usize {
        self.newtype(
            rose::Ty::Array {
                index: id::ty(index),
                elem: id::ty(elem),
            },
            EnumSet::only(rose::Constraint::Value),
        )
    }

    #[wasm_bindgen]
    pub fn bind(&mut self, t: usize) -> usize {
        self.newvar(id::ty(t)).var()
    }

    #[wasm_bindgen]
    pub fn param(&mut self, t: usize) -> usize {
        let x = self.newvar(id::ty(t));
        self.params.push(x);
        x.var()
    }

    fn constant(&mut self, t: usize, expr: rose::Expr) -> usize {
        let x = self.vars.len();
        self.vars.push(Var {
            ty: id::ty(t),
            extra: Extra::Constant,
        });
        self.constants.push(rose::Instr {
            var: id::var(x),
            expr,
        });
        x
    }

    #[wasm_bindgen]
    pub fn unit(&mut self, t: usize) -> Result<usize, JsError> {
        if t == self.ty_unit() {
            Ok(self.constant(t, rose::Expr::Unit))
        } else {
            Err(JsError::new("did not expect null"))
        }
    }

    #[wasm_bindgen]
    pub fn bool(&mut self, t: usize, val: bool) -> Result<usize, JsError> {
        if t == self.ty_bool() {
            Ok(self.constant(t, rose::Expr::Bool { val }))
        } else {
            Err(JsError::new("did not expect boolean"))
        }
    }

    #[wasm_bindgen]
    pub fn num(&mut self, t: usize, x: f64) -> Result<usize, JsError> {
        match self.ty(t)? {
            rose::Ty::F64 => Ok(self.constant(t, rose::Expr::F64 { val: x })),
            &rose::Ty::Fin { size } => {
                let y = x as usize;
                if y as f64 != x {
                    Err(JsError::new("can't be represented by an unsigned integer"))
                } else if y >= size {
                    Err(JsError::new("out of range"))
                } else {
                    Ok(self.constant(t, rose::Expr::Fin { val: y }))
                }
            }
            _ => Err(JsError::new("type is not numeric")),
        }
    }

    #[wasm_bindgen]
    pub fn array(&mut self, t: usize, xs: &[usize]) -> usize {
        let elems = xs.iter().map(|&x| id::var(x)).collect();
        let expr = rose::Expr::Array { elems };
        match xs
            .iter()
            .filter_map(|&x| match self.vars[x].extra {
                Extra::Constant => None,
                Extra::Parent(_) => Some(x),
                Extra::Child(y) => Some(y.var()),
                Extra::Expired => unreachable!(),
            })
            .max()
        {
            None => self.constant(t, expr),
            Some(x) => {
                let y = self.vars.len();
                self.vars.push(Var {
                    ty: id::ty(t),
                    extra: Extra::Child(id::var(x)),
                });
                match &mut self.vars[x].extra {
                    Extra::Parent(instrs) => instrs.push(rose::Instr {
                        var: id::var(y),
                        expr,
                    }),
                    _ => unreachable!(),
                }
                y
            }
        }
    }

    /// Resolve `ty` via `generics` and `types`, then return its ID in `typemap`, inserting if need
    /// be.
    ///
    /// This is meant to be used to pull all the types from a callee into a broader context. The
    /// `generics` are the IDs of all the types provided as generic type parameters for the callee.
    /// The `types are the IDs of all the types that have been pulled in so far.
    ///
    /// The element type of `types`, and the return type of this function, use `Option` because some
    /// types from the callee may be ignored, specifically those that reference internal scopes.
    /// These cannot appear in the callee's signature if it is a valid function, so we can simply
    /// turn them into `None` and propagate those through if we find further types that depend on
    /// them.
    fn resolve(
        &mut self,
        generics: &[usize],
        types: &[Option<id::Ty>],
        ty: &rose::Ty,
    ) -> Option<id::Ty> {
        let (deduped, constrs) = match ty {
            // inner scopes can't be in the param or return types, which are all we care about here
            rose::Ty::Scope { kind: _, id: _ } => return None,
            rose::Ty::Generic { id } => return Some(id::ty(generics[id.generic()])),

            rose::Ty::Unit => (rose::Ty::Unit, EnumSet::only(rose::Constraint::Value)),
            rose::Ty::Bool => (rose::Ty::Bool, EnumSet::only(rose::Constraint::Value)),
            rose::Ty::F64 => (rose::Ty::F64, EnumSet::only(rose::Constraint::Value)),
            &rose::Ty::Fin { size } => (
                rose::Ty::Fin { size },
                rose::Constraint::Value | rose::Constraint::Index,
            ),

            rose::Ty::Ref { scope, inner } => (
                rose::Ty::Ref {
                    scope: types[scope.ty()]?,
                    inner: types[inner.ty()]?,
                },
                EnumSet::empty(),
            ),
            rose::Ty::Array { index, elem } => (
                rose::Ty::Array {
                    index: types[index.ty()]?,
                    elem: types[elem.ty()]?,
                },
                EnumSet::only(rose::Constraint::Value),
            ),
            rose::Ty::Tuple { members } => (
                rose::Ty::Tuple {
                    members: members
                        .iter()
                        .map(|&x| types[x.ty()])
                        .collect::<Option<_>>()?,
                },
                EnumSet::only(rose::Constraint::Value),
            ),
        };
        let (i, _) = self.types.insert_full(deduped, constrs);
        Some(id::ty(i))
    }

    pub fn ingest(&mut self, f: &Func, generics: &[usize]) -> Vec<usize> {
        let mut types = vec![];
        let (_, def) = f.rc.as_ref();
        // push a corresponding type onto our own `types` for each type in the callee
        for ty in def.types.iter() {
            types.push(self.resolve(generics, &types, ty));
        }

        let mut sig: Vec<_> = def
            .params
            .iter()
            .map(|x| types[def.vars[x.var()].ty()].unwrap().ty())
            .collect();
        sig.push(types[def.vars[def.ret.var()].ty()].unwrap().ty());
        sig
    }

    fn var_ty_id(&self, x: id::Var) -> id::Ty {
        self.vars[x.var()].ty
    }

    fn var_ty(&self, x: id::Var) -> &rose::Ty {
        let (ty, _) = self.types.get_index(self.var_ty_id(x).ty()).unwrap();
        ty
    }
}

/// A block under construction.
#[wasm_bindgen]
pub struct Block {
    code: Vec<rose::Instr>,
}

// just to appease Clippy
impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
impl Block {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { code: vec![] }
    }

    ///
    ///
    /// Must not use `f.constants`.
    fn finish(self, f: &mut FuncBuilder, code: &mut Vec<rose::Instr>) {
        for instr in self.code.into_iter() {
            let var = instr.var;
            code.push(instr);
            f.extra(var, code);
            match &code.last().unwrap().expr {
                &rose::Expr::Read { ret, .. } | &rose::Expr::Accum { ret, .. } => {
                    f.extra(ret, code);
                }
                _ => {}
            }
        }
    }

    fn instr(&mut self, f: &mut FuncBuilder, t: id::Ty, expr: rose::Expr) -> usize {
        let x = f.newvar(t);
        self.code.push(rose::Instr { var: x, expr });
        x.var()
    }

    #[wasm_bindgen]
    pub fn index(&mut self, f: &mut FuncBuilder, arr: usize, idx: usize) -> Result<usize, JsError> {
        let array = id::var(arr);
        let index = id::var(idx);
        let t = match f.var_ty(array) {
            &rose::Ty::Array { index: _, elem } => elem,
            _ => return Err(JsError::new("not an array")),
        };
        Ok(self.instr(f, t, rose::Expr::Index { array, index }))
    }

    // unary

    #[wasm_bindgen]
    pub fn not(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Not,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn neg(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Neg,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn abs(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Abs,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn sqrt(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Sqrt,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    // end of unary

    // binary

    #[wasm_bindgen]
    pub fn and(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::And,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn or(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Or,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn iff(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Iff,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn xor(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Xor,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn neq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Neq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn lt(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Lt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn leq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Leq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn eq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Eq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn gt(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Gt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn geq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Geq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn add(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Add,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn sub(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Sub,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn mul(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Mul,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    #[wasm_bindgen]
    pub fn div(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Div,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    // end of binary

    #[wasm_bindgen]
    pub fn select(
        &mut self,
        f: &mut FuncBuilder,
        cond: usize,
        t: usize,
        then: usize,
        els: usize,
    ) -> usize {
        let expr = rose::Expr::Select {
            cond: id::var(cond),
            then: id::var(then),
            els: id::var(els),
        };
        self.instr(f, id::ty(t), expr)
    }

    #[wasm_bindgen]
    pub fn call(
        &mut self,
        f: &mut FuncBuilder,
        g: &Func,
        generics: &[usize],
        t: usize,
        args: &[usize],
    ) -> Result<usize, JsError> {
        // add the function reference to the callee
        let id = id::function(f.functions.len());
        f.functions.push(g.clone());

        let expr = rose::Expr::Call {
            id,
            generics: generics.iter().map(|&i| id::ty(i)).collect(),
            args: args.iter().map(|&x| id::var(x)).collect(),
        };
        Ok(self.instr(f, id::ty(t), expr))
    }

    #[wasm_bindgen]
    pub fn vec(
        &mut self,
        f: &mut FuncBuilder,
        t: usize,
        arg: usize,
        body: Self,
        out: usize,
    ) -> usize {
        let arg = id::var(arg);
        let mut code = vec![];
        f.extra(arg, &mut code);
        body.finish(f, &mut code);
        let expr = rose::Expr::For {
            arg,
            body: code.into(),
            ret: id::var(out),
        };
        self.instr(f, id::ty(t), expr)
    }
}

/// Interpret a function with no generics or parameters.
///
/// The return value is Serde-converted from `rose_interp::Val`.
#[wasm_bindgen]
pub fn interp(f: &Func) -> Result<JsValue, JsError> {
    let ret = rose_interp::interp(f, IndexSet::new(), &[], [].into_iter())?;
    Ok(to_js_value(&ret)?)
}
