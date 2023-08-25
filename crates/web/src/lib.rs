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

mod str_id {
    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub struct StrId(usize);

    pub fn str_id(id: usize) -> StrId {
        StrId(id)
    }

    impl StrId {
        pub fn str(self) -> usize {
            self.0
        }
    }
}

use str_id::{str_id, StrId};

#[derive(Debug)]
struct Inner {
    deps: Box<[Func]>,
    def: rose::Function,
    structs: Box<[Option<Box<[StrId]>>]>,
}

/// A node in a reference-counted acyclic digraph of functions.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct Func {
    rc: Rc<Inner>,
}

impl<'a> rose::FuncNode for &'a Func {
    fn def(&self) -> &rose::Function {
        &self.rc.as_ref().def
    }

    fn get(&self, id: id::Function) -> Option<Self> {
        self.rc.as_ref().deps.get(id.function())
    }
}

#[wasm_bindgen]
impl Func {
    #[wasm_bindgen(js_name = "retType")]
    pub fn ret_type(&self) -> usize {
        let def = &self.rc.as_ref().def;
        def.vars[def.ret.var()].ty()
    }

    pub fn elem(&self, t: usize) -> usize {
        match self.rc.as_ref().def.types[t] {
            rose::Ty::Array { index: _, elem } => elem.ty(),
            _ => panic!("not an array"),
        }
    }

    pub fn key(&self, t: usize, i: usize) -> usize {
        self.rc.as_ref().structs[t].as_ref().unwrap()[i].str()
    }

    pub fn mem(&self, t: usize, i: usize) -> usize {
        match &self.rc.as_ref().def.types[t] {
            rose::Ty::Tuple { members } => members[i].ty(),
            _ => panic!("not a struct"),
        }
    }

    /// Interpret a function with no generics or parameters.
    ///
    /// The return value is Serde-converted from `rose_interp::Val`.
    pub fn interp(&self) -> Result<JsValue, JsError> {
        let ret = rose_interp::interp(self, IndexSet::new(), &[], [].into_iter())?;
        Ok(to_js_value(&ret)?)
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
    let def = &f.rc.as_ref().def;

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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Ty {
    Unit,
    Bool,
    F64,
    Fin { size: usize },
    Ref { scope: id::Ty, inner: id::Ty },
    Array { index: id::Ty, elem: id::Ty },
    Struct { members: Box<[(StrId, id::Ty)]> },
}

impl Ty {
    fn ty(&self) -> rose::Ty {
        match self {
            Ty::Unit => rose::Ty::Unit,
            Ty::Bool => rose::Ty::Bool,
            Ty::F64 => rose::Ty::F64,
            &Ty::Fin { size } => rose::Ty::Fin { size },
            &Ty::Ref { scope, inner } => rose::Ty::Ref { scope, inner },
            &Ty::Array { index, elem } => rose::Ty::Array { index, elem },
            Ty::Struct { members } => rose::Ty::Tuple {
                members: members.iter().map(|&(_, t)| t).collect(),
            },
        }
    }

    fn keys(&self) -> Option<Box<[StrId]>> {
        match self {
            Ty::Struct { members } => Some(members.iter().map(|&(k, _)| k).collect()),
            _ => None,
        }
    }
}

/// Metadata about a variable while its containing function is still under construction.
enum Extra {
    /// Does not depend on any of the function's parameters.
    Constant,

    /// Part of the main function body; these are definitions for variables that depend on it.
    Parent(Vec<rose::Instr>),

    /// Depends on a `Parent` variable; others can depend on it only indirectly through its parent.
    Child(id::Var),

    /// Is no longer in scope.
    Expired,
}

struct Var {
    t: id::Ty,
    extra: Extra,
}

/// A function under construction.
#[wasm_bindgen]
pub struct FuncBuilder {
    /// Called functions. More can be added as the function is built.
    functions: Vec<Func>,

    /// Constraints on generic type parameters. These are fixed when the `FuncBuilder` is started.
    generics: Box<[EnumSet<rose::Constraint>]>,

    /// Index of types, with constraints tracked for validation (e.g. array index must be `Index`).
    types: IndexMap<Ty, EnumSet<rose::Constraint>>,

    /// Variable types, scopes (expired or not?), and dependent definitions.
    vars: Vec<Var>,

    /// Parameters, in order. Typically added all at once right after the `FuncBuilder` is started.
    params: Vec<id::Var>,

    /// Definitions that don't depend on parameters (but may depend on each other), in order.
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
    pub fn finish(mut self, out: usize, body: Block) -> Func {
        // We replace `self.params` and `self.constants` with empty vec because we need to satisfy
        // the borrow checker when we pass `self` to `body.finish` below; this is OK though, because
        // `Block::finish` is guaranteed not to use either `self.params` or `self.constants`.
        let params = std::mem::take(&mut self.params).into_boxed_slice();
        let mut code = std::mem::take(&mut self.constants);
        for &x in params.iter() {
            self.extra(x, &mut code);
        }
        body.finish(&mut self, &mut code);
        Func {
            rc: Rc::new(Inner {
                deps: self.functions.into(),
                def: rose::Function {
                    generics: self.generics,
                    types: self.types.keys().map(|t| t.ty()).collect(),
                    vars: self.vars.into_iter().map(|x| x.t).collect(),
                    params,
                    ret: id::var(out),
                    body: code.into(),
                },
                structs: self.types.keys().map(|t| t.keys()).collect(),
            }),
        }
    }

    /// Finalize `x`, appending its dependencies onto `code` and marking it and them as expired.
    ///
    /// Must not use `self.params` or `self.constants`.
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
        matches!(ty, Ty::Fin { .. })
    }

    #[wasm_bindgen(js_name = "isArray")]
    pub fn is_array(&self, t: usize) -> bool {
        let (ty, _) = self.types.get_index(t).unwrap();
        matches!(ty, Ty::Array { .. })
    }

    #[wasm_bindgen(js_name = "isStruct")]
    pub fn is_struct(&self, t: usize) -> bool {
        let (ty, _) = self.types.get_index(t).unwrap();
        matches!(ty, Ty::Struct { .. })
    }

    /// Return a reference to the type with ID `t` if it exists, `Err` otherwise.
    ///
    /// This returns a `Result` with `JsError` because it is a helper method meant to be used by
    /// `pub` methods exposed to JavaScript; don't prefer it for more Rusty stuff, because `JsError`
    /// doesn't implement `Debug` so you can't easily call `Result::unwrap` here.
    fn ty(&self, t: usize) -> Result<&Ty, JsError> {
        match self.types.get_index(t) {
            None => Err(JsError::new("type does not exist")),
            Some((ty, _)) => Ok(ty),
        }
    }

    /// Return the ID of the index type for the array type with ID `t`.
    ///
    /// `Err` if `t` is out of range or does not represent an array type.
    pub fn index(&self, t: usize) -> Result<usize, JsError> {
        match self.ty(t)? {
            &Ty::Array { index, elem: _ } => Ok(index.ty()),
            _ => Err(JsError::new("type is not an array")),
        }
    }

    /// Return the number of elements for the array type with ID `t`.
    ///
    /// `Err` if `t` is out of range or does not represent an array type, or if its index type is
    /// not a fixed size (e.g. if it is a generic type parameter of the function).
    pub fn size(&self, t: usize) -> Result<usize, JsError> {
        match self.ty(t)? {
            &Ty::Array { index, elem: _ } => {
                let (i, _) = self.types.get_index(index.ty()).unwrap();
                match i {
                    &Ty::Fin { size } => Ok(size),
                    _ => Err(JsError::new("index type is not a fixed size")),
                }
            }
            _ => Err(JsError::new("type is not an array")),
        }
    }

    /// Return the ID of the element type for the array type with ID `t`.
    ///
    /// `Err` if `t` is out of range or does not represent an array type.
    pub fn elem(&self, t: usize) -> Result<usize, JsError> {
        match self.ty(t)? {
            &Ty::Array { index: _, elem } => Ok(elem.ty()),
            _ => Err(JsError::new("type is not an array")),
        }
    }

    pub fn keys(&self, t: usize) -> Result<Vec<usize>, JsError> {
        match self.ty(t)? {
            Ty::Struct { members } => Ok(members.iter().map(|&(id, _)| id.str()).collect()),
            _ => Err(JsError::new("type is not a struct")),
        }
    }

    pub fn members(&self, t: usize) -> Result<Vec<usize>, JsError> {
        match self.ty(t)? {
            Ty::Struct { members } => Ok(members.iter().map(|&(_, id)| id.ty()).collect()),
            _ => Err(JsError::new("type is not a struct")),
        }
    }

    /// Return `x` if it exists, is in scope, and has type ID `t`; `Err` otherwise.
    pub fn expect(&self, t: usize, x: usize) -> Result<usize, JsError> {
        match self.vars.get(x) {
            None => Err(JsError::new("variable does not exist")),
            Some(var) => match var.extra {
                Extra::Expired => Err(JsError::new("variable is out of scope")),
                _ => {
                    if var.t == id::ty(t) {
                        Ok(x)
                    } else {
                        Err(JsError::new("variable type mismatch"))
                    }
                }
            },
        }
    }

    /// Return the type ID for `ty`, creating if needed, and marking its constraints as `constrs`.
    fn newtype(&mut self, ty: Ty, constrs: EnumSet<rose::Constraint>) -> usize {
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
            t,
            extra: Extra::Parent(vec![]),
        });
        id::var(id)
    }

    /// Return the ID for the unit type, creating if needed.
    #[wasm_bindgen(js_name = "tyUnit")]
    pub fn ty_unit(&mut self) -> usize {
        self.newtype(Ty::Unit, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the boolean type, creating if needed.
    #[wasm_bindgen(js_name = "tyBool")]
    pub fn ty_bool(&mut self) -> usize {
        self.newtype(Ty::Bool, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the 64-bit floating-point type, creating if needed.
    #[wasm_bindgen(js_name = "tyF64")]
    pub fn ty_f64(&mut self) -> usize {
        self.newtype(Ty::F64, EnumSet::only(rose::Constraint::Value))
    }

    /// Return the ID for the type of nonnegative integers less than `size`, creating if needed.
    #[wasm_bindgen(js_name = "tyFin")]
    pub fn ty_fin(&mut self, size: usize) -> usize {
        self.newtype(
            Ty::Fin { size },
            rose::Constraint::Value | rose::Constraint::Index,
        )
    }

    /// Return the ID for the type of arrays with index type `index` and element type `elem`,
    ///
    /// Assumes `index` and `elem` are valid type IDs.
    #[wasm_bindgen(js_name = "tyArray")]
    pub fn ty_array(&mut self, index: usize, elem: usize) -> Result<usize, JsError> {
        let (_, constrs) = self.types.get_index(index).unwrap();
        // If we support non-`Value` types then we should also check that `elem` satisfies `Value`.
        if constrs.contains(rose::Constraint::Index) {
            Ok(self.newtype(
                Ty::Array {
                    index: id::ty(index),
                    elem: id::ty(elem),
                },
                EnumSet::only(rose::Constraint::Value),
            ))
        } else {
            Err(JsError::new("index type cannot be used as an index"))
        }
    }

    #[wasm_bindgen(js_name = "tyStruct")]
    pub fn ty_struct(&mut self, keys: &[usize], mems: &[usize]) -> usize {
        self.newtype(
            Ty::Struct {
                members: keys
                    .iter()
                    .zip(mems.iter())
                    .map(|(&s, &t)| (str_id(s), id::ty(t)))
                    .collect(),
            },
            EnumSet::only(rose::Constraint::Value),
        )
    }

    /// Return the ID of a new variable with type ID `t`.
    pub fn bind(&mut self, t: usize) -> usize {
        self.newvar(id::ty(t)).var()
    }

    /// Append a parameter with type ID `t` and return its variable ID.
    pub fn param(&mut self, t: usize) -> usize {
        let x = self.newvar(id::ty(t));
        self.params.push(x);
        x.var()
    }

    /// Append a constant with type ID `t` and definition `expr`, and return its variable ID.
    fn constant(&mut self, t: usize, expr: rose::Expr) -> usize {
        let x = self.vars.len();
        self.vars.push(Var {
            t: id::ty(t),
            extra: Extra::Constant,
        });
        self.constants.push(rose::Instr {
            var: id::var(x),
            expr,
        });
        x
    }

    /// Create a constant variable with the unit type, and return its ID.
    ///
    /// `Err` if `t` is not the ID of the unit type.
    pub fn unit(&mut self, t: usize) -> Result<usize, JsError> {
        if t == self.ty_unit() {
            Ok(self.constant(t, rose::Expr::Unit))
        } else {
            Err(JsError::new("did not expect null"))
        }
    }

    /// Create a constant variable with the boolean type and value `val`, and return its ID.
    ///
    /// `Err` if `t` is not the ID of the boolean type.
    pub fn bool(&mut self, t: usize, val: bool) -> Result<usize, JsError> {
        if t == self.ty_bool() {
            Ok(self.constant(t, rose::Expr::Bool { val }))
        } else {
            Err(JsError::new("did not expect boolean"))
        }
    }

    /// Return the ID of a new numeric constant variable with type `t` and value converted from `x`.
    ///
    /// `Err` unless `t` is the ID of either the 64-bit floating-point type or a finite nonnegative
    /// integer type; or if `t` is an integer type which cannot represent the given value of `x`.
    pub fn num(&mut self, t: usize, x: f64) -> Result<usize, JsError> {
        match self.ty(t)? {
            Ty::F64 => Ok(self.constant(t, rose::Expr::F64 { val: x })),
            &Ty::Fin { size } => {
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

    fn attach(&mut self, t: usize, xs: &[usize], expr: rose::Expr) -> usize {
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
                    t: id::ty(t),
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

    /// Return the ID of a new array variable with type ID `t` and elements `xs`.
    ///
    /// Assumes that `t` is a valid type ID and `xs` are all valid variable IDs. If there are no
    /// dependencies on parameters then the new array variable is a constant; otherwise, it is
    /// attached to whichever parent variable reachable from `xs` has the highest ID.
    pub fn array(&mut self, t: usize, xs: &[usize]) -> usize {
        let elems = xs.iter().map(|&x| id::var(x)).collect();
        let expr = rose::Expr::Array { elems };
        self.attach(t, xs, expr)
    }

    pub fn obj(&mut self, t: usize, xs: &[usize]) -> usize {
        let members = xs.iter().map(|&x| id::var(x)).collect();
        let expr = rose::Expr::Tuple { members };
        self.attach(t, xs, expr)
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
        strings: &[usize],
        structs: &[Option<Box<[StrId]>>],
        types: &[Option<id::Ty>],
        t: usize,
        ty: &rose::Ty,
    ) -> Option<id::Ty> {
        let (deduped, constrs) = match ty {
            // inner scopes can't be in the param or return types, which are all we care about here
            rose::Ty::Scope { kind: _, id: _ } => return None,
            rose::Ty::Generic { id } => return Some(id::ty(generics[id.generic()])),

            rose::Ty::Unit => (Ty::Unit, EnumSet::only(rose::Constraint::Value)),
            rose::Ty::Bool => (Ty::Bool, EnumSet::only(rose::Constraint::Value)),
            rose::Ty::F64 => (Ty::F64, EnumSet::only(rose::Constraint::Value)),
            &rose::Ty::Fin { size } => (
                Ty::Fin { size },
                rose::Constraint::Value | rose::Constraint::Index,
            ),

            rose::Ty::Ref { scope, inner } => (
                Ty::Ref {
                    scope: types[scope.ty()]?,
                    inner: types[inner.ty()]?,
                },
                EnumSet::empty(),
            ),
            rose::Ty::Array { index, elem } => (
                Ty::Array {
                    index: types[index.ty()]?,
                    elem: types[elem.ty()]?,
                },
                EnumSet::only(rose::Constraint::Value),
            ),
            rose::Ty::Tuple { members } => (
                Ty::Struct {
                    members: members
                        .iter()
                        .zip(structs[t].as_ref().unwrap().iter())
                        .map(|(x, s)| Some((str_id(strings[s.str()]), types[x.ty()]?)))
                        .collect::<Option<_>>()?,
                },
                EnumSet::only(rose::Constraint::Value),
            ),
        };
        let (i, _) = self.types.insert_full(deduped, constrs);
        Some(id::ty(i))
    }

    /// Return the parameter and return type IDs in this function for calling `f` with `generics`.
    ///
    /// Assumes `generics` are all valid type IDs, and have the right length and constraints.
    ///
    /// The returned `Vec` is always nonempty, since its last element is the return type; all the
    /// other elements are the parameter types.
    pub fn ingest(&mut self, f: &Func, strings: &[usize], generics: &[usize]) -> Vec<usize> {
        let mut types = vec![];
        let inner = f.rc.as_ref();
        let def = &inner.def;
        // push a corresponding type onto our own `types` for each type in the callee
        for (t, ty) in def.types.iter().enumerate() {
            types.push(self.resolve(generics, strings, &inner.structs, &types, t, ty));
        }

        let mut sig: Vec<_> = def
            .params
            .iter()
            .map(|x| types[def.vars[x.var()].ty()].unwrap().ty())
            .collect();
        sig.push(types[def.vars[def.ret.var()].ty()].unwrap().ty());
        sig
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
    /// Start building a block.
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { code: vec![] }
    }

    /// Pour the contents of this block (including dependent variables) into `code`.
    ///
    /// Must not use `f.params` or `f.constants`. Marks all variables defined in this block as
    /// expired.
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

    /// Define a new variable in this block with type `t` and definition `expr`, and return its ID.
    fn instr(&mut self, f: &mut FuncBuilder, t: id::Ty, expr: rose::Expr) -> usize {
        let x = f.newvar(t);
        self.code.push(rose::Instr { var: x, expr });
        x.var()
    }

    pub fn index(&mut self, f: &mut FuncBuilder, t: usize, arr: usize, idx: usize) -> usize {
        let array = id::var(arr);
        let index = id::var(idx);
        self.instr(f, id::ty(t), rose::Expr::Index { array, index })
    }

    pub fn member(&mut self, f: &mut FuncBuilder, t: usize, x: usize, mem: usize) -> usize {
        let tuple = id::var(x);
        let member = id::member(mem);
        self.instr(f, id::ty(t), rose::Expr::Member { tuple, member })
    }

    // unary

    /// Return the variable ID for a new boolean negation instruction on `arg`.
    ///
    /// Assumes `arg` is defined, in scope, and has boolean type.
    pub fn not(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Not,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new floating-point negation instruction on `arg`.
    ///
    /// Assumes `arg` is defined, in scope, and has 64-bit floating point type.
    pub fn neg(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Neg,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new absolute value instruction on `arg`.
    ///
    /// Assumes `arg` is defined, in scope, and has 64-bit floating point type.
    pub fn abs(&mut self, f: &mut FuncBuilder, arg: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Unary {
            op: rose::Unop::Abs,
            arg: id::var(arg),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new square root instruction on `arg`.
    ///
    /// Assumes `arg` is defined, in scope, and has 64-bit floating point type.
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

    /// Return the variable ID for a new logical conjunction instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have boolean type.
    pub fn and(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::And,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new logical disjunction instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have boolean type.
    pub fn or(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Or,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new boolean equality instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have boolean type.
    pub fn iff(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Iff,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new exclusive disjunction instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have boolean type.
    pub fn xor(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Xor,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "not equal" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn neq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Neq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "less than" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn lt(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Lt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "less than or equal" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn leq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Leq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "equal" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn eq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Eq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "greater than" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn gt(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Gt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new "greater than or equal" instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn geq(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_bool());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Geq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new addition instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn add(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Add,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new subtraction instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn sub(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Sub,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new multiplication instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
    pub fn mul(&mut self, f: &mut FuncBuilder, left: usize, right: usize) -> usize {
        let t = id::ty(f.ty_f64());
        let expr = rose::Expr::Binary {
            op: rose::Binop::Mul,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(f, t, expr)
    }

    /// Return the variable ID for a new division instruction on `left` and `right`.
    ///
    /// Assumes `left` and `right` are defined, in scope, and have 64-bit floating point type.
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

    /// Return the variable ID for a new instruction using `cond` to choose `then` or `els`.
    ///
    /// Assumes `cond`, `then`, and `els` are defined and in scope, that `cond` has boolean type,
    /// and that `then` and `els` both have type `t`.
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

    /// Return the variable ID for a new instruction calling `g` with `generics` and `args`.
    ///
    /// Assumes that `generics` are all valid type IDs, and have the right length and constraints;
    /// that `args` are all valid variable IDs and match up with `g`'s parameter types; and that the
    /// return type of `g` matches `t` (all in the context of the given `generics`).
    pub fn call(
        &mut self,
        f: &mut FuncBuilder,
        g: &Func,
        generics: &[usize],
        t: usize,
        args: &[usize],
    ) -> usize {
        // add the function reference to the callee
        let id = id::function(f.functions.len());
        f.functions.push(g.clone());

        let expr = rose::Expr::Call {
            id,
            generics: generics.iter().map(|&i| id::ty(i)).collect(),
            args: args.iter().map(|&x| id::var(x)).collect(),
        };
        self.instr(f, id::ty(t), expr)
    }

    /// Return the variable ID for a new instruction defining an array elementwise via `body`.
    ///
    /// Assumes `arg` is defined and in scope; this represents the index variable for the element
    /// definition body, so its dependencies are prepended to the body code and it is marked as
    /// expired. Also assumes `out` is defined by `body`; this represents the final variable
    /// defining each array element. Finally, assumes the type of the array (not the element)
    /// matches `t`.
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
