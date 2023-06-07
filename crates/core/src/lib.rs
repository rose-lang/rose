use serde::{Deserialize, Serialize};
use ts_rs::TS;
use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

/// Index of a typedef in the module context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Typedef(pub usize);

/// Index of a function in the module context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Defn(pub usize);

/// Index of a typevar in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Var(pub usize);

/// Index of a function instantiation in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Func(pub usize);

/// Index of a generic parameter in a definition context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Generic(pub usize);

/// Index of a member in a tuple.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Member(pub usize);

/// Index of a local variable in a function context.
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Local(pub usize);

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Size {
    Const { val: usize },
    Generic { id: Generic },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Type {
    Bool,
    Int,
    Real,
    Size { val: Size },
    Nat { bound: Size },
    Var { id: Var },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub enum Typexpr {
    Vector { elem: Type, size: Size },
    Tuple { members: Vec<Type> },
    Typedef { id: Typedef, params: Vec<Size> },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Inst {
    pub id: Defn,
    /// Generic size parameters.
    pub params: Vec<Size>,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Def<T> {
    /// Number of generic size parameters.
    pub generics: usize,
    pub types: Vec<Typexpr>,
    pub def: T,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Unop {
    // Bool -> Bool
    Not,

    // Int -> Int
    NegInt,
    AbsInt,

    // Real -> Real
    NegReal,
    AbsReal,
    Sqrt,

    // Vec<Int> -> Int
    SumInt,
    ProdInt,
    MaxInt,
    MinInt,

    // Vec<Real> -> Real
    SumReal,
    ProdReal,
    MaxReal,
    MinReal,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Binop {
    // Bool -> Bool -> Bool
    And,
    Or,
    EqBool,
    NeqBool,

    // Int -> Int -> Bool
    NeqInt,
    LtInt,
    LeqInt,
    EqInt,
    GtInt,
    GeqInt,

    // Real -> Real -> Bool
    NeqReal,
    LtReal,
    LeqReal,
    EqReal,
    GtReal,
    GeqReal,

    // Int -> Int -> Int
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    Mod,

    // Real -> Real -> Real
    AddReal,
    SubReal,
    MulReal,
    DivReal,
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    Generic { id: Generic },
    Get { id: Local },
    Set { id: Local },
    Bool { val: bool },
    Int { val: u32 },
    Real { val: f64 },
    Vector { dim: usize },
    Tuple { id: Var },
    Index,
    Member { id: Member },
    Call { id: Func },
    Unary { op: Unop },
    Binary { op: Binop },
    If,
    Else,
    End,
    For { limit: Size },
}

#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Function {
    pub params: Vec<Type>,
    pub ret: Vec<Type>,
    pub locals: Vec<Type>,
    pub funcs: Vec<Inst>,
    pub body: Vec<Instr>,
}

impl Function {
    pub fn get_func(&self, id: Func) -> &Inst {
        &self.funcs[id.0]
    }
}

#[wasm_bindgen]
#[cfg_attr(test, derive(TS), ts(export))]
#[cfg_attr(feature = "serde-enable", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Module {
    #[wasm_bindgen(skip)]
    pub types: Vec<Def<Typexpr>>,

    #[wasm_bindgen(skip)]
    pub funcs: Vec<Def<Function>>,
}

impl Module {
    pub fn get_func(&self, id: Defn) -> &Def<Function> {
        &self.funcs[id.0]
    }
}

#[wasm_bindgen]
#[cfg(feature = "serde-enable")]
pub fn module_from_js(my_mod_js: JsValue) -> Result<Module, JsValue> {
    // Deserialize the JavaScript object to the Rust Module struct
    let my_mod: Module = serde_wasm_bindgen::from_value(my_mod_js)?;
    Ok(my_mod)
}

#[wasm_bindgen]
#[cfg(feature = "serde-enable")]
pub fn module_to_js(my_mod: &Module) -> JsValue {
    // Serialize the modified Module object back to a JsValue and return the modified JsValue
    to_js_value(&my_mod).unwrap()
}

fn to_js_value(value: &(impl Serialize + ?Sized)) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}
