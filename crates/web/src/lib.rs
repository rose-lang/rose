use rose::{build, id};
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

/// A reference-counted pointer to a function.
#[wasm_bindgen]
pub struct Func(Rc<rose::Function>);

#[cfg(feature = "debug")]
#[wasm_bindgen(js_name = "js2Rust")]
pub fn js_to_rust(Func(f): &Func) -> String {
    let def: &rose::Def<rose::Function> = f;
    let func: &rose::Function = &def.def;
    format!("{:#?}", func)
}

/// A function under construction.
#[wasm_bindgen]
pub struct FuncBuilder {
    f: build::Function,
}

#[wasm_bindgen]
pub struct Block {
    f: FuncBuilder,
}

#[wasm_bindgen]
pub fn bake(ctx: Context) -> Func {
    Func(Rc::new(rose::Def {
        generics: ctx.generics,
        types: ctx.types,
        def: rose::Function {
            params: ctx.params,
            ret: vec![ctx.ret],
            locals: ctx.locals,
            funcs: ctx.funcs,
            body: ctx.body,
        },
    }))
}

#[wasm_bindgen]
impl Context {
    /// The `param_types` argument is Serde-converted to `Vec<rose::Type>`, and the `ret_type`
    /// argument is Serde-converted to `rose::Type`.
    ///
    /// TODO: currently no support for
    /// - generics
    /// - non-primitive types
    /// - calling other functions
    #[wasm_bindgen(constructor)]
    pub fn new(param_types: JsValue, ret_type: JsValue) -> Result<Context, JsError> {
        let params: Vec<rose::Type> = serde_wasm_bindgen::from_value(param_types)?;
        let ret: rose::Type = serde_wasm_bindgen::from_value(ret_type)?;
        Ok(Self {
            generics: 0,
            types: vec![],
            params,
            ret,
            locals: vec![],
            funcs: vec![],
            body: vec![],
        })
    }

    /// Create a new local and then emit a `rose::Instr::Set` instruction.
    ///
    /// The `t` argument is Serde-converted to `rose::Type`.
    #[wasm_bindgen]
    pub fn set(&mut self, t: JsValue) -> Result<usize, JsError> {
        let local: rose::Type = serde_wasm_bindgen::from_value(t)?;
        let id = self.locals.len();
        self.locals.push(local);
        self.body.push(rose::Instr::Set { id: id::local(id) });
        Ok(id)
    }

    #[wasm_bindgen]
    pub fn generic(&mut self, id: usize) {
        self.body.push(rose::Instr::Generic {
            id: id::generic(id),
        });
    }

    #[wasm_bindgen]
    pub fn get(&mut self, id: usize) {
        self.body.push(rose::Instr::Get { id: id::local(id) });
    }

    #[wasm_bindgen]
    pub fn bool(&mut self, val: bool) {
        self.body.push(rose::Instr::Bool { val });
    }

    #[wasm_bindgen]
    pub fn real(&mut self, val: f64) {
        self.body.push(rose::Instr::Real { val });
    }

    #[wasm_bindgen]
    pub fn vector(&mut self, id: usize) {
        self.body.push(rose::Instr::Vector { id: id::typ(id) });
    }

    #[wasm_bindgen]
    pub fn tuple(&mut self, id: usize) {
        self.body.push(rose::Instr::Tuple { id: id::typ(id) });
    }

    #[wasm_bindgen]
    pub fn index(&mut self) {
        self.body.push(rose::Instr::Index);
    }

    #[wasm_bindgen]
    pub fn member(&mut self, id: usize) {
        self.body.push(rose::Instr::Member { id: id::member(id) });
    }

    // unary

    #[wasm_bindgen]
    pub fn not(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::Not,
        });
    }

    #[wasm_bindgen(js_name = "negReal")]
    pub fn neg(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::NegReal,
        });
    }

    #[wasm_bindgen(js_name = "absReal")]
    pub fn abs(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::AbsReal,
        });
    }

    #[wasm_bindgen]
    pub fn sqrt(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::Sqrt,
        });
    }

    // end of unary

    // binary

    #[wasm_bindgen]
    pub fn and(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::And,
        });
    }

    #[wasm_bindgen]
    pub fn or(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::Or,
        });
    }

    #[wasm_bindgen(js_name = "eqBool")]
    pub fn eq_bool(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::EqBool,
        });
    }

    #[wasm_bindgen(js_name = "neqBool")]
    pub fn neq_bool(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::NeqBool,
        });
    }

    #[wasm_bindgen(js_name = "neqReal")]
    pub fn neq(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::NeqReal,
        });
    }

    #[wasm_bindgen(js_name = "ltReal")]
    pub fn lt(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LtReal,
        });
    }

    #[wasm_bindgen(js_name = "leqReal")]
    pub fn leq(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LeqReal,
        });
    }

    #[wasm_bindgen(js_name = "eqReal")]
    pub fn eq(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::EqReal,
        });
    }

    #[wasm_bindgen(js_name = "gtReal")]
    pub fn gt(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GtReal,
        });
    }

    #[wasm_bindgen(js_name = "geqReal")]
    pub fn geq(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GeqReal,
        });
    }

    #[wasm_bindgen(js_name = "addReal")]
    pub fn add(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::AddReal,
        });
    }

    #[wasm_bindgen(js_name = "subReal")]
    pub fn sub(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::SubReal,
        });
    }

    #[wasm_bindgen(js_name = "mulReal")]
    pub fn mul(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::MulReal,
        });
    }

    #[wasm_bindgen(js_name = "divReal")]
    pub fn div(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::DivReal,
        });
    }

    // end of binary

    /// `rose::Instr::If`
    #[wasm_bindgen]
    pub fn cond(&mut self) {
        self.body.push(rose::Instr::If);
    }

    /// `rose::Instr::Else`
    #[wasm_bindgen]
    pub fn alt(&mut self) {
        self.body.push(rose::Instr::Else);
    }

    #[wasm_bindgen]
    pub fn end(&mut self) {
        self.body.push(rose::Instr::End);
    }

    #[wasm_bindgen(js_name = "forConst")]
    pub fn for_const(&mut self, val: usize) {
        self.body.push(rose::Instr::For {
            limit: rose::Size::Const { val },
        });
    }

    #[wasm_bindgen(js_name = "forGeneric")]
    pub fn for_generic(&mut self, id: usize) {
        self.body.push(rose::Instr::For {
            limit: rose::Size::Generic {
                id: id::generic(id),
            },
        });
    }
}

/// Interpret a function with the given arguments.
///
/// The `args` are each Serde-converted to `Vec<rose_interp::Val>`, and the return value is
/// Serde-converted from `rose_interp::Val`.
#[wasm_bindgen]
pub fn interp(Func(f): &Func, generics: &[usize], args: JsValue) -> Result<JsValue, JsError> {
    let vals: Vec<rose_interp::Val> = serde_wasm_bindgen::from_value(args)?;
    let ret = rose_interp::interp(f, generics, vals)?;
    assert_eq!(ret.len(), 1);
    Ok(to_js_value(&ret[0])?)
}
