use serde::Serialize;
use std::rc::Rc;
use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

#[wasm_bindgen]
pub fn initialize() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

fn to_js_value(value: &impl Serialize) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}

/// A reference-counted pointer to a function.
#[wasm_bindgen]
pub struct Func(Rc<rose::Def<rose::Function>>);

/// An under-construction function body.
///
/// All methods simply push one instruction onto the end.
#[wasm_bindgen]
pub struct Context {
    generics: usize,
    types: Vec<rose::Typexpr>,
    params: Vec<rose::Type>,
    ret: rose::Type,
    locals: Vec<rose::Type>,
    funcs: Vec<rose::Inst>,
    body: Vec<rose::Instr>,
}

/// Start a new context.
///
/// The `param_types` argument is Serde-converted to `Vec<rose::Type>`, and the `ret_type` argument
/// is Serde-converted to `rose::Type`.
///
/// TODO: currently no support for
/// - generics
/// - non-primitive types
/// - calling other functions
#[wasm_bindgen(js_name = "makeContext")]
pub fn make_context(
    param_types: JsValue,
    ret_type: JsValue,
) -> Result<Context, serde_wasm_bindgen::Error> {
    let params: Vec<rose::Type> = serde_wasm_bindgen::from_value(param_types)?;
    let ret: rose::Type = serde_wasm_bindgen::from_value(ret_type)?;
    Ok(Context {
        generics: 0,
        types: vec![],
        params,
        ret,
        locals: vec![],
        funcs: vec![],
        body: vec![],
    })
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen]
    pub fn bake(self) -> Func {
        Func(Rc::new(rose::Def {
            generics: self.generics,
            types: self.types,
            def: rose::Function {
                params: self.params,
                ret: vec![self.ret],
                locals: self.locals,
                funcs: self.funcs,
                body: self.body,
            },
        }))
    }

    #[wasm_bindgen(js_name = "makeLocal")]
    pub fn make_local(&mut self, t: JsValue) -> Result<usize, serde_wasm_bindgen::Error> {
        let t: rose::Type = serde_wasm_bindgen::from_value(t)?;
        let id = self.locals.len();
        self.locals.push(t);
        Ok(id)
    }

    #[wasm_bindgen]
    pub fn get(&mut self, id: usize) {
        self.body.push(rose::Instr::Get {
            id: rose::Local(id),
        });
    }

    #[wasm_bindgen]
    pub fn set(&mut self, id: usize) {
        self.body.push(rose::Instr::Set {
            id: rose::Local(id),
        });
    }

    #[wasm_bindgen]
    pub fn real(&mut self, val: f64) {
        self.body.push(rose::Instr::Real { val });
    }

    #[wasm_bindgen(js_name = "addReal")]
    pub fn add_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::AddReal,
        });
    }

    #[wasm_bindgen(js_name = "subReal")]
    pub fn sub_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::SubReal,
        });
    }

    #[wasm_bindgen(js_name = "mulReal")]
    pub fn mul_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::MulReal,
        });
    }

    #[wasm_bindgen(js_name = "divReal")]
    pub fn div_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::DivReal,
        });
    }
}

/// Interpret a function with the given arguments.
///
/// The `args` are each Serde-converted to `Vec<rose_interp::Val>`, and the return value is
/// Serde-converted from `rose_interp::Val`.
#[wasm_bindgen]
pub fn interp(Func(f): &Func, args: JsValue) -> Result<JsValue, serde_wasm_bindgen::Error> {
    let vals: Vec<rose_interp::Val> = serde_wasm_bindgen::from_value(args)?;
    let ret = rose_interp::interp(f, vals);
    assert_eq!(ret.len(), 1);
    to_js_value(&ret[0])
}
