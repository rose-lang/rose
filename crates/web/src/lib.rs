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

#[wasm_bindgen]
pub struct Func(Rc<rose::Def<rose::Function>>);

#[wasm_bindgen(js_name = "makeFunc")]
pub fn make_func(
    param_types: JsValue,
    local_types: JsValue,
    body: JsValue,
) -> Result<Func, serde_wasm_bindgen::Error> {
    let params: Vec<rose::Type> = serde_wasm_bindgen::from_value(param_types)?;
    let locals: Vec<rose::Type> = serde_wasm_bindgen::from_value(local_types)?;
    let body: Vec<rose::Instr> = serde_wasm_bindgen::from_value(body)?;
    Ok(Func(Rc::new(rose::Def {
        generics: 0,
        types: vec![],
        def: rose::Function {
            params,
            ret: vec![rose::Type::Real],
            locals,
            funcs: vec![],
            body,
        },
    })))
}

#[wasm_bindgen]
pub fn interp(Func(f): &Func, args: JsValue) -> Result<JsValue, serde_wasm_bindgen::Error> {
    let vals: Vec<rose_interp::Val> = serde_wasm_bindgen::from_value(args)?;
    let ret = rose_interp::interp(f, vals);
    assert_eq!(ret.len(), 1);
    to_js_value(&ret[0])
}
