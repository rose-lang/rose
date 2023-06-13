use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

#[wasm_bindgen]
pub fn initialize() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct Body(Vec<rose::Instr>);

#[wasm_bindgen]
pub fn body_from_js(body_js: JsValue) -> Result<Body, JsValue> {
    // Deserialize the JavaScript object to the Rust Module struct
    let body: Body = serde_wasm_bindgen::from_value(body_js)?;
    Ok(body)
}

#[wasm_bindgen]
pub fn body_to_js(body: &Body) -> JsValue {
    // Serialize the modified Module object back to a JsValue and return the modified JsValue
    to_js_value(&body).unwrap()
}

fn to_js_value(value: &(impl Serialize + ?Sized)) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}
