use rose::Module;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

#[wasm_bindgen]
pub fn initialize() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
#[cfg(feature = "serde")]
pub fn module_from_js(my_mod_js: JsValue) -> Result<Module, JsValue> {
    // Deserialize the JavaScript object to the Rust Module struct
    let my_mod: Module = serde_wasm_bindgen::from_value(my_mod_js)?;
    Ok(my_mod)
}

#[wasm_bindgen]
#[cfg(feature = "serde")]
pub fn module_to_js(my_mod: &Module) -> JsValue {
    // Serialize the modified Module object back to a JsValue and return the modified JsValue
    to_js_value(&my_mod).unwrap()
}

fn to_js_value(value: &(impl Serialize + ?Sized)) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}
