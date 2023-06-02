use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn initialize() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
pub fn greet(name: String) -> String {
    format!("Hello, {}!", name)
}

#[wasm_bindgen]
pub fn double(x: u32) -> u32 {
    x * 2
}
