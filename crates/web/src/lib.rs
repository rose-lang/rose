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
pub struct Body {
    instrs: Vec<rose::Instr>,
}

// just to appease Clippy
impl Default for Body {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
impl Body {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Body { instrs: vec![] }
    }

    #[wasm_bindgen]
    pub fn get(&mut self, id: usize) {
        self.instrs.push(rose::Instr::Get {
            id: rose::Local(id),
        });
    }

    #[wasm_bindgen]
    pub fn set(&mut self, id: usize) {
        self.instrs.push(rose::Instr::Set {
            id: rose::Local(id),
        });
    }

    #[wasm_bindgen]
    pub fn real(&mut self, val: f64) {
        self.instrs.push(rose::Instr::Real { val });
    }

    #[wasm_bindgen(js_name = "addReal")]
    pub fn add_real(&mut self) {
        self.instrs.push(rose::Instr::Binary {
            op: rose::Binop::AddReal,
        });
    }

    #[wasm_bindgen(js_name = "subReal")]
    pub fn sub_real(&mut self) {
        self.instrs.push(rose::Instr::Binary {
            op: rose::Binop::SubReal,
        });
    }

    #[wasm_bindgen(js_name = "mulReal")]
    pub fn mul_real(&mut self) {
        self.instrs.push(rose::Instr::Binary {
            op: rose::Binop::MulReal,
        });
    }

    #[wasm_bindgen(js_name = "divReal")]
    pub fn div_real(&mut self) {
        self.instrs.push(rose::Instr::Binary {
            op: rose::Binop::DivReal,
        });
    }
}

#[wasm_bindgen]
pub struct Func(Rc<rose::Def<rose::Function>>);

#[wasm_bindgen(js_name = "makeFunc")]
pub fn make_func(
    param_types: JsValue,
    local_types: JsValue,
    body: Body,
) -> Result<Func, serde_wasm_bindgen::Error> {
    let params: Vec<rose::Type> = serde_wasm_bindgen::from_value(param_types)?;
    let locals: Vec<rose::Type> = serde_wasm_bindgen::from_value(local_types)?;
    Ok(Func(Rc::new(rose::Def {
        generics: 0,
        types: vec![],
        def: rose::Function {
            params,
            ret: vec![rose::Type::Real],
            locals,
            funcs: vec![],
            body: body.instrs,
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
