use rose::id;
use serde::Serialize;
use std::rc::Rc;
use wasm_bindgen::prelude::{wasm_bindgen, JsError, JsValue};

#[wasm_bindgen]
pub fn initialize() {
    #[cfg(feature = "debug")]
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

fn to_js_value(value: &impl Serialize) -> Result<JsValue, serde_wasm_bindgen::Error> {
    value.serialize(&serde_wasm_bindgen::Serializer::json_compatible())
}

/// A reference-counted pointer to a function.
#[wasm_bindgen]
pub struct Func(Rc<rose::Def<rose::Function>>);

/// A function under construction.
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

// Debugging
#[cfg(feature = "debug")]
#[wasm_bindgen(js_name = "js2Rust")]
pub fn js_to_rust(Func(f): &Func) -> String {
    let def: &rose::Def<rose::Function> = f;
    let func: &rose::Function = &def.def;
    format!("{:#?}", func)
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
    pub fn int(&mut self, val: u32) {
        self.body.push(rose::Instr::Int { val });
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

    #[wasm_bindgen(js_name = "negInt")]
    pub fn neg_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::NegInt,
        });
    }

    #[wasm_bindgen(js_name = "absInt")]
    pub fn abs_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::AbsInt,
        });
    }

    #[wasm_bindgen(js_name = "negReal")]
    pub fn neg_real(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::NegReal,
        });
    }

    #[wasm_bindgen(js_name = "absReal")]
    pub fn abs_real(&mut self) {
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

    #[wasm_bindgen(js_name = "sumInt")]
    pub fn sum_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::SumInt,
        });
    }

    #[wasm_bindgen(js_name = "prodInt")]
    pub fn prod_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::ProdInt,
        });
    }

    #[wasm_bindgen(js_name = "maxInt")]
    pub fn max_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::MaxInt,
        });
    }

    #[wasm_bindgen(js_name = "minInt")]
    pub fn min_int(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::MinInt,
        });
    }

    #[wasm_bindgen(js_name = "sumReal")]
    pub fn sum_real(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::SumReal,
        });
    }

    #[wasm_bindgen(js_name = "prodReal")]
    pub fn prod_real(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::ProdReal,
        });
    }

    #[wasm_bindgen(js_name = "maxReal")]
    pub fn max_real(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::MaxReal,
        });
    }

    #[wasm_bindgen(js_name = "minReal")]
    pub fn min_real(&mut self) {
        self.body.push(rose::Instr::Unary {
            op: rose::Unop::MinReal,
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

    #[wasm_bindgen(js_name = "neqInt")]
    pub fn neq_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::NeqInt,
        });
    }

    #[wasm_bindgen(js_name = "ltInt")]
    pub fn lt_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LtInt,
        });
    }

    #[wasm_bindgen(js_name = "leqInt")]
    pub fn leq_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LeqInt,
        });
    }

    #[wasm_bindgen(js_name = "eqInt")]
    pub fn eq_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::EqInt,
        });
    }

    #[wasm_bindgen(js_name = "gtInt")]
    pub fn gt_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GtInt,
        });
    }

    #[wasm_bindgen(js_name = "geqInt")]
    pub fn geq_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GeqInt,
        });
    }

    #[wasm_bindgen(js_name = "neqReal")]
    pub fn neq_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::NeqReal,
        });
    }

    #[wasm_bindgen(js_name = "ltReal")]
    pub fn lt_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LtReal,
        });
    }

    #[wasm_bindgen(js_name = "leqReal")]
    pub fn leq_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::LeqReal,
        });
    }

    #[wasm_bindgen(js_name = "eqReal")]
    pub fn eq_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::EqReal,
        });
    }

    #[wasm_bindgen(js_name = "gtReal")]
    pub fn gt_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GtReal,
        });
    }

    #[wasm_bindgen(js_name = "geqReal")]
    pub fn geq_real(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::GeqReal,
        });
    }

    #[wasm_bindgen(js_name = "addInt")]
    pub fn add_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::AddInt,
        });
    }

    #[wasm_bindgen(js_name = "subInt")]
    pub fn sub_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::SubInt,
        });
    }

    #[wasm_bindgen(js_name = "mulInt")]
    pub fn mul_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::MulInt,
        });
    }

    #[wasm_bindgen(js_name = "divInt")]
    pub fn div_int(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::DivInt,
        });
    }

    #[wasm_bindgen(js_name = "mod")]
    pub fn modulus(&mut self) {
        self.body.push(rose::Instr::Binary {
            op: rose::Binop::Mod,
        });
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
