use enumset::EnumSet;
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

/// A reference-counted pointer to a function.
#[wasm_bindgen]
pub struct Func(Rc<rose::Function>);

#[cfg(feature = "debug")]
#[wasm_bindgen(js_name = "js2Rust")]
pub fn js_to_rust(Func(f): &Func) -> String {
    format!("{:#?}", f)
}

/// A function under construction.
#[wasm_bindgen]
pub struct Context {
    generics: Vec<EnumSet<rose::Constraint>>,
    types: Vec<rose::Typexpr>,
    funcs: Vec<rose::Func>,
    param: rose::Type,
    ret: rose::Type,
    vars: Vec<rose::Type>,
    blocks: Vec<rose::Block>,
}

#[wasm_bindgen]
pub fn bake(ctx: Context, main: usize) -> Func {
    let Context {
        generics,
        types,
        funcs,
        param,
        ret,
        vars,
        blocks,
    } = ctx;
    Func(Rc::new(rose::Function {
        generics,
        types,
        funcs,
        param,
        ret,
        vars,
        blocks,
        main: id::block(main),
    }))
}

/// A block under construction. Implicitly refers to the current `Context`.
#[wasm_bindgen]
pub struct Block {
    code: Vec<rose::Instr>,
}

#[wasm_bindgen]
impl Block {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { code: vec![] }
    }
}

// just to appease Clippy
impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
pub struct Body {
    ctx: Option<Context>,
    main: Option<Block>,
    pub arg: usize,
    args: Option<Vec<usize>>,
}

#[wasm_bindgen]
impl Body {
    #[wasm_bindgen]
    pub fn ctx(&mut self) -> Result<Context, JsError> {
        self.ctx
            .take()
            .ok_or_else(|| JsError::new("context already taken"))
    }

    #[wasm_bindgen]
    pub fn main(&mut self) -> Result<Block, JsError> {
        self.main
            .take()
            .ok_or_else(|| JsError::new("block already taken"))
    }

    #[wasm_bindgen]
    pub fn args(&mut self) -> Result<Vec<usize>, JsError> {
        self.args
            .take()
            .ok_or_else(|| JsError::new("args already taken"))
    }
}

/// The `param_types` argument is Serde-converted to `Vec<rose::Type>`, and the `ret_type`
/// argument is Serde-converted to `rose::Type`.
///
/// TODO: currently no support for
/// - non-primitive types
/// - calling other functions
#[wasm_bindgen]
pub fn make(generics: usize, param_types: JsValue, ret_type: JsValue) -> Result<Body, JsError> {
    let params: Vec<rose::Type> = serde_wasm_bindgen::from_value(param_types)?;
    let ret: rose::Type = serde_wasm_bindgen::from_value(ret_type)?;

    let param = rose::Type::Expr { id: id::typexpr(0) };
    let mut ctx = Context {
        generics: vec![EnumSet::only(rose::Constraint::Index); generics],
        types: vec![], // we populate this further down
        funcs: vec![],
        param,
        ret,
        vars: vec![],
        blocks: vec![],
    };

    let arg = ctx.var(param);
    let mut main = Block { code: vec![] };
    let args = params
        .iter()
        .enumerate()
        .map(|(i, &t)| {
            let expr = rose::Expr::Member {
                tuple: arg,
                member: id::member(i),
            };
            ctx.instr(&mut main, t, expr)
        })
        .collect();
    ctx.types.push(rose::Typexpr::Tuple { members: params });

    Ok(Body {
        ctx: Some(ctx),
        main: Some(main),
        arg: arg.var(),
        args: Some(args),
    })
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen]
    pub fn block(&mut self, b: Block, arg_id: usize, ret_id: usize) -> usize {
        let Block { code } = b;
        let id = self.blocks.len();
        self.blocks.push(rose::Block {
            arg: id::var(arg_id),
            code,
            ret: id::var(ret_id),
        });
        id
    }

    fn get(&self, var: id::Var) -> &rose::Type {
        &self.vars[var.var()]
    }

    fn var(&mut self, t: rose::Type) -> id::Var {
        let id = self.vars.len();
        self.vars.push(t);
        id::var(id)
    }

    fn typexpr(&mut self, t: rose::Typexpr) -> id::Typexpr {
        let id = self.types.len();
        self.types.push(t);
        id::typexpr(id)
    }

    // for `If`
    #[wasm_bindgen(js_name = "varUnit")]
    pub fn var_unit(&mut self) -> usize {
        self.var(rose::Type::Unit).var()
    }

    // for `For`
    #[wasm_bindgen(js_name = "varFin")]
    pub fn var_fin(&mut self, size: usize) -> usize {
        self.var(rose::Type::Fin { size }).var()
    }

    // for `For`
    #[wasm_bindgen(js_name = "varGeneric")]
    pub fn var_generic(&mut self, id: usize) -> usize {
        self.var(rose::Type::Generic {
            id: id::generic(id),
        })
        .var()
    }

    fn instr(&mut self, b: &mut Block, t: rose::Type, expr: rose::Expr) -> usize {
        let var = self.var(t);
        b.code.push(rose::Instr { var, expr });
        var.var()
    }

    #[wasm_bindgen]
    pub fn unit(&mut self, b: &mut Block) -> usize {
        self.instr(b, rose::Type::Unit, rose::Expr::Unit)
    }

    #[wasm_bindgen]
    pub fn bool(&mut self, b: &mut Block, val: bool) -> usize {
        self.instr(b, rose::Type::Bool, rose::Expr::Bool { val })
    }

    #[wasm_bindgen]
    pub fn f64(&mut self, b: &mut Block, val: f64) -> usize {
        self.instr(b, rose::Type::F64, rose::Expr::F64 { val })
    }

    #[wasm_bindgen]
    pub fn fin(&mut self, b: &mut Block, size: usize, val: usize) -> usize {
        self.instr(b, rose::Type::Fin { size }, rose::Expr::Fin { val })
    }

    // TODO: support `Expr::Array` and `Expr::Tuple`

    #[wasm_bindgen]
    pub fn index(&mut self, b: &mut Block, arr: usize, idx: usize) -> Result<usize, JsError> {
        let array = id::var(arr);
        let index = id::var(idx);
        let &t = match self.get(array) {
            rose::Type::Expr { id } => match &self.types[id.typexpr()] {
                rose::Typexpr::Array { index: _, elem } => Some(elem),
                rose::Typexpr::Def { def: _, params: _ } => todo!(),
                rose::Typexpr::Ref { .. } | rose::Typexpr::Tuple { .. } => None,
            },
            _ => None,
        }
        .ok_or_else(|| JsError::new("not an array"))?;
        Ok(self.instr(b, t, rose::Expr::Index { array, index }))
    }

    #[wasm_bindgen]
    pub fn member(&mut self, b: &mut Block, tup: usize, mem: usize) -> Result<usize, JsError> {
        let tuple = id::var(tup);
        let member = id::member(mem);
        let t = match self.get(tuple) {
            rose::Type::Expr { id } => match &self.types[id.typexpr()] {
                rose::Typexpr::Tuple { members } => Some(members[mem]),
                rose::Typexpr::Def { def: _, params: _ } => todo!(),
                rose::Typexpr::Ref { .. } | rose::Typexpr::Array { .. } => None,
            },
            _ => None,
        }
        .ok_or_else(|| JsError::new("not a tuple"))?;
        Ok(self.instr(b, t, rose::Expr::Member { tuple, member }))
    }

    // no method for `Expr::Slice` here, because we don't currently expose mutation to JS

    // unary

    #[wasm_bindgen]
    pub fn not(&mut self, b: &mut Block, arg: usize) -> usize {
        let expr = rose::Expr::Unary {
            op: rose::Unop::Not,
            arg: id::var(arg),
        };
        self.instr(b, rose::Type::Bool, expr)
    }

    #[wasm_bindgen]
    pub fn neg(&mut self, b: &mut Block, arg: usize) -> usize {
        let expr = rose::Expr::Unary {
            op: rose::Unop::Neg,
            arg: id::var(arg),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn abs(&mut self, b: &mut Block, arg: usize) -> usize {
        let expr = rose::Expr::Unary {
            op: rose::Unop::Abs,
            arg: id::var(arg),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn sqrt(&mut self, b: &mut Block, arg: usize) -> usize {
        let expr = rose::Expr::Unary {
            op: rose::Unop::Sqrt,
            arg: id::var(arg),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    // end of unary

    // binary

    #[wasm_bindgen]
    pub fn and(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::And,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::Bool, expr)
    }

    #[wasm_bindgen]
    pub fn or(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Or,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::Bool, expr)
    }

    #[wasm_bindgen]
    pub fn iff(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Iff,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::Bool, expr)
    }

    #[wasm_bindgen]
    pub fn xor(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Xor,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::Bool, expr)
    }

    #[wasm_bindgen]
    pub fn neq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Neq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn lt(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Lt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn leq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Leq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn eq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Eq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn gt(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Gt,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn geq(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Geq,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn add(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Add,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn sub(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Sub,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn mul(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Mul,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    #[wasm_bindgen]
    pub fn div(&mut self, b: &mut Block, left: usize, right: usize) -> usize {
        let expr = rose::Expr::Binary {
            op: rose::Binop::Div,
            left: id::var(left),
            right: id::var(right),
        };
        self.instr(b, rose::Type::F64, expr)
    }

    // end of binary

    /// `rose::Expr::If`
    #[wasm_bindgen]
    pub fn cond(&mut self, b: &mut Block, cond: usize, then: usize, els: usize) -> usize {
        let &t = self.get(self.blocks[then].ret); // arbitrary; could have used `els` instead
        let expr = rose::Expr::If {
            cond: id::var(cond),
            then: id::block(then),
            els: id::block(els),
        };
        self.instr(b, t, expr)
    }

    // `rose::Expr::For`
    fn arr(&mut self, b: &mut Block, index: rose::Type, body: usize) -> usize {
        let rose::Block { arg, ret, .. } = self.blocks[body];
        let t = self.typexpr(rose::Typexpr::Array {
            index: *self.get(arg),
            elem: *self.get(ret),
        });
        let expr = rose::Expr::For {
            index,
            body: id::block(body),
        };
        self.instr(b, rose::Type::Expr { id: t }, expr)
    }

    #[wasm_bindgen(js_name = "forFin")]
    pub fn for_fin(&mut self, b: &mut Block, size: usize, body: usize) -> usize {
        self.arr(b, rose::Type::Fin { size }, body)
    }

    #[wasm_bindgen(js_name = "forGeneric")]
    pub fn for_generic(&mut self, b: &mut Block, id: usize, body: usize) -> usize {
        let index = rose::Type::Generic {
            id: id::generic(id),
        };
        self.arr(b, index, body)
    }
}

/// Interpret a function with the given arguments.
///
/// The `generics` are Serde-converted to `Vec<rose_interp::Typeval>`, the `arg` is Serde-converted
/// to `rose_interp::Val`, and the return value is Serde-converted from `rose_interp::Val`.
#[wasm_bindgen]
pub fn interp(Func(f): &Func, generics: JsValue, arg: JsValue) -> Result<JsValue, JsError> {
    let types: Vec<rose_interp::Typeval> = serde_wasm_bindgen::from_value(generics)?;
    let val: rose_interp::Val = serde_wasm_bindgen::from_value(arg)?;
    let ret = rose_interp::interp(f, &types, val)?;
    Ok(to_js_value(&ret)?)
}
