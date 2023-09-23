use by_address::ByAddress;
use indexmap::{IndexMap, IndexSet};
use rose::{id, Binop, Expr, Func, Instr, Node, Refs, Ty, Unop};
use std::hash::Hash;
use wasm_encoder::{
    CodeSection, EntityType, ExportSection, Function, FunctionSection, ImportSection, Instruction,
    Module, TypeSection, ValType,
};

fn resolve(typemap: &mut IndexSet<Ty>, generics: &[id::Ty], types: &[id::Ty], ty: &Ty) -> id::Ty {
    let resolved = match ty {
        Ty::Generic { id } => return generics[id.generic()],

        Ty::Unit => Ty::Unit,
        Ty::Bool => Ty::Bool,
        Ty::F64 => Ty::F64,
        &Ty::Fin { size } => Ty::Fin { size },

        Ty::Ref { inner } => Ty::Ref {
            inner: types[inner.ty()],
        },
        Ty::Array { index, elem } => Ty::Array {
            index: types[index.ty()],
            elem: types[elem.ty()],
        },
        Ty::Tuple { members } => Ty::Tuple {
            members: members.iter().map(|&x| types[x.ty()]).collect(),
        },
    };
    let (i, _) = typemap.insert_full(resolved);
    id::ty(i)
}

type Imports<O> = IndexMap<(O, Box<[id::Ty]>), (Box<[id::Ty]>, id::Ty)>;
type Funcs<'a, T> = IndexMap<(ByAddress<&'a Func>, Box<[id::Ty]>), (T, Box<[id::Ty]>)>;

struct Topsort<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    types: IndexSet<Ty>,
    imports: Imports<O>,
    funcs: Funcs<'a, T>,
}

impl<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> Topsort<'a, O, T> {
    fn block(&mut self, refs: &T, f: &'a Func, types: &[id::Ty], block: &[Instr]) {
        for instr in block.iter() {
            match &instr.expr {
                Expr::Unit
                | Expr::Bool { .. }
                | Expr::F64 { .. }
                | Expr::Fin { .. }
                | Expr::Array { .. }
                | Expr::Tuple { .. }
                | Expr::Index { .. }
                | Expr::Member { .. }
                | Expr::Slice { .. }
                | Expr::Field { .. }
                | Expr::Unary { .. }
                | Expr::Binary { .. }
                | Expr::Select { .. }
                | Expr::Accum { .. }
                | Expr::Add { .. }
                | Expr::Resolve { .. } => {}
                Expr::For { body, .. } => {
                    self.block(refs, f, types, body);
                }
                Expr::Call { id, generics, args } => {
                    let gens = generics.iter().map(|t| types[t.ty()]).collect();
                    match refs.get(*id).unwrap() {
                        Node::Transparent { refs, def } => {
                            let key = (ByAddress(def), gens);
                            if !self.funcs.contains_key(&key) {
                                let (_, gens) = key;
                                self.func(refs, def, gens);
                            }
                        }
                        Node::Opaque { def, .. } => {
                            self.imports.insert(
                                (def, gens),
                                (
                                    args.iter().map(|x| types[f.vars[x.var()].ty()]).collect(),
                                    types[f.vars[instr.var.var()].ty()],
                                ),
                            );
                        }
                    }
                }
            }
        }
    }

    fn func(&mut self, refs: T, def: &'a Func, generics: Box<[id::Ty]>) {
        let mut types = vec![];
        for ty in def.types.iter() {
            types.push(resolve(&mut self.types, &generics, &types, ty));
        }
        self.block(&refs, def, &types, &def.body);
        self.funcs
            .insert((ByAddress(def), generics), (refs, types.into()));
    }
}

fn val_type(ty: &Ty) -> ValType {
    match ty {
        Ty::Unit | Ty::Bool | Ty::Fin { .. } => ValType::I32,
        Ty::F64 => ValType::F64,
        Ty::Generic { .. } => unreachable!(),
        Ty::Ref { inner: _ } => todo!(),
        Ty::Array { index: _, elem: _ } => todo!(),
        Ty::Tuple { members: _ } => todo!(),
    }
}

struct Codegen<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    imports: &'b Imports<O>,
    funcs: &'b Funcs<'a, T>,
    refs: &'b T,
    def: &'b Func,
    types: &'b [id::Ty],
    locals: &'b [u32],
    wasm: Function,
}

impl<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> Codegen<'a, '_, O, T> {
    fn get(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalGet(self.locals[x.var()]));
    }

    fn block(&mut self, block: &[Instr]) {
        for instr in block.iter() {
            match &instr.expr {
                Expr::Unit => {
                    self.wasm.instruction(&Instruction::I32Const(0));
                }
                &Expr::Bool { val } => {
                    self.wasm
                        .instruction(&Instruction::I32Const(if val { 1 } else { 0 }));
                }
                &Expr::F64 { val } => {
                    self.wasm.instruction(&Instruction::F64Const(val));
                }
                &Expr::Fin { val } => {
                    self.wasm
                        .instruction(&Instruction::I32Const(val.try_into().unwrap()));
                }
                Expr::Array { elems: _ } => todo!(),
                Expr::Tuple { members: _ } => todo!(),
                &Expr::Index { array: _, index: _ } => todo!(),
                &Expr::Member {
                    tuple: _,
                    member: _,
                } => todo!(),
                &Expr::Slice { array: _, index: _ } => todo!(),
                &Expr::Field {
                    tuple: _,
                    member: _,
                } => todo!(),
                &Expr::Unary { op, arg } => {
                    self.get(arg);
                    match op {
                        Unop::Not => todo!(),
                        Unop::Neg => self.wasm.instruction(&Instruction::F64Neg),
                        Unop::Abs => self.wasm.instruction(&Instruction::F64Abs),
                        Unop::Sign => todo!(),
                        Unop::Ceil => self.wasm.instruction(&Instruction::F64Ceil),
                        Unop::Floor => self.wasm.instruction(&Instruction::F64Floor),
                        Unop::Trunc => self.wasm.instruction(&Instruction::F64Trunc),
                        Unop::Sqrt => self.wasm.instruction(&Instruction::F64Sqrt),
                    };
                }
                &Expr::Binary { op, left, right } => {
                    self.get(left);
                    self.get(right);
                    match op {
                        Binop::And => todo!(),
                        Binop::Or => todo!(),
                        Binop::Iff => todo!(),
                        Binop::Xor => todo!(),
                        Binop::Neq => self.wasm.instruction(&Instruction::F64Ne),
                        Binop::Lt => self.wasm.instruction(&Instruction::F64Lt),
                        Binop::Leq => self.wasm.instruction(&Instruction::F64Le),
                        Binop::Eq => self.wasm.instruction(&Instruction::F64Eq),
                        Binop::Gt => self.wasm.instruction(&Instruction::F64Gt),
                        Binop::Geq => self.wasm.instruction(&Instruction::F64Ge),
                        Binop::Add => self.wasm.instruction(&Instruction::F64Add),
                        Binop::Sub => self.wasm.instruction(&Instruction::F64Sub),
                        Binop::Mul => self.wasm.instruction(&Instruction::F64Mul),
                        Binop::Div => self.wasm.instruction(&Instruction::F64Div),
                    };
                }
                &Expr::Select {
                    cond: _,
                    then: _,
                    els: _,
                } => todo!(),
                Expr::Call { id, generics, args } => {
                    let gens = generics
                        .iter()
                        .map(|t| self.types[self.def.vars[t.ty()].ty()])
                        .collect();
                    let i = match self.refs.get(*id).unwrap() {
                        Node::Transparent { def, .. } => {
                            self.funcs.get_index_of(&(ByAddress(def), gens))
                        }
                        Node::Opaque { def, .. } => self.imports.get_index_of(&(def, gens)),
                    };
                    for &arg in args.iter() {
                        self.get(arg);
                    }
                    self.wasm
                        .instruction(&Instruction::Call(i.unwrap().try_into().unwrap()));
                }
                Expr::For {
                    arg: _,
                    body: _,
                    ret: _,
                } => todo!(),
                &Expr::Accum { shape: _ } => todo!(),
                &Expr::Add {
                    accum: _,
                    addend: _,
                } => todo!(),
                &Expr::Resolve { var: _ } => todo!(),
            }
            self.wasm
                .instruction(&Instruction::LocalSet(self.locals[instr.var.var()]));
        }
    }
}

pub struct Wasm<O> {
    pub bytes: Vec<u8>,
    pub imports: Imports<O>,
}

pub fn compile<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>>(f: Node<'a, O, T>) -> Wasm<O> {
    let mut topsort = Topsort {
        types: IndexSet::new(),
        imports: IndexMap::new(),
        funcs: IndexMap::new(),
    };
    match f {
        Node::Transparent { refs, def } => {
            topsort.func(refs, def, [].into());
        }
        Node::Opaque {
            types,
            params,
            ret,
            def,
            ..
        } => {
            let mut def_types = vec![];
            for ty in types.iter() {
                def_types.push(resolve(&mut topsort.types, &[], &def_types, ty));
            }
            topsort.imports.insert(
                (def, [].into()),
                (
                    params.iter().map(|t| def_types[t.ty()]).collect(),
                    def_types[ret.ty()],
                ),
            );
        }
    }
    let Topsort {
        types,
        imports,
        funcs,
    } = topsort;

    let mut func_types: IndexSet<(Box<[ValType]>, ValType)> = IndexSet::new();

    let mut import_section = ImportSection::new();
    for (i, (params, ret)) in imports.values().enumerate() {
        let (type_index, _) = func_types.insert_full((
            params.iter().map(|t| val_type(&types[t.ty()])).collect(),
            val_type(&types[ret.ty()]),
        ));
        import_section.import(
            "",
            &i.to_string(),
            EntityType::Function(type_index.try_into().unwrap()),
        );
    }

    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();
    for ((def, _), (refs, def_types)) in funcs.iter() {
        let vt = |t: id::Ty| val_type(&types[def_types[t.ty()].ty()]);
        let params: u32 = def.params.len().try_into().unwrap();
        let mut locals = vec![None; def.vars.len()];

        let (type_index, _) = func_types.insert_full((
            def.params
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    locals[param.var()] = Some(i.try_into().unwrap());
                    vt(def.vars[param.var()])
                })
                .collect(),
            vt(def.vars[def.ret.var()]),
        ));
        function_section.function(type_index.try_into().unwrap());

        let mut i32s = 0;
        for (i, &t) in def.vars.iter().enumerate() {
            if locals[i].is_none() {
                if let ValType::I32 = vt(t) {
                    locals[i] = Some(params + i32s);
                    i32s += 1;
                }
            }
        }
        let mut f64s = 0;
        for (i, &t) in def.vars.iter().enumerate() {
            if locals[i].is_none() {
                assert_eq!(vt(t), ValType::F64);
                locals[i] = Some(params + i32s + f64s);
                f64s += 1;
            }
        }

        let locals = locals.into_iter().map(Option::unwrap).collect::<Box<_>>();
        let mut codegen = Codegen {
            imports: &imports,
            funcs: &funcs,
            refs,
            def,
            types: def_types,
            locals: &locals,
            wasm: Function::new([(i32s, ValType::I32), (f64s, ValType::F64)]),
        };
        codegen.block(&def.body);
        codegen.get(def.ret);
        codegen.wasm.instruction(&Instruction::End);
        code_section.function(&codegen.wasm);
    }

    let mut type_section = TypeSection::new();
    for (params, ret) in func_types {
        type_section.function(params.into_vec(), [ret]);
    }

    let mut export_section = ExportSection::new();
    export_section.export(
        "",
        wasm_encoder::ExportKind::Func,
        (imports.len() + funcs.len() - 1).try_into().unwrap(),
    );

    let mut module = Module::new();
    module.section(&type_section);
    module.section(&import_section);
    module.section(&function_section);
    module.section(&export_section);
    module.section(&code_section);
    Wasm {
        bytes: module.finish(),
        imports,
    }
}
