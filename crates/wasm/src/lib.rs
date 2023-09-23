use by_address::ByAddress;
use indexmap::{IndexMap, IndexSet};
use rose::{id, Binop, Expr, Func, Instr, Node, Refs, Ty, Unop};
use std::hash::Hash;
use wasm_encoder::{
    CodeSection, EntityType, ExportSection, Function, FunctionSection, ImportSection, Instruction,
    MemArg, MemorySection, MemoryType, Module, TypeSection, ValType,
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
                            // TODO: check that the value matches if the key is already present
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
        let prev = self
            .funcs
            .insert((ByAddress(def), generics), (refs, types.into()));
        assert!(prev.is_none());
    }
}

fn val_type(ty: &Ty) -> ValType {
    match ty {
        Ty::Unit
        | Ty::Bool
        | Ty::Fin { .. }
        | Ty::Ref { .. }
        | Ty::Array { .. }
        | Ty::Tuple { .. } => ValType::I32,
        Ty::F64 => ValType::F64,
        Ty::Generic { .. } => unreachable!(),
    }
}

type Size = u32;

fn u_size(x: usize) -> Size {
    x.try_into().unwrap()
}

fn align(size: Size, align: Size) -> Size {
    (size + align - 1) & !(align - 1)
}

#[derive(Clone, Copy)]
enum Layout {
    Unit,
    U8,
    U16,
    U32,
    F64,
    Ref,
}

impl Layout {
    fn size(self) -> Size {
        match self {
            Layout::Unit => 0,
            Layout::U8 => 1,
            Layout::U16 => 2,
            Layout::U32 | Layout::Ref => 4,
            Layout::F64 => 8,
        }
    }
}

type Local = u32;

struct Codegen<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    layouts: &'b [Layout],
    imports: &'b Imports<O>,
    funcs: &'b Funcs<'a, T>,
    costs: &'b [Size],
    refs: &'b T,
    def: &'b Func,
    types: &'b [id::Ty],
    locals: &'b [Local],
    offset: Size,
    wasm: Function,
}

impl<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> Codegen<'a, '_, O, T> {
    fn layout(&self, t: id::Ty) -> Layout {
        self.layouts[self.types[t.ty()].ty()]
    }

    fn get(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalGet(self.locals[x.var()]));
    }

    fn pointer(&mut self) {
        self.wasm
            .instruction(&Instruction::LocalGet(u_size(self.def.params.len())));
    }

    fn u32_const(&mut self, x: u32) {
        self.wasm.instruction(&Instruction::I32Const(x as i32));
    }

    fn bump(&mut self, size: Size) {
        let aligned = align(size, 8);
        self.pointer();
        self.u32_const(aligned);
        self.wasm.instruction(&Instruction::I32Add);
        self.wasm
            .instruction(&Instruction::LocalSet(u_size(self.def.params.len())));
        self.offset += aligned;
    }

    fn load(&mut self, layout: Layout, offset: Size) {
        let offset = offset.into();
        match layout {
            Layout::Unit => {
                self.wasm.instruction(&Instruction::Drop);
                self.wasm.instruction(&Instruction::I32Const(0));
            }
            Layout::U8 => {
                self.wasm.instruction(&Instruction::I32Load8U(MemArg {
                    offset,
                    align: 0,
                    memory_index: 0,
                }));
            }
            Layout::U16 => {
                self.wasm.instruction(&Instruction::I32Load16U(MemArg {
                    offset,
                    align: 1,
                    memory_index: 0,
                }));
            }
            Layout::U32 => {
                self.wasm.instruction(&Instruction::I32Load(MemArg {
                    offset,
                    align: 2,
                    memory_index: 0,
                }));
            }
            Layout::F64 => {
                self.wasm.instruction(&Instruction::F64Load(MemArg {
                    offset,
                    align: 3,
                    memory_index: 0,
                }));
            }
            Layout::Ref => unreachable!(),
        }
    }

    fn store(&mut self, layout: Layout, offset: Size) {
        let offset = offset.into();
        match layout {
            Layout::Unit => {
                self.wasm.instruction(&Instruction::Drop);
                self.wasm.instruction(&Instruction::Drop);
            }
            Layout::U8 => {
                self.wasm.instruction(&Instruction::I32Store8(MemArg {
                    offset,
                    align: 0,
                    memory_index: 0,
                }));
            }
            Layout::U16 => {
                self.wasm.instruction(&Instruction::I32Store16(MemArg {
                    offset,
                    align: 1,
                    memory_index: 0,
                }));
            }
            Layout::U32 => {
                self.wasm.instruction(&Instruction::I32Store(MemArg {
                    offset,
                    align: 2,
                    memory_index: 0,
                }));
            }
            Layout::F64 => {
                self.wasm.instruction(&Instruction::F64Store(MemArg {
                    offset,
                    align: 3,
                    memory_index: 0,
                }));
            }
            Layout::Ref => unreachable!(),
        }
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
                Expr::Array { elems } => {
                    let layout =
                        self.layout(match self.def.types[self.def.vars[instr.var.var()].ty()] {
                            Ty::Array { elem, .. } => elem,
                            _ => unreachable!(),
                        });
                    let size = layout.size();
                    for (i, &elem) in elems.iter().enumerate() {
                        self.pointer();
                        self.get(elem);
                        self.store(layout, size * u_size(i));
                    }
                    self.pointer();
                    self.bump(size * u_size(elems.len()));
                }
                Expr::Tuple { members: _ } => todo!(),
                &Expr::Index { array, index } => {
                    let layout = self.layout(self.def.vars[instr.var.var()]);
                    let size = layout.size();
                    self.get(array);
                    self.get(index);
                    self.u32_const(size);
                    self.wasm.instruction(&Instruction::I32Mul);
                    self.wasm.instruction(&Instruction::I32Add);
                    self.load(layout, 0);
                }
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
                    for &arg in args.iter() {
                        self.get(arg);
                    }
                    let i = match self.refs.get(*id).unwrap() {
                        Node::Transparent { def, .. } => {
                            self.pointer();
                            let j = self.funcs.get_index_of(&(ByAddress(def), gens)).unwrap();
                            self.bump(self.costs[j]);
                            self.imports.len() + j
                        }
                        Node::Opaque { def, .. } => {
                            self.imports.get_index_of(&(def, gens)).unwrap()
                        }
                    };
                    self.wasm
                        .instruction(&Instruction::Call(i.try_into().unwrap()));
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

    let layouts: Box<_> = types
        .iter()
        .map(|ty| match ty {
            Ty::Unit => Layout::Unit,
            Ty::Bool => Layout::U8,
            Ty::F64 => Layout::F64,
            &Ty::Fin { size } => {
                if size <= 256 {
                    Layout::U8
                } else if size <= 65536 {
                    Layout::U16
                } else {
                    Layout::U32
                }
            }
            Ty::Generic { .. } => unreachable!(),
            Ty::Ref { .. } => Layout::Ref,
            Ty::Array { .. } | Ty::Tuple { .. } => Layout::U32,
        })
        .collect();

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

    let mut costs = vec![];

    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();
    for ((def, _), (refs, def_types)) in funcs.iter() {
        let vt = |t: id::Ty| val_type(&types[def_types[t.ty()].ty()]);
        let params: Local = (def.params.len() + 1).try_into().unwrap();
        let mut locals = vec![None; def.vars.len()];

        let (type_index, _) = func_types.insert_full((
            def.params
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    locals[param.var()] = Some(i.try_into().unwrap());
                    vt(def.vars[param.var()])
                })
                .chain([ValType::I32])
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
            layouts: &layouts,
            imports: &imports,
            funcs: &funcs,
            costs: &costs,
            refs,
            def,
            types: def_types,
            locals: &locals,
            offset: 0,
            wasm: Function::new([(i32s, ValType::I32), (f64s, ValType::F64)]),
        };
        codegen.block(&def.body);
        codegen.get(def.ret);
        codegen.wasm.instruction(&Instruction::End);
        code_section.function(&codegen.wasm);
        costs.push(codegen.offset);
    }

    let mut type_section = TypeSection::new();
    for (params, ret) in func_types {
        type_section.function(params.into_vec(), [ret]);
    }

    let mut memory_section = MemorySection::new();
    memory_section.memory(MemoryType {
        // TODO: calculate the number of memory pages
        minimum: 1,
        maximum: Some(1),
        memory64: false,
        shared: false,
    });

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
    module.section(&memory_section);
    module.section(&export_section);
    module.section(&code_section);
    Wasm {
        bytes: module.finish(),
        imports,
    }
}
