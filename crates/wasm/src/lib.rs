use by_address::ByAddress;
use indexmap::{map::Entry, IndexMap, IndexSet};
use rose::{id, Binop, Expr, Func, Instr, Node, Refs, Ty, Unop};
use std::{hash::Hash, mem::take};
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemArg, MemorySection, MemoryType, Module, TypeSection, ValType,
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
                            let resolved = (
                                args.iter().map(|x| types[f.vars[x.var()].ty()]).collect(),
                                types[f.vars[instr.var.var()].ty()],
                            );
                            match self.imports.entry((def, gens)) {
                                Entry::Occupied(entry) => {
                                    assert_eq!(entry.get(), &resolved);
                                }
                                Entry::Vacant(entry) => {
                                    entry.insert(resolved);
                                }
                            }
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

// TODO: is this function overused?
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
    fn size_align(self) -> (Size, Size) {
        match self {
            Self::Unit => (0, 1),
            Self::U8 => (1, 1),
            Self::U16 => (2, 2),
            Self::U32 => (4, 4),
            Self::F64 => (8, 8),
            Self::Ref => unreachable!(),
        }
    }

    fn aligned(self) -> Size {
        let (s, a) = self.size_align();
        align(s, a)
    }

    fn load(self, function: &mut Function, offset: Size) {
        let offset = offset.into();
        match self {
            Self::Unit => {
                function.instruction(&Instruction::Drop);
                function.instruction(&Instruction::I32Const(0));
            }
            Self::U8 => {
                function.instruction(&Instruction::I32Load8U(MemArg {
                    offset,
                    align: 0,
                    memory_index: 0,
                }));
            }
            Self::U16 => {
                function.instruction(&Instruction::I32Load16U(MemArg {
                    offset,
                    align: 1,
                    memory_index: 0,
                }));
            }
            Self::U32 => {
                function.instruction(&Instruction::I32Load(MemArg {
                    offset,
                    align: 2,
                    memory_index: 0,
                }));
            }
            Self::F64 => {
                function.instruction(&Instruction::F64Load(MemArg {
                    offset,
                    align: 3,
                    memory_index: 0,
                }));
            }
            Self::Ref => unreachable!(),
        }
    }

    fn store(self, function: &mut Function, offset: Size) {
        let offset = offset.into();
        match self {
            Self::Unit => {
                function.instruction(&Instruction::Drop);
                function.instruction(&Instruction::Drop);
            }
            Self::U8 => {
                function.instruction(&Instruction::I32Store8(MemArg {
                    offset,
                    align: 0,
                    memory_index: 0,
                }));
            }
            Self::U16 => {
                function.instruction(&Instruction::I32Store16(MemArg {
                    offset,
                    align: 1,
                    memory_index: 0,
                }));
            }
            Self::U32 => {
                function.instruction(&Instruction::I32Store(MemArg {
                    offset,
                    align: 2,
                    memory_index: 0,
                }));
            }
            Self::F64 => {
                function.instruction(&Instruction::F64Store(MemArg {
                    offset,
                    align: 3,
                    memory_index: 0,
                }));
            }
            Self::Ref => unreachable!(),
        }
    }
}

type Local = u32;

#[derive(Clone, Copy)]
struct Accum {
    /// The ID of the zero function.
    zero: u32,

    /// The allocation cost of the zero function.
    cost: Size,

    // The ID of the add function, which has no allocation cost.
    add: u32,
}

struct Meta {
    ty: Ty,
    layout: Layout,
    accum: Option<Accum>,
    members: Option<Box<[Size]>>,
}

struct Codegen<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    metas: &'b [Meta],
    imports: &'b Imports<O>,
    extras: usize,
    funcs: &'b Funcs<'a, T>,
    costs: &'b [Size],
    refs: &'b T,
    def: &'b Func,
    types: &'b [id::Ty],
    locals: &'b [Local],
    offset: Size,
    wasm: Function,
}

impl<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> Codegen<'a, 'b, O, T> {
    fn meta(&self, t: id::Ty) -> &'b Meta {
        &self.metas[self.types[t.ty()].ty()]
    }

    fn get(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalGet(self.locals[x.var()]));
    }

    fn set(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalSet(self.locals[x.var()]));
    }

    fn tee(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalTee(self.locals[x.var()]));
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
        layout.load(&mut self.wasm, offset)
    }

    fn store(&mut self, layout: Layout, offset: Size) {
        layout.store(&mut self.wasm, offset)
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
                    let &Meta { layout, .. } =
                        self.meta(match self.def.types[self.def.vars[instr.var.var()].ty()] {
                            Ty::Array { elem, .. } => elem,
                            _ => unreachable!(),
                        });
                    let size = layout.aligned();
                    for (i, &elem) in elems.iter().enumerate() {
                        self.pointer();
                        self.get(elem);
                        self.store(layout, size * u_size(i));
                    }
                    self.pointer();
                    self.bump(size * u_size(elems.len()));
                }
                Expr::Tuple { members } => {
                    let mut size = 0;
                    let Meta { members: mems, .. } = self.meta(self.def.vars[instr.var.var()]);
                    for (&member, &offset) in members.iter().zip(mems.as_ref().unwrap().iter()) {
                        let &Meta { layout, .. } = self.meta(self.def.vars[member.var()]);
                        self.pointer();
                        self.get(member);
                        self.store(layout, offset);
                        size = size.max(offset + layout.aligned());
                    }
                    self.pointer();
                    self.bump(size);
                }
                &Expr::Index { array, index } => {
                    let &Meta { layout, .. } = self.meta(self.def.vars[instr.var.var()]);
                    let size = layout.aligned();
                    self.get(array);
                    self.get(index);
                    self.u32_const(size);
                    self.wasm.instruction(&Instruction::I32Mul);
                    self.wasm.instruction(&Instruction::I32Add);
                    self.load(layout, 0);
                }
                &Expr::Member { tuple, member } => {
                    let Meta { members, .. } = self.meta(self.def.vars[tuple.var()]);
                    let offset = members.as_ref().unwrap()[member.member()];
                    let &Meta { layout, .. } = self.meta(self.def.vars[instr.var.var()]);
                    self.get(tuple);
                    self.load(layout, offset);
                }
                &Expr::Slice { array, index } => {
                    let meta =
                        self.meta(match self.def.types[self.def.vars[instr.var.var()].ty()] {
                            Ty::Ref { inner } => inner,
                            _ => unreachable!(),
                        });
                    let size = meta.layout.aligned();
                    self.get(array);
                    self.get(index);
                    self.u32_const(size);
                    self.wasm.instruction(&Instruction::I32Mul);
                    self.wasm.instruction(&Instruction::I32Add);
                    if let Ty::Array { .. } | Ty::Tuple { .. } = &meta.ty {
                        self.load(meta.layout, 0);
                    }
                }
                &Expr::Field { tuple, member } => {
                    let Meta { members, .. } =
                        self.meta(match self.def.types[self.def.vars[tuple.var()].ty()] {
                            Ty::Ref { inner } => inner,
                            _ => unreachable!(),
                        });
                    let offset = members.as_ref().unwrap()[member.member()];
                    let meta =
                        self.meta(match self.def.types[self.def.vars[instr.var.var()].ty()] {
                            Ty::Ref { inner } => inner,
                            _ => unreachable!(),
                        });
                    self.get(tuple);
                    match &meta.ty {
                        Ty::Unit | Ty::Bool | Ty::F64 | Ty::Fin { .. } => {
                            self.u32_const(offset);
                            self.wasm.instruction(&Instruction::I32Add);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            self.load(meta.layout, offset);
                        }
                    }
                }
                &Expr::Unary { op, arg } => match op {
                    Unop::Not => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::I32Eqz);
                    }
                    Unop::Neg => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Neg);
                    }
                    Unop::Abs => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Abs);
                    }
                    Unop::Sign => {
                        self.wasm.instruction(&Instruction::F64Const(1.));
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Copysign);
                    }
                    Unop::Ceil => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Ceil);
                    }
                    Unop::Floor => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Floor);
                    }
                    Unop::Trunc => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Trunc);
                    }
                    Unop::Sqrt => {
                        self.get(arg);
                        self.wasm.instruction(&Instruction::F64Sqrt);
                    }
                },
                &Expr::Binary { op, left, right } => {
                    self.get(left);
                    self.get(right);
                    match op {
                        Binop::And => self.wasm.instruction(&Instruction::I32And),
                        Binop::Or => self.wasm.instruction(&Instruction::I32Or),
                        Binop::Iff => self.wasm.instruction(&Instruction::I32Eq),
                        Binop::Xor => self.wasm.instruction(&Instruction::I32Xor),
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
                &Expr::Select { cond, then, els } => {
                    self.get(then);
                    self.get(els);
                    self.get(cond);
                    self.wasm.instruction(&Instruction::Select);
                }
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
                            self.imports.len() + self.extras + j
                        }
                        Node::Opaque { def, .. } => {
                            self.imports.get_index_of(&(def, gens)).unwrap()
                        }
                    };
                    self.wasm
                        .instruction(&Instruction::Call(i.try_into().unwrap()));
                }
                Expr::For { arg, body, ret } => {
                    let n = u_size(match self.meta(self.def.vars[arg.var()]).ty {
                        Ty::Fin { size } => size,
                        _ => unreachable!(),
                    });
                    let &Meta { layout, .. } = self.meta(self.def.vars[ret.var()]);
                    let size = layout.aligned();

                    self.pointer();
                    self.set(instr.var);

                    if n > 0 {
                        self.bump(size * n);
                        let offset = take(&mut self.offset);

                        self.wasm.instruction(&Instruction::I32Const(0));
                        self.set(*arg);
                        self.wasm.instruction(&Instruction::Loop(BlockType::Empty));

                        self.block(body);

                        self.get(instr.var);
                        self.get(*arg);
                        self.u32_const(size);
                        self.wasm.instruction(&Instruction::I32Mul);
                        self.wasm.instruction(&Instruction::I32Add);
                        self.get(*ret);
                        self.store(layout, 0);

                        self.get(*arg);
                        self.wasm.instruction(&Instruction::I32Const(1));
                        self.wasm.instruction(&Instruction::I32Add);
                        self.tee(*arg);
                        self.u32_const(n);
                        self.wasm.instruction(&Instruction::I32LtU);
                        self.wasm.instruction(&Instruction::BrIf(0));
                        self.wasm.instruction(&Instruction::End);

                        self.offset = offset + self.offset * n;
                    }

                    continue;
                }
                &Expr::Accum { shape } => {
                    let meta = self.meta(self.def.vars[shape.var()]);
                    match &meta.ty {
                        Ty::Unit | Ty::Bool | Ty::Fin { .. } => self.get(shape),
                        Ty::F64 => {
                            self.pointer();
                            self.pointer();
                            self.wasm.instruction(&Instruction::F64Const(0.));
                            self.store(Layout::F64, 0);
                            self.bump(8);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            let Accum { zero, cost, .. } = meta.accum.unwrap();
                            self.pointer();
                            self.get(shape);
                            self.wasm.instruction(&Instruction::Call(zero));
                            self.pointer();
                            self.bump(cost);
                        }
                    }
                }
                &Expr::Add { accum, addend } => {
                    let meta = self.meta(self.def.vars[addend.var()]);
                    match &meta.ty {
                        Ty::Unit | Ty::Bool | Ty::Fin { .. } => {}
                        Ty::F64 => {
                            self.get(accum);
                            self.get(accum);
                            self.load(Layout::F64, 0);
                            self.get(addend);
                            self.wasm.instruction(&Instruction::F64Add);
                            self.store(Layout::F64, 0);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            self.get(accum);
                            self.get(addend);
                            self.wasm
                                .instruction(&Instruction::Call(meta.accum.unwrap().add));
                        }
                    }
                    self.wasm.instruction(&Instruction::I32Const(0));
                }
                &Expr::Resolve { var } => {
                    self.get(var);
                    if let Ty::F64 = &self.meta(self.def.vars[instr.var.var()]).ty {
                        self.load(Layout::F64, 0);
                    }
                }
            }
            self.set(instr.var);
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
            EntityType::Function((1 + type_index).try_into().unwrap()),
        );
    }

    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();

    let mut metas: Vec<Meta> = vec![];
    let mut extras: usize = 0;
    for ty in types.into_iter() {
        let (layout, cost, members) = match &ty {
            Ty::Unit => (Layout::Unit, None, None),
            Ty::Bool => (Layout::U8, None, None),
            Ty::F64 => (Layout::F64, None, None),
            &Ty::Fin { size } => (
                if size <= 1 {
                    Layout::Unit
                } else if size <= 256 {
                    Layout::U8
                } else if size <= 65536 {
                    Layout::U16
                } else {
                    Layout::U32
                },
                None,
                None,
            ),
            Ty::Generic { .. } => unreachable!(),
            Ty::Ref { .. } => (Layout::Ref, None, None),
            Ty::Array { index, elem } => {
                let n = u_size(match metas[index.ty()].ty {
                    Ty::Fin { size } => size,
                    _ => unreachable!(),
                });
                let meta = &metas[elem.ty()];
                let size = meta.layout.aligned();

                let mut zero = Function::new([(2, ValType::I32)]);
                let mut total = align(size * n, 8);
                let mut add = Function::new([(1, ValType::I32)]);

                if n > 0 {
                    zero.instruction(&Instruction::LocalGet(0));
                    zero.instruction(&Instruction::I32Const(total.try_into().unwrap()));
                    zero.instruction(&Instruction::I32Add);
                    zero.instruction(&Instruction::LocalTee(2));
                    zero.instruction(&Instruction::LocalSet(3));
                    zero.instruction(&Instruction::Loop(BlockType::Empty));

                    add.instruction(&Instruction::LocalGet(0));
                    add.instruction(&Instruction::I32Const(total.try_into().unwrap()));
                    add.instruction(&Instruction::I32Add);
                    add.instruction(&Instruction::LocalSet(2));
                    add.instruction(&Instruction::Loop(BlockType::Empty));

                    match &meta.ty {
                        Ty::Unit => {}
                        Ty::Bool | Ty::Fin { .. } => {
                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut zero, 0);
                            meta.layout.store(&mut zero, 0);
                        }
                        Ty::F64 => {
                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::F64Const(0.));
                            meta.layout.store(&mut zero, 0);

                            add.instruction(&Instruction::LocalGet(0));
                            add.instruction(&Instruction::LocalGet(0));
                            meta.layout.load(&mut add, 0);
                            add.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut add, 0);
                            add.instruction(&Instruction::F64Add);
                            meta.layout.store(&mut add, 0);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            let accum = meta.accum.unwrap();
                            let cost = accum.cost;

                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::LocalGet(3));
                            meta.layout.store(&mut zero, 0);
                            zero.instruction(&Instruction::LocalGet(3));
                            zero.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut zero, 0);
                            zero.instruction(&Instruction::Call(accum.zero));
                            zero.instruction(&Instruction::LocalGet(3));
                            zero.instruction(&Instruction::I32Const(cost.try_into().unwrap()));
                            zero.instruction(&Instruction::I32Add);
                            zero.instruction(&Instruction::LocalSet(3));

                            total += cost * n;

                            add.instruction(&Instruction::LocalGet(0));
                            meta.layout.load(&mut add, 0);
                            add.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut add, 0);
                            add.instruction(&Instruction::Call(accum.add));
                        }
                    }

                    zero.instruction(&Instruction::LocalGet(1));
                    zero.instruction(&Instruction::I32Const(size.try_into().unwrap()));
                    zero.instruction(&Instruction::I32Add);
                    zero.instruction(&Instruction::LocalSet(1));
                    zero.instruction(&Instruction::LocalGet(0));
                    zero.instruction(&Instruction::I32Const(size.try_into().unwrap()));
                    zero.instruction(&Instruction::I32Add);
                    zero.instruction(&Instruction::LocalTee(0));
                    zero.instruction(&Instruction::LocalGet(2));
                    zero.instruction(&Instruction::I32LtU);
                    zero.instruction(&Instruction::BrIf(0));
                    zero.instruction(&Instruction::End);

                    add.instruction(&Instruction::LocalGet(1));
                    add.instruction(&Instruction::I32Const(size.try_into().unwrap()));
                    add.instruction(&Instruction::I32Add);
                    add.instruction(&Instruction::LocalSet(1));
                    add.instruction(&Instruction::LocalGet(0));
                    add.instruction(&Instruction::I32Const(size.try_into().unwrap()));
                    add.instruction(&Instruction::I32Add);
                    add.instruction(&Instruction::LocalTee(0));
                    add.instruction(&Instruction::LocalGet(2));
                    add.instruction(&Instruction::I32LtU);
                    add.instruction(&Instruction::BrIf(0));
                    add.instruction(&Instruction::End);
                }

                zero.instruction(&Instruction::End);
                code_section.function(&zero);

                add.instruction(&Instruction::End);
                code_section.function(&add);

                (Layout::U32, Some(total), None)
            }
            Ty::Tuple { members } => {
                let mut mems: Vec<_> = members
                    .iter()
                    .enumerate()
                    .map(|(i, t)| {
                        let Meta { layout, .. } = metas[t.ty()];
                        let (size, align) = layout.size_align();
                        (i, size, align)
                    })
                    .collect();
                mems.sort_unstable_by_key(|&(_, _, align)| align);
                let mut offsets = vec![0; members.len()];
                let mut offset = 0;
                for (i, s, a) in mems {
                    offset = align(offset, a);
                    offsets[i] = offset;
                    offset += s;
                }

                let mut zero = Function::new([(1, ValType::I32)]);
                let mut total = align(offset, 8);
                let mut add = Function::new([]);

                for (member, &offset) in members.iter().zip(offsets.iter()) {
                    let meta = &metas[member.ty()];

                    match &meta.ty {
                        Ty::Unit => {}
                        Ty::Bool | Ty::Fin { .. } => {
                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut zero, offset);
                            meta.layout.store(&mut zero, offset);
                        }
                        Ty::F64 => {
                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::F64Const(0.));
                            meta.layout.store(&mut zero, offset);

                            add.instruction(&Instruction::LocalGet(0));
                            add.instruction(&Instruction::LocalGet(0));
                            meta.layout.load(&mut add, offset);
                            add.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut add, offset);
                            add.instruction(&Instruction::F64Add);
                            meta.layout.store(&mut add, offset);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            let accum = meta.accum.unwrap();
                            let cost = accum.cost;

                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::LocalGet(0));
                            zero.instruction(&Instruction::I32Const(total.try_into().unwrap()));
                            zero.instruction(&Instruction::I32Add);
                            zero.instruction(&Instruction::LocalTee(2));
                            meta.layout.store(&mut zero, offset);
                            zero.instruction(&Instruction::LocalGet(2));
                            zero.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut zero, offset);
                            zero.instruction(&Instruction::Call(accum.zero));

                            total += cost;

                            add.instruction(&Instruction::LocalGet(0));
                            meta.layout.load(&mut add, offset);
                            add.instruction(&Instruction::LocalGet(1));
                            meta.layout.load(&mut add, offset);
                            add.instruction(&Instruction::Call(accum.add));
                        }
                    }
                }

                zero.instruction(&Instruction::End);
                code_section.function(&zero);

                add.instruction(&Instruction::End);
                code_section.function(&add);

                (Layout::U32, Some(total), Some(offsets.into()))
            }
        };
        metas.push(Meta {
            ty,
            layout,
            accum: cost.map(|cost| {
                let zero = extras.try_into().unwrap();
                function_section.function(0);
                let add = (extras + 1).try_into().unwrap();
                function_section.function(0);
                extras += 2;
                Accum { zero, cost, add }
            }),
            members,
        });
    }

    let mut costs = vec![];
    for ((def, _), (refs, def_types)) in funcs.iter() {
        let vt = |t: id::Ty| val_type(&metas[def_types[t.ty()].ty()].ty);
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
        function_section.function((1 + type_index).try_into().unwrap());

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
            metas: &metas,
            imports: &imports,
            extras,
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
    type_section.function([ValType::I32, ValType::I32], []);
    for (params, ret) in func_types {
        type_section.function(params.into_vec(), [ret]);
    }

    let mut memory_section = MemorySection::new();
    let page_size = 65536;
    let cost = funcs.last().map_or(0, |((def, _), (_, def_types))| {
        def.params
            .iter()
            .filter_map(|param| metas[def_types[def.vars[param.var()].ty()].ty()].accum)
            .map(|accum| accum.cost)
            .sum()
    }) + costs.last().unwrap_or(&0);
    let pages = ((cost + page_size - 1) / page_size).into();
    memory_section.memory(MemoryType {
        minimum: pages,
        maximum: Some(pages),
        memory64: false,
        shared: false,
    });

    let mut export_section = ExportSection::new();
    export_section.export(
        "f",
        wasm_encoder::ExportKind::Func,
        (imports.len() + extras + funcs.len() - 1)
            .try_into()
            .unwrap(),
    );
    export_section.export("m", wasm_encoder::ExportKind::Memory, 0);

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
