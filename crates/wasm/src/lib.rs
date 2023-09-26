use by_address::ByAddress;
use indexmap::{map::Entry, IndexMap, IndexSet};
use rose::{id, Binop, Expr, Func, Instr, Node, Refs, Ty, Unop};
use std::{hash::Hash, mem::take};
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemArg, MemorySection, MemoryType, Module, TypeSection, ValType,
};

/// Resolve `ty` via `generics` and `types`, then return its ID in `typemap`, inserting if need be.
///
/// This is meant to be used to pull all the types from a callee into a broader context. The
/// `generics` are the IDs of all the types provided as generic type parameters for the callee. The
/// `types are the IDs of all the types that have been pulled in so far.
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

/// An index of opaque functions.
///
/// Each key holds the opaque function itself followed by the generic parameters used for this
/// particular instance. The value is the resolved type signature of the function according to a
/// global type index.
type Imports<O> = IndexMap<(O, Box<[id::Ty]>), (Box<[id::Ty]>, id::Ty)>;

/// An index of transparent functions.
///
/// Each key holds a reference to the function itself followed by the generic parameters used for
/// this particular instance. The value holds the function's immediate callees (see `rose::Refs`)
/// followed by a mapping from the function's own type indices to resolved type indices in a
/// global type index.
type Funcs<'a, T> = IndexMap<(ByAddress<&'a Func>, Box<[id::Ty]>), (T, Box<[id::Ty]>)>;

/// Computes a topological sort of a call graph via depth-first search.
struct Topsort<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    /// All types seen so far.
    types: IndexSet<Ty>,

    /// All opaque functions seen so far.
    imports: Imports<O>,

    /// All transparent functions seen so far, in topological sorted order.
    funcs: Funcs<'a, T>,
}

impl<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>> Topsort<'a, O, T> {
    /// Search in the given `block` of `f`, using `refs` to resolve immediate function calls.
    ///
    /// The `types` argument is the resolved type ID for each of `f.types` in `self.types`.
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
                                let (_, gens) = key; // get back `gens` to please the borrow checker
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
                                    // we should never see the same exact opaque function with the
                                    // same generic type parameters but multiple different type
                                    // signatures
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

    /// Search from `def` with the given `generics`, using `refs` to resolve immediate calls.
    fn func(&mut self, refs: T, def: &'a Func, generics: Box<[id::Ty]>) {
        let mut types = vec![];
        for ty in def.types.iter() {
            types.push(resolve(&mut self.types, &generics, &types, ty));
        }
        self.block(&refs, def, &types, &def.body);
        let prev = self
            .funcs
            .insert((ByAddress(def), generics), (refs, types.into()));
        // we're doing depth-first search on a DAG, so even if we wait until this last moment to
        // mark the node as visited, we still can't have seen it already
        assert!(prev.is_none());
    }
}

/// Return the WebAssembly value type used to represent a local of type `ty`.
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

/// A WebAssembly memory offset or size.
type Size = u32;

/// Convert a `usize` to a `Size`.
///
/// This will always succeed if the compiler itself is running inside WebAssembly.
fn u_size(x: usize) -> Size {
    x.try_into().unwrap()
}

/// Round up `size` to the nearest multiple of `align`.
fn aligned(size: Size, align: Size) -> Size {
    (size + align - 1) & !(align - 1)
}

/// The layout of a type in memory.
#[derive(Clone, Copy)]
enum Layout {
    /// The unit type. Zero-sized.
    Unit,

    /// An unsigned 8-bit integer.
    U8,

    /// An unsigned 16-bit integer.
    U16,

    /// An unsigned 32-bit integer.
    U32,

    /// A 64-bit floating-point number.
    F64,

    /// `Ty::Ref` cannot be stored in memory.
    Ref,
}

impl Layout {
    /// Return the size and alignment of this `Layout`, in bytes.
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

    /// Return the size of this `Layout`, which is always aligned.
    fn size(self) -> Size {
        let (size, _) = self.size_align();
        size // no need to use alignment, because every possible `Layout` size is already aligned
    }

    /// Emit a load instruction for this layout with the given byte offset.
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

    /// Emit a store instruction for this layout with the given byte offset.
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

/// The index of a WebAssembly local.
type Local = u32;

/// Information about a type that has functions for accumulation.
#[derive(Clone, Copy)]
struct Accum {
    /// The ID of the zero function.
    zero: u32,

    /// The allocation cost of the zero function.
    cost: Size,

    // The ID of the add function, which has no allocation cost.
    add: u32,
}

/// Information about a type that is necessary for code generation.
struct Meta {
    /// The type.
    ty: Ty,

    /// The layout of the type.
    layout: Layout,

    /// Zero and add functions for accumulation, if this type is an array or tuple.
    accum: Option<Accum>,

    /// Offsets of each member of a tuple.
    members: Option<Box<[Size]>>,
}

/// Generates WebAssembly code for a function.
struct Codegen<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> {
    /// Metadata about all the types in the global type index.
    metas: &'b [Meta],

    /// All opaque functions.
    imports: &'b Imports<O>,

    /// The number of opaque functions plus the number of accumulation functions (zeros and adds).
    extras: usize,

    /// All transparent functions.
    funcs: &'b Funcs<'a, T>,

    /// The allocation cost of each transparent function.
    costs: &'b [Size],

    /// To resolve calls.
    refs: &'b T,

    /// The definition of the particular function we're generating code for.
    def: &'b Func,

    /// Mapping from this function's type indices to type indices in the global type index.
    types: &'b [id::Ty],

    /// The WebAssembly local assigned to each variable in this function.
    locals: &'b [Local],

    /// The amount of memory allocated so far in the current block.
    ///
    /// This is for the block and not the entire function, because for instance, a loop's total
    /// allocation cost depends both on its block's allocation cost and on the number of iterations.
    offset: Size,

    /// The WebAssembly function under construction.
    wasm: Function,
}

impl<'a, 'b, O: Hash + Eq, T: Refs<'a, Opaque = O>> Codegen<'a, 'b, O, T> {
    /// Return metadata for the type ID `t` in the current function.
    ///
    /// Do not use this if your type ID is already resolved to refer to the global type index.
    fn meta(&self, t: id::Ty) -> &'b Meta {
        &self.metas[self.types[t.ty()].ty()]
    }

    /// Emit an instruction to push the value of `x` onto the stack.
    fn get(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalGet(self.locals[x.var()]));
    }

    /// Emit an instruction to pop the top of the stack and store it in `x`.
    fn set(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalSet(self.locals[x.var()]));
    }

    /// Emit an instruction to store the stack top in `x` without popping it.
    fn tee(&mut self, x: id::Var) {
        self.wasm
            .instruction(&Instruction::LocalTee(self.locals[x.var()]));
    }

    /// Emit an instruction to push the current memory allocation pointer onto the stack.
    fn pointer(&mut self) {
        self.wasm
            .instruction(&Instruction::LocalGet(u_size(self.def.params.len())));
    }

    /// Emit an instruction to push the constant integer value `x` onto the stack.
    fn u32_const(&mut self, x: u32) {
        self.wasm.instruction(&Instruction::I32Const(x as i32));
    }

    /// Emit instructions to increase the memory allocation pointer by `size` bytes.
    fn bump(&mut self, size: Size) {
        let aligned = aligned(size, 8);
        self.pointer();
        self.u32_const(aligned);
        self.wasm.instruction(&Instruction::I32Add);
        self.wasm
            .instruction(&Instruction::LocalSet(u_size(self.def.params.len())));
        self.offset += aligned;
    }

    /// Emit instruction(s) to load a value with the given `layout` and `offset`.
    fn load(&mut self, layout: Layout, offset: Size) {
        layout.load(&mut self.wasm, offset)
    }

    /// Emit instruction(s) to store a value with the given `layout` and `offset`.
    fn store(&mut self, layout: Layout, offset: Size) {
        layout.store(&mut self.wasm, offset)
    }

    /// Generate code for the given `block`.
    fn block(&mut self, block: &[Instr]) {
        for instr in block.iter() {
            match &instr.expr {
                Expr::Unit => {
                    self.wasm.instruction(&Instruction::I32Const(0));
                }
                &Expr::Bool { val } => {
                    self.wasm.instruction(&Instruction::I32Const(val.into()));
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
                    let size = layout.size();
                    for (i, &elem) in elems.iter().enumerate() {
                        self.pointer();
                        self.get(elem);
                        self.store(layout, size * u_size(i));
                    }
                    self.pointer();
                    self.bump(size * u_size(elems.len()));
                }
                Expr::Tuple { members } => {
                    let Meta { members: mems, .. } = self.meta(self.def.vars[instr.var.var()]);
                    let mut size = 0;
                    for (&member, &offset) in members.iter().zip(mems.as_ref().unwrap().iter()) {
                        let &Meta { layout, .. } = self.meta(self.def.vars[member.var()]);
                        self.pointer();
                        self.get(member);
                        self.store(layout, offset);
                        size = size.max(offset + layout.size());
                    }
                    self.pointer();
                    self.bump(size);
                }
                &Expr::Index { array, index } => {
                    let &Meta { layout, .. } = self.meta(self.def.vars[instr.var.var()]);
                    let size = layout.size();
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
                    let size = meta.layout.size();
                    self.get(array);
                    self.get(index);
                    self.u32_const(size);
                    self.wasm.instruction(&Instruction::I32Mul);
                    self.wasm.instruction(&Instruction::I32Add);
                    if let Ty::Array { .. } | Ty::Tuple { .. } = &meta.ty {
                        // if this array holds primitives then we just want a pointer to the
                        // element, but if it's actually another composite value then it's already a
                        // pointer, so we need to do a load because otherwise we'd have a pointer to
                        // a pointer instead of just one direct pointer
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
                            // if this array holds primitives then we just want a pointer to the
                            // element
                            self.u32_const(offset);
                            self.wasm.instruction(&Instruction::I32Add);
                        }
                        Ty::Generic { .. } | Ty::Ref { .. } => unreachable!(),
                        Ty::Array { .. } | Ty::Tuple { .. } => {
                            // if this array holds other composite values then each element is
                            // already a pointer, so we need to do a load because otherwise we'd
                            // have a pointer to a pointer instead of just one direct pointer
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
                        // TODO: `f64.const` instructions are always 8 bytes, much larger than most
                        // instructions; maybe we should just keep this constant in a local
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
                            self.extras + j
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
                    let size = layout.size();

                    // we need to set the local now rather than later, because we're going to bump
                    // the pointer for the array itself and possibly in the loop body, but we still
                    // need to know this pointer so we can use it to store each element of the array
                    self.pointer();
                    self.set(instr.var);

                    // we put the bounds check at the end of the loop, so if it's going to execute
                    // zero times then we need to make sure not to enter it at all; easiest way is
                    // to just not emit the loop instructions at all
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
                        // this is a bit subtle: usually a `Ref` variable is a pointer, and that is
                        // also true for `Ref` variables to values of these three discrete
                        // continuous types if they come from an `Expr::Slice` or `Expr::Field`;
                        // but, if we're directly starting an accumulator for a discrete primitive
                        // value, then its value can't be modified, so we can just store it directly
                        // instead of allocating extra memory; this works because the WebAssembly
                        // value types for all these discrete primitive types are the same as for
                        // pointers, and it's OK to have the representation be different depending
                        // on whether it's directly introduced by `Expr::Accum` or not, because
                        // those are the only ones on which we can use `Expr::Resolve`, and `Ref`s
                        // cannot be directly read before they're resolved anyway
                        Ty::Unit | Ty::Bool | Ty::Fin { .. } => self.get(shape),
                        Ty::F64 => {
                            self.pointer();
                            self.pointer();
                            // TODO: `f64.const` instructions are always 8 bytes, much larger than
                            // most instructions; maybe we should just keep this constant in a local
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
                        // as explained above, if the `inner` value is a discrete primitive type
                        // then we cheated and stored it directly in the local so there's nothing to
                        // do, and if it's a pointer then we still just need a pointer so there's
                        // also nothing to do; but if it's a continuous primitive type then we
                        // introduced an extra layer of indirection so we need to loads
                        self.load(Layout::F64, 0);
                    }
                }
            }
            self.set(instr.var);
        }
    }
}

/// A WebAssembly module for a graph of functions.
///
/// The module exports its memory with name `"m"` and its entrypoint function with name `"f"`. The
/// function takes one parameter in addition to its original parameters, which must be an
/// 8-byte-aligned pointer to the start of the memory region it can use for allocation. The memory
/// is the exact number of pages necessary to accommodate the function's own memory allocation as
/// well as memory allocation for all of its parameters, with each node in each parameter's memory
/// allocation tree being 8-byte aligned. That is, the function's last argument should be just large
/// enough to accommodate those allocations for all the parameters; in that case, no memory will be
/// incorrectly overwritten and no out-of-bounds memory accesses will occur.
pub struct Wasm<O> {
    /// The bytes of the WebAssembly module binary.
    pub bytes: Vec<u8>,

    /// All the opaque functions that the WebAssembly module must import, in order.
    ///
    /// The module name for each import is the empty string, and the field name is the base-ten
    /// representation of its index in this collection.
    pub imports: Imports<O>,
}

/// Compile `f` and all its direct and indirect callees to a WebAssembly module.
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
            // if `f` itself is an opaque function then the graph of all callees has only one node
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

    // we add to this lazily as we generate our imports, functions, and code, after which we'll
    // generate the actual function types section right near the end; it doesn't matter as long as
    // the order we actually add the sections to the module itself is correct
    let mut func_types: IndexSet<(Box<[ValType]>, ValType)> = IndexSet::new();

    let mut import_section = ImportSection::new();
    for (i, (params, ret)) in imports.values().enumerate() {
        let (type_index, _) = func_types.insert_full((
            params.iter().map(|t| val_type(&types[t.ty()])).collect(),
            val_type(&types[ret.ty()]),
        ));
        // we reserve type index zero for the type with two `i32` params and no results, which we
        // use for accumulation zero and add functions; we don't include that in the `func_types`
        // index itself, because that index only holds function types with exactly one result
        import_section.import(
            "",
            &i.to_string(),
            EntityType::Function((1 + type_index).try_into().unwrap()),
        );
    }

    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();

    let mut metas: Vec<Meta> = vec![];
    let mut extras: usize = imports.len();
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
                let size = meta.layout.size();

                // for both the zero function and the add function, the first parameter is a pointer
                // to the accumulator value, and the second parameter is the pointer to the other
                // value (the shape for zero, or the addend for add)

                // the first local is a pointer to the end of the accumulator array, used for bounds
                // checking; the second local is a memory allocation pointer, used as the
                // accumulator pointer for calls to the zero function for elements if this array
                // stores composite values
                let mut zero = Function::new([(2, ValType::I32)]);
                let mut total = aligned(size * n, 8);
                // same as zero, the local is a pointer to the end of the accumulator array, used
                // for bounds checking
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
                            // TODO: `f64.const` instructions are always 8 bytes, much larger than
                            // most instructions; maybe we should just keep this constant in a local
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
                mems.sort_by_key(|&(_, _, align)| align);
                let mut offsets = vec![0; members.len()];
                let mut offset = 0;
                for (i, s, a) in mems {
                    offset = aligned(offset, a);
                    offsets[i] = offset;
                    offset += s;
                }

                // the local is a memory allocation pointer, used as the accumulator pointer for
                // calls to the zero function for composite elements of the tuple
                let mut zero = Function::new([(1, ValType::I32)]);
                let mut total = aligned(offset, 8);
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
                            // TODO: `f64.const` instructions are always 8 bytes, much larger than
                            // most instructions; maybe we should just keep this constant in a local
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

    let mut costs = vec![]; // allocation cost of each function, in bytes
    for ((def, _), (refs, def_types)) in funcs.iter() {
        let vt = |t: id::Ty| val_type(&metas[def_types[t.ty()].ty()].ty); // short for `ValType`
        let params: Local = (def.params.len() + 1).try_into().unwrap(); // extra pointer parameter
        let mut locals = vec![None; def.vars.len()];

        let (type_index, _) = func_types.insert_full((
            def.params
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    locals[param.var()] = Some(i.try_into().unwrap());
                    vt(def.vars[param.var()])
                })
                .chain([ValType::I32]) // extra pointer parameter
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

        let locals: Box<_> = locals.into_iter().map(Option::unwrap).collect();
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
    type_section.function([ValType::I32, ValType::I32], []); // for accumulation functions
    for (params, ret) in func_types {
        type_section.function(params.into_vec(), [ret]);
    }

    let mut memory_section = MemorySection::new();
    let page_size = 65536; // https://webassembly.github.io/spec/core/exec/runtime.html#page-size
    let cost = funcs.last().map_or(0, |((def, _), (_, def_types))| {
        def.params
            .iter()
            .filter_map(|param| metas[def_types[def.vars[param.var()].ty()].ty()].accum)
            .map(|accum| accum.cost)
            .sum()
    }) + costs.last().unwrap_or(&0);
    let pages = ((cost + page_size - 1) / page_size).into(); // round up to a whole number of pages
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
        (extras + funcs.len() - 1).try_into().unwrap(),
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
