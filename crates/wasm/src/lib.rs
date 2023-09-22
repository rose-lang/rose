use by_address::ByAddress;
use indexmap::{map::Entry, IndexMap, IndexSet};
use rose::{id, Expr, Func, Instr, Node, Refs, Ty};
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

#[derive(Eq, Hash, PartialEq)]
struct Import<O: Hash + Eq> {
    params: Box<[id::Ty]>,
    ret: id::Ty,
    def: O,
    generics: Box<[id::Ty]>,
}

fn search<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>>(
    imports: &mut IndexSet<Import<O>>,
    refs: &T,
    stack: &mut Vec<(T, &'a Func, Box<[id::Ty]>)>,
    vars: &[id::Ty],
    types: &[id::Ty],
    block: &[Instr],
) {
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
                search(imports, refs, stack, vars, types, body);
            }
            Expr::Call { id, generics, args } => {
                let gens = generics.iter().map(|t| types[t.ty()]).collect();
                match refs.get(*id).unwrap() {
                    Node::Transparent { refs, def } => {
                        stack.push((refs, def, gens));
                    }
                    Node::Opaque { def, .. } => {
                        imports.insert(Import {
                            params: args.iter().map(|x| types[vars[x.var()].ty()]).collect(),
                            ret: types[vars[instr.var.var()].ty()],
                            def,
                            generics: gens,
                        });
                    }
                }
            }
        }
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

type Funcs<'a> = IndexMap<(ByAddress<&'a Func>, Box<[id::Ty]>), Box<[id::Ty]>>;

pub fn compile<'a, O: Hash + Eq, T: Refs<'a, Opaque = O>>(f: Node<'a, O, T>) -> Vec<u8> {
    let mut types = IndexSet::new();
    let mut imports = IndexSet::new();
    let mut funcs: Funcs = IndexMap::new();

    match f {
        Node::Opaque { .. } => todo!(),
        Node::Transparent { refs, def } => {
            let mut stack: Vec<(T, &'a Func, Box<[id::Ty]>)> = vec![(refs, def, [].into())];
            while let Some((refs, def, generics)) = stack.pop() {
                if let Entry::Vacant(entry) = funcs.entry((ByAddress(def), generics)) {
                    let mut def_types = vec![];
                    let (_, generics) = entry.key();
                    for ty in def.types.iter() {
                        def_types.push(resolve(&mut types, generics, &def_types, ty));
                    }
                    search(
                        &mut imports,
                        &refs,
                        &mut stack,
                        &def.vars,
                        &def_types,
                        &def.body,
                    );
                    entry.insert(def_types.into());
                }
            }
        }
    }

    let mut func_types: IndexSet<(Box<[ValType]>, ValType)> = IndexSet::new();
    let import_types: Vec<_> = imports
        .iter()
        .map(|import| {
            let (i, _) = func_types.insert_full((
                import
                    .params
                    .iter()
                    .map(|t| val_type(&types[t.ty()]))
                    .collect(),
                val_type(&types[import.ret.ty()]),
            ));
            i
        })
        .collect();
    let other_types: Vec<_> = funcs
        .iter()
        .map(|((def, _), def_types)| {
            let (i, _) = func_types.insert_full((
                def.params
                    .iter()
                    .map(|param| val_type(&types[def_types[def.vars[param.var()].ty()].ty()]))
                    .collect(),
                val_type(&types[def_types[def.vars[def.ret.var()].ty()].ty()]),
            ));
            i
        })
        .collect();

    let mut type_section = TypeSection::new();
    for (params, ret) in func_types {
        type_section.function(params.into_vec(), [ret]);
    }

    let mut import_section = ImportSection::new();
    for i in import_types {
        // TODO: use unique import names
        import_section.import("", "", EntityType::Function(i.try_into().unwrap()));
    }

    let mut function_section = FunctionSection::new();
    for i in other_types {
        function_section.function(i.try_into().unwrap());
    }

    let mut export_section = ExportSection::new();
    export_section.export("", wasm_encoder::ExportKind::Func, 0); // TODO: get the function index

    let mut code_section = CodeSection::new();

    let mut g = Function::new([]); // TODO: generate code
    g.instruction(&Instruction::LocalGet(0));
    g.instruction(&Instruction::LocalGet(1));
    g.instruction(&Instruction::F64Add);
    g.instruction(&Instruction::End);
    code_section.function(&g);

    let mut module = Module::new();
    module.section(&type_section);
    module.section(&import_section);
    module.section(&function_section);
    module.section(&export_section);
    module.section(&code_section);
    module.finish()
}
