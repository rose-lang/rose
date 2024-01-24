use by_address::ByAddress;
use enumset::EnumSet;
use indexmap::{IndexMap, IndexSet};
use rose::{id, Binop, Constraint, Expr, Func, Instr, Node, Refs, Ty, Unop};
use std::{fmt, hash::Hash};

fn write_constraints(f: &mut fmt::Formatter<'_>, constraints: EnumSet<Constraint>) -> fmt::Result {
    let mut first = true;
    for constraint in constraints.iter() {
        if first {
            first = false;
        } else {
            write!(f, " + ")?;
        }
        write!(f, "{constraint:?}")?;
    }
    Ok(())
}

fn write_generics(f: &mut fmt::Formatter<'_>, generics: &[EnumSet<Constraint>]) -> fmt::Result {
    write!(f, "<")?;
    let mut first = true;
    for (i, &constraints) in generics.iter().enumerate() {
        if first {
            first = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, "G{i}: ")?;
        write_constraints(f, constraints)?;
    }
    write!(f, ">")
}

fn write_types(f: &mut fmt::Formatter<'_>, types: &[Ty]) -> fmt::Result {
    for (i, ty) in types.iter().enumerate() {
        write!(f, "  type T{i} = ")?;
        match ty {
            Ty::Unit | Ty::Bool | Ty::F64 => writeln!(f, "{ty:?}")?,
            Ty::Fin { size } => writeln!(f, "{size}")?,
            Ty::Generic { id } => writeln!(f, "G{}", id.generic())?,
            Ty::Ref { inner } => writeln!(f, "&T{}", inner.ty())?,
            Ty::Array { index, elem } => writeln!(f, "[T{}]T{}", index.ty(), elem.ty())?,
            Ty::Tuple { members } => {
                write!(f, "(")?;
                write_elems(f, 'T', members.iter().map(|member| member.ty()))?;
                writeln!(f, ")")?;
            }
        }
    }
    Ok(())
}

fn write_opaque(
    f: &mut fmt::Formatter<'_>,
    generics: &[EnumSet<Constraint>],
    types: &[Ty],
    params: &[id::Ty],
    ret: id::Ty,
) -> fmt::Result {
    write_generics(f, generics)?;
    writeln!(f, "{{")?;
    write_types(f, types)?;
    write!(f, "  opaque: (")?;
    write_elems(f, 'T', params.iter().map(|t| t.ty()))?;
    writeln!(f, ") -> T{}", ret.ty())?;
    writeln!(f, "}}")
}

fn search<'a, O: Eq + Hash, T: Refs<'a, Opaque = O>>(
    f: &mut fmt::Formatter<'_>,
    imports: &mut IndexSet<O>,
    funcs: &mut IndexMap<ByAddress<&'a Func>, T>,
    refs: &T,
    block: &[Instr],
) -> fmt::Result {
    for instr in block.iter() {
        match &instr.expr {
            &Expr::Call { id, .. } => match refs.get(id).unwrap() {
                Node::Transparent { refs, def } => {
                    let key = ByAddress(def);
                    if !funcs.contains_key(&key) {
                        search(f, imports, funcs, &refs, &def.body)?;
                        funcs.insert(key, refs);
                    }
                }
                Node::Opaque {
                    generics,
                    types,
                    params,
                    ret,
                    def,
                } => {
                    let (i, new) = imports.insert_full(def);
                    if new {
                        write!(f, "fn f{i} = ")?;
                        write_opaque(f, generics, types, params, ret)?;
                        writeln!(f)?;
                    }
                }
            },
            Expr::For { body, .. } => search(f, imports, funcs, refs, body)?,
            _ => {}
        }
    }
    Ok(())
}

fn write_elems(
    f: &mut fmt::Formatter<'_>,
    prefix: char,
    items: impl Iterator<Item = usize>,
) -> std::fmt::Result {
    let mut first = true;
    for item in items {
        if first {
            first = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, "{}{}", prefix, item)?;
    }
    Ok(())
}

struct Function<'a, 'b, O, T> {
    imports: &'b IndexSet<O>,
    funcs: &'b IndexMap<ByAddress<&'a Func>, T>,
    refs: &'b T,
    def: &'a Func,
}

impl<'a, O: Eq + Hash, T: Refs<'a, Opaque = O>> Function<'a, '_, O, T> {
    fn write_instr(&self, f: &mut fmt::Formatter<'_>, spaces: usize, instr: &Instr) -> fmt::Result {
        for _ in 0..spaces {
            write!(f, " ")?;
        }
        let x = instr.var.var();
        write!(f, "let x{}: T{} = ", x, self.def.vars[x].ty())?;
        match &instr.expr {
            Expr::Unit => writeln!(f, "unit")?,
            Expr::Bool { val } => writeln!(f, "{val}")?,
            Expr::F64 { val } => writeln!(f, "{val}")?,
            Expr::Fin { val } => writeln!(f, "{val}")?,
            Expr::Array { elems } => {
                write!(f, "[")?;
                write_elems(f, 'x', elems.iter().map(|elem| elem.var()))?;
                writeln!(f, "]")?;
            }
            Expr::Tuple { members } => {
                write!(f, "(")?;
                write_elems(f, 'x', members.iter().map(|member| member.var()))?;
                writeln!(f, ")")?;
            }
            Expr::Index { array, index } => writeln!(f, "x{}[x{}]", array.var(), index.var())?,
            Expr::Member { tuple, member } => writeln!(f, "x{}[{}]", tuple.var(), member.member())?,
            Expr::Slice { array, index } => writeln!(f, "&x{}[x{}]", array.var(), index.var())?,
            Expr::Field { tuple, member } => writeln!(f, "&x{}[{}]", tuple.var(), member.member())?,
            Expr::Unary { op, arg } => match op {
                Unop::Not => writeln!(f, "not x{}", arg.var())?,
                Unop::IMod => writeln!(f, "x{} mod T{}", arg.var(), self.def.vars[x].ty())?,
                Unop::Neg => writeln!(f, "-x{}", arg.var())?,
                Unop::Abs => writeln!(f, "|x{}|", arg.var())?,
                Unop::Sign => writeln!(f, "sign(x{})", arg.var())?,
                Unop::Ceil => writeln!(f, "ceil(x{})", arg.var())?,
                Unop::Floor => writeln!(f, "floor(x{})", arg.var())?,
                Unop::Trunc => writeln!(f, "trunc(x{})", arg.var())?,
                Unop::Sqrt => writeln!(f, "sqrt(x{})", arg.var())?,
            },
            Expr::Binary { op, left, right } => match op {
                Binop::And => writeln!(f, "x{} and x{}", left.var(), right.var())?,
                Binop::Or => writeln!(f, "x{} or x{}", left.var(), right.var())?,
                Binop::Iff => writeln!(f, "x{} iff x{}", left.var(), right.var())?,
                Binop::Xor => writeln!(f, "x{} xor x{}", left.var(), right.var())?,
                Binop::INeq => writeln!(f, "x{} != x{}", left.var(), right.var())?,
                Binop::ILt => writeln!(f, "x{} < x{}", left.var(), right.var())?,
                Binop::ILeq => writeln!(f, "x{} <= x{}", left.var(), right.var())?,
                Binop::IEq => writeln!(f, "x{} == x{}", left.var(), right.var())?,
                Binop::IGt => writeln!(f, "x{} > x{}", left.var(), right.var())?,
                Binop::IGeq => writeln!(f, "x{} >= x{}", left.var(), right.var())?,
                Binop::IAdd => writeln!(f, "x{} + x{}", left.var(), right.var())?,
                Binop::Neq => writeln!(f, "x{} != x{}", left.var(), right.var())?,
                Binop::Lt => writeln!(f, "x{} < x{}", left.var(), right.var())?,
                Binop::Leq => writeln!(f, "x{} <= x{}", left.var(), right.var())?,
                Binop::Eq => writeln!(f, "x{} == x{}", left.var(), right.var())?,
                Binop::Gt => writeln!(f, "x{} > x{}", left.var(), right.var())?,
                Binop::Geq => writeln!(f, "x{} >= x{}", left.var(), right.var())?,
                Binop::Add => writeln!(f, "x{} + x{}", left.var(), right.var())?,
                Binop::Sub => writeln!(f, "x{} - x{}", left.var(), right.var())?,
                Binop::Mul => writeln!(f, "x{} * x{}", left.var(), right.var())?,
                Binop::Div => writeln!(f, "x{} / x{}", left.var(), right.var())?,
            },
            Expr::Select { cond, then, els } => {
                writeln!(f, "x{} ? x{} : x{}", cond.var(), then.var(), els.var())?
            }
            Expr::Call { id, generics, args } => {
                let i = match self.refs.get(*id).unwrap() {
                    Node::Transparent { def, .. } => {
                        self.imports.len() + self.funcs.get_index_of(&ByAddress(def)).unwrap()
                    }
                    Node::Opaque { def, .. } => self.imports.get_index_of(&def).unwrap(),
                };
                write!(f, "f{i}<")?;
                write_elems(f, 'T', generics.iter().map(|generic| generic.ty()))?;
                write!(f, ">(")?;
                write_elems(f, 'x', args.iter().map(|arg| arg.var()))?;
                writeln!(f, ")")?;
            }
            Expr::For { arg, body, ret } => {
                writeln!(
                    f,
                    "for x{}: T{} {{",
                    arg.var(),
                    self.def.vars[arg.var()].ty()
                )?;
                self.write_block(f, spaces + 2, body, *ret)?;
                for _ in 0..spaces {
                    write!(f, " ")?;
                }
                writeln!(f, "}}")?
            }
            Expr::Accum { shape } => writeln!(f, "accum x{}", shape.var())?,
            Expr::Add { accum, addend } => writeln!(f, "x{} += x{}", accum.var(), addend.var())?,
            Expr::Resolve { var } => writeln!(f, "resolve x{}", var.var())?,
        }
        Ok(())
    }

    fn write_block(
        &self,
        f: &mut fmt::Formatter<'_>,
        spaces: usize,
        body: &[Instr],
        ret: id::Var,
    ) -> fmt::Result {
        for instr in body.iter() {
            self.write_instr(f, spaces, instr)?;
        }
        for _ in 0..spaces {
            write!(f, " ")?;
        }
        writeln!(f, "x{}", ret.var())?;
        Ok(())
    }

    fn write_func(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_generics(f, &self.def.generics)?;
        writeln!(f, "{{")?;
        write_types(f, &self.def.types)?;
        write!(f, "  (")?;
        let mut first = true;
        for param in self.def.params.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "x{}: T{}", param.var(), self.def.vars[param.var()].ty())?;
        }
        writeln!(f, ") -> T{} {{", self.def.vars[self.def.ret.var()].ty())?;
        self.write_block(f, 4, &self.def.body, self.def.ret)?;
        writeln!(f, "  }}")?;
        writeln!(f, "}}")
    }
}

pub fn write_graph<'a, O: Eq + Hash, T: Refs<'a, Opaque = O>>(
    f: &mut fmt::Formatter<'_>,
    root: Node<'a, O, T>,
) -> fmt::Result {
    match root {
        Node::Opaque {
            generics,
            types,
            params,
            ret,
            def: _,
        } => {
            write!(f, "fn f0 = ")?;
            write_opaque(f, generics, types, params, ret)
        }
        Node::Transparent { refs, def } => {
            let mut imports = IndexSet::new();
            let mut funcs = IndexMap::new();
            search(f, &mut imports, &mut funcs, &refs, &def.body)?;
            for (i, (def, refs)) in funcs.iter().enumerate() {
                write!(f, "fn f{} = ", imports.len() + i)?;
                Function {
                    imports: &imports,
                    funcs: &funcs,
                    refs,
                    def,
                }
                .write_func(f)?;
                writeln!(f)?;
            }
            write!(f, "fn f{} = ", imports.len() + funcs.len())?;
            Function {
                imports: &imports,
                funcs: &funcs,
                refs: &refs,
                def,
            }
            .write_func(f)
        }
    }
}
