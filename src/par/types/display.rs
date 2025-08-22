use crate::location::{FileName, Span};
use crate::par::process::NameWithType;
use crate::par::types::{PrimitiveType, Type, TypeDefs};
use std::fmt;
use std::fmt::Write;

impl Type {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Primitive(_, PrimitiveType::Nat) => write!(f, "Nat"),
            Self::Primitive(_, PrimitiveType::Int) => write!(f, "Int"),
            Self::Primitive(_, PrimitiveType::String) => write!(f, "String"),
            Self::Primitive(_, PrimitiveType::Char) => write!(f, "Char"),
            Self::Primitive(_, PrimitiveType::Byte) => write!(f, "Byte"),
            Self::Primitive(_, PrimitiveType::Bytes) => write!(f, "Bytes"),

            Self::DualPrimitive(_, PrimitiveType::Nat) => write!(f, "dual Nat"),
            Self::DualPrimitive(_, PrimitiveType::Int) => write!(f, "dual Int"),
            Self::DualPrimitive(_, PrimitiveType::String) => write!(f, "dual String"),
            Self::DualPrimitive(_, PrimitiveType::Char) => write!(f, "dual Char"),
            Self::DualPrimitive(_, PrimitiveType::Byte) => write!(f, "dual Byte"),
            Self::DualPrimitive(_, PrimitiveType::Bytes) => write!(f, "dual Bytes"),

            Self::Var(_, name) => write!(f, "{}", name),
            Self::DualVar(_, name) => write!(f, "dual {}", name),
            Self::Name(_, name, args) => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty(f, indent)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }
            Self::DualName(_, name, args) => {
                write!(f, "dual {}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty(f, indent)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }

            Self::Box(_, body) => {
                write!(f, "box ")?;
                body.pretty(f, indent)
            }
            Self::DualBox(_, body) => {
                write!(f, "dual box ")?;
                body.pretty(f, indent)
            }

            Self::Pair(_, arg, then) => {
                let mut then = then;
                write!(f, "(")?;
                arg.pretty(f, indent)?;
                while let Self::Pair(_, arg, next) = then.as_ref() {
                    write!(f, ", ")?;
                    arg.pretty(f, indent)?;
                    then = next;
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty(f, indent)
                }
            }

            Self::Function(_, param, then) => {
                let mut then = then;
                write!(f, "[")?;
                param.pretty(f, indent)?;
                while let Self::Function(_, param, next) = then.as_ref() {
                    write!(f, ", ")?;
                    param.pretty(f, indent)?;
                    then = next;
                }
                if let Self::Continue(_) = then.as_ref() {
                    write!(f, "]?")
                } else {
                    write!(f, "] ")?;
                    then.pretty(f, indent)
                }
            }

            Self::Either(_, branches) => {
                write!(f, "either {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} ", branch)?;
                    typ.pretty(f, indent + 1)?;
                    write!(f, ",")?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Choice(_, branches) => {
                write!(f, "choice {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} => ", branch)?;
                    typ.pretty(f, indent + 1)?;
                    write!(f, ",")?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { label, body, .. } => {
                write!(f, "recursive")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }

            Self::Exists(_, name, then) => {
                let mut then = then;
                write!(f, "(type {name}")?;
                while let Self::Exists(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, ") ")?;
                then.pretty(f, indent)
            }

            Self::Forall(_, name, then) => {
                let mut then = then;
                write!(f, "[type {name}")?;
                while let Self::Forall(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, "] ")?;
                then.pretty(f, indent)
            }
        }
    }

    pub fn pretty_compact(&self, f: &mut impl Write) -> fmt::Result {
        match self {
            Self::Primitive(_, PrimitiveType::Nat) => write!(f, "Nat"),
            Self::Primitive(_, PrimitiveType::Int) => write!(f, "Int"),
            Self::Primitive(_, PrimitiveType::String) => write!(f, "String"),
            Self::Primitive(_, PrimitiveType::Char) => write!(f, "Char"),
            Self::Primitive(_, PrimitiveType::Byte) => write!(f, "Byte"),
            Self::Primitive(_, PrimitiveType::Bytes) => write!(f, "Bytes"),

            Self::DualPrimitive(_, PrimitiveType::Nat) => write!(f, "dual Nat"),
            Self::DualPrimitive(_, PrimitiveType::Int) => write!(f, "dual Int"),
            Self::DualPrimitive(_, PrimitiveType::String) => write!(f, "dual String"),
            Self::DualPrimitive(_, PrimitiveType::Char) => write!(f, "dual Char"),
            Self::DualPrimitive(_, PrimitiveType::Byte) => write!(f, "dual Byte"),
            Self::DualPrimitive(_, PrimitiveType::Bytes) => write!(f, "dual Bytes"),

            Self::Var(_, name) => write!(f, "{}", name),
            Self::DualVar(_, name) => write!(f, "dual {}", name),
            Self::Name(_, name, args) => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty_compact(f)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }
            Self::DualName(_, name, args) => {
                write!(f, "dual {}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty_compact(f)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }

            Self::Box(_, body) => {
                write!(f, "box ")?;
                body.pretty_compact(f)
            }
            Self::DualBox(_, body) => {
                write!(f, "dual box ")?;
                body.pretty_compact(f)
            }

            Self::Pair(_, arg, then) => {
                let mut then = then;
                write!(f, "(")?;
                arg.pretty_compact(f)?;
                while let Self::Pair(_, arg, next) = then.as_ref() {
                    write!(f, ", ")?;
                    arg.pretty_compact(f)?;
                    then = next;
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Function(_, param, then) => {
                let mut then = then;
                write!(f, "[")?;
                param.pretty_compact(f)?;
                while let Self::Function(_, param, next) = then.as_ref() {
                    write!(f, ", ")?;
                    param.pretty_compact(f)?;
                    then = next;
                }
                if let Self::Continue(_) = then.as_ref() {
                    write!(f, "]?")
                } else {
                    write!(f, "] ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Either(_, branches) => {
                let branches = branches
                    .iter()
                    .map(|(branch, _)| format!(".{branch}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "either {{ {branches} }}")
            }

            Self::Choice(_, branches) => {
                let branches = branches
                    .iter()
                    .map(|(branch, _)| format!(".{branch}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "choice {{ {branches} }}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { label, body, .. } => {
                write!(f, "recursive")?;
                if !matches!(body.as_ref(), Self::Either(..)) {
                    if let Some(label) = label {
                        write!(f, "/{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if !matches!(body.as_ref(), Self::Choice(..)) {
                    if let Some(label) = label {
                        write!(f, "/{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }

            Self::Exists(_, name, then) => {
                let mut then = then;
                write!(f, "(type {name}")?;
                while let Self::Exists(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, ") ")?;
                then.pretty_compact(f)
            }

            Self::Forall(_, name, then) => {
                let mut then = then;
                write!(f, "[type {name}")?;
                while let Self::Forall(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, "] ")?;
                then.pretty_compact(f)
            }
        }
    }

    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs,
        consume: &mut impl FnMut(Span, NameWithType),
    ) {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => {}
            Self::Var(_, _) | Self::DualVar(_, _) => {}
            Self::Name(span, name, args) | Self::DualName(span, name, args) => {
                let (def_span, def_file, typ) = match self {
                    Self::Name(..) => type_defs.get_with_span(span, name, args),
                    _ => type_defs.get_dual_with_span(span, name, args),
                }
                .unwrap_or_else(|_| (Span::None, &FileName::Builtin, self.clone()));
                consume(
                    *span,
                    NameWithType {
                        name: None,
                        typ,
                        def_span,
                        decl_span: def_span,
                        def_file: def_file.clone(),
                    },
                );
                for arg in args {
                    arg.types_at_spans(type_defs, consume);
                }
            }
            Self::Box(_, body) | Self::DualBox(_, body) => body.types_at_spans(type_defs, consume),
            Self::Pair(_, t, u) => {
                t.types_at_spans(type_defs, consume);
                u.types_at_spans(type_defs, consume);
            }
            Self::Function(_, t, u) => {
                t.types_at_spans(type_defs, consume);
                u.types_at_spans(type_defs, consume);
            }
            Self::Either(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, consume);
                }
            }
            Self::Choice(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, consume);
                }
            }
            Self::Break(_) => {}
            Self::Continue(_) => {}
            Self::Recursive { body, .. } => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Iterative { body, .. } => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Self_(_, _) | Self::DualSelf(_, _) => {}
            Self::Exists(_, _, body) => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Forall(_, _, body) => {
                body.types_at_spans(type_defs, consume);
            }
        }
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
