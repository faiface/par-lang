use crate::frontend_impl::language::GlobalName;
use crate::frontend_impl::process::HoverInfo;
use crate::frontend_impl::program::Docs;
use crate::frontend_impl::types::core::NamedTypeDisplay;
use crate::frontend_impl::types::{PrimitiveType, Type, TypeDefs};
use crate::location::{Span, Spanning};
use std::fmt;
use std::fmt::Write;

impl<S: Clone> Type<S>
where
    GlobalName<S>: fmt::Display,
{
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        if let Some(display_hint) = self.display_hint() {
            return write_named_type_display(f, display_hint, indent, false);
        }

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

            Self::Pair(_, arg, then, vars) => {
                let mut then = then;
                if !vars.is_empty() {
                    write!(f, "<")?;
                    write!(f, "{}", vars[0])?;
                    for var in vars.iter().skip(1) {
                        write!(f, ", {}", var)?;
                    }
                    write!(f, ">")?;
                    write!(f, "(")?;
                    arg.pretty(f, indent)?;
                } else {
                    write!(f, "(")?;
                    arg.pretty(f, indent)?;
                    while let Self::Pair(_, arg, next, vars) = then.as_ref() {
                        if !vars.is_empty() {
                            break;
                        }
                        write!(f, ", ")?;
                        arg.pretty(f, indent)?;
                        then = next;
                    }
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty(f, indent)
                }
            }

            Self::Function(_, param, then, vars) => {
                let mut then = then;
                if !vars.is_empty() {
                    write!(f, "<")?;
                    write!(f, "{}", vars[0])?;
                    for var in vars.iter().skip(1) {
                        write!(f, ", {}", var)?;
                    }
                    write!(f, ">")?;
                    write!(f, "[")?;
                    param.pretty(f, indent)?;
                } else {
                    write!(f, "[")?;
                    param.pretty(f, indent)?;
                    while let Self::Function(_, arg, next, vars) = then.as_ref() {
                        if !vars.is_empty() {
                            break;
                        }
                        write!(f, ", ")?;
                        arg.pretty(f, indent)?;
                        then = next;
                    }
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
                    write!(f, "@{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if let Some(label) = label {
                    write!(f, "@{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "@{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "@{}", label)?;
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
            Type::Hole(_, name, _) => write!(f, "%{}", name),
            Type::DualHole(_, name, _) => write!(f, "dual %{}", name),
            Type::Fail(_) => write!(f, "<error>"),
        }
    }

    pub fn pretty_compact(&self, f: &mut impl Write) -> fmt::Result {
        if let Some(display_hint) = self.display_hint() {
            return write_named_type_display(f, display_hint, 0, true);
        }

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

            Self::Pair(_, arg, then, vars) => {
                let mut then = then;
                if !vars.is_empty() {
                    write!(f, "<")?;
                    write!(f, "{}", vars[0])?;
                    for var in vars.iter().skip(1) {
                        write!(f, ", {}", var)?;
                    }
                    write!(f, ">")?;
                    write!(f, "(")?;
                    arg.pretty_compact(f)?;
                } else {
                    write!(f, "(")?;
                    arg.pretty_compact(f)?;
                    while let Self::Pair(_, arg, next, vars) = then.as_ref() {
                        if !vars.is_empty() {
                            break;
                        }
                        write!(f, ", ")?;
                        arg.pretty_compact(f)?;
                        then = next;
                    }
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Function(_, param, then, vars) => {
                let mut then = then;
                if !vars.is_empty() {
                    write!(f, "<")?;
                    write!(f, "{}", vars[0])?;
                    for var in vars.iter().skip(1) {
                        write!(f, ", {}", var)?;
                    }
                    write!(f, ">")?;
                    write!(f, "[")?;
                    param.pretty_compact(f)?;
                } else {
                    write!(f, "[")?;
                    param.pretty_compact(f)?;
                    while let Self::Function(_, arg, next, vars) = then.as_ref() {
                        if !vars.is_empty() {
                            break;
                        }
                        write!(f, ", ")?;
                        arg.pretty_compact(f)?;
                        then = next;
                    }
                }
                if let Self::Continue(_) = then.as_ref() {
                    write!(f, "]?")
                } else {
                    write!(f, "] ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Either(_, branches) => {
                write!(f, "either {{")?;
                for (branch, typ) in branches {
                    write!(f, ".{} ", branch)?;
                    typ.pretty_compact(f)?;
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }

            Self::Choice(_, branches) => {
                write!(f, "choice {{")?;
                for (branch, typ) in branches {
                    write!(f, ".{} => ", branch)?;
                    typ.pretty_compact(f)?;
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { label, body, .. } => {
                write!(f, "recursive")?;
                if !matches!(body.as_ref(), Self::Either(..)) {
                    if let Some(label) = label {
                        write!(f, "@{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if !matches!(body.as_ref(), Self::Choice(..)) {
                    if let Some(label) = label {
                        write!(f, "@{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "@{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "@{}", label)?;
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
            Type::Hole(_, name, _) => write!(f, "%{}", name),
            Type::DualHole(_, name, _) => write!(f, "dual %{}", name),
            Type::Fail(_) => write!(f, "<error>"),
        }
    }

    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs<S>,
        docs: &Docs<S>,
        consume: &mut impl FnMut(Span, HoverInfo<S>),
    ) where
        S: Eq + std::hash::Hash,
    {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => {}
            Self::Var(_, _) | Self::DualVar(_, _) => {}
            Self::Name(span, name, args) => {
                let (def_span, typ) = type_defs
                    .get_with_span(span, name, args)
                    .unwrap_or_else(|_| (&Span::None, self.clone()));
                consume(
                    span.clone(),
                    HoverInfo::type_instantiation(
                        name.clone(),
                        args.clone(),
                        typ,
                        docs.type_doc(name).cloned(),
                        def_span.clone(),
                    ),
                );
                for arg in args {
                    arg.types_at_spans(type_defs, docs, consume);
                }
            }
            Self::DualName(span, name, args) => {
                let (def_span, typ) =
                    type_defs
                        .get_with_span(span, name, args)
                        .unwrap_or_else(|_| {
                            (
                                &Span::None,
                                Type::Name(span.clone(), name.clone(), args.clone()),
                            )
                        });
                consume(
                    dual_name_hover_span(span, name),
                    HoverInfo::type_instantiation(
                        name.clone(),
                        args.clone(),
                        typ,
                        docs.type_doc(name).cloned(),
                        def_span.clone(),
                    ),
                );

                let (_dual_def_span, dual_typ) = type_defs
                    .get_dual_with_span(span, name, args)
                    .unwrap_or_else(|_| (&Span::None, self.clone()));
                consume(
                    dual_keyword_hover_span(span, name),
                    HoverInfo::unnamed(dual_typ),
                );

                for arg in args {
                    arg.types_at_spans(type_defs, docs, consume);
                }
            }
            Self::Box(_, body) | Self::DualBox(_, body) => {
                body.types_at_spans(type_defs, docs, consume)
            }
            Self::Pair(_, t, u, _) => {
                t.types_at_spans(type_defs, docs, consume);
                u.types_at_spans(type_defs, docs, consume);
            }
            Self::Function(_, t, u, _) => {
                t.types_at_spans(type_defs, docs, consume);
                u.types_at_spans(type_defs, docs, consume);
            }
            Self::Either(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, docs, consume);
                }
            }
            Self::Choice(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, docs, consume);
                }
            }
            Self::Break(_) => {}
            Self::Continue(_) => {}
            Self::Recursive { body, .. } => {
                body.types_at_spans(type_defs, docs, consume);
            }
            Self::Iterative { body, .. } => {
                body.types_at_spans(type_defs, docs, consume);
            }
            Self::Self_(_, _) | Self::DualSelf(_, _) => {}
            Self::Exists(_, _, body) => {
                body.types_at_spans(type_defs, docs, consume);
            }
            Self::Forall(_, _, body) => {
                body.types_at_spans(type_defs, docs, consume);
            }
            Type::Hole(_, _, _) => {}
            Type::DualHole(_, _, _) => {}
            Type::Fail(_) => {}
        }
    }
}

fn write_named_type_display<S: Clone>(
    f: &mut impl Write,
    display_hint: &NamedTypeDisplay<S>,
    indent: usize,
    compact: bool,
) -> fmt::Result
where
    GlobalName<S>: fmt::Display,
{
    if display_hint.dual {
        write!(f, "dual ")?;
    }
    write!(f, "{}", display_hint.name)?;
    if display_hint.args.is_empty() {
        return Ok(());
    }

    write!(f, "<")?;
    for (i, arg) in display_hint.args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        if compact {
            arg.pretty_compact(f)?;
        } else {
            arg.pretty(f, indent)?;
        }
    }
    write!(f, ">")
}

fn dual_name_hover_span<S>(full_span: &Span, name: &GlobalName<S>) -> Span {
    match (name.span().start(), full_span.end(), full_span.file()) {
        (Some(start), Some(end), Some(file)) => Span::At { start, end, file },
        _ => name.span(),
    }
}

fn dual_keyword_hover_span<S>(full_span: &Span, name: &GlobalName<S>) -> Span {
    match (full_span.start(), name.span().start(), full_span.file()) {
        (Some(start), Some(end), Some(file)) if start.offset < end.offset => {
            Span::At { start, end, file }
        }
        _ => Span::None,
    }
}

impl<S: Clone> fmt::Display for Type<S>
where
    GlobalName<S>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_compact(f)
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
