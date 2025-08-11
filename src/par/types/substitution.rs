use super::super::language::LocalName;
use super::core::Type;
use super::error::TypeError;
use crate::location::{Span, Spanning};
use std::collections::BTreeMap;

impl Type {
    pub fn substitute(self, map: BTreeMap<&LocalName, &Type>) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),
            Self::Var(span, name) => {
                if let Some(&typ) = map.get(&name) {
                    typ.clone()
                } else {
                    Self::Var(span, name)
                }
            }
            Self::DualVar(span, name) => {
                if let Some(&typ) = map.get(&name) {
                    typ.clone().dual(Span::None)
                } else {
                    Self::DualVar(span, name)
                }
            }
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(map.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(map.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Box(span, body) => Self::Box(span, Box::new(body.substitute(map)?)),
            Self::DualBox(span, body) => Self::DualBox(span, Box::new(body.substitute(map)?)),
            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.substitute(map.clone())?),
                Box::new(u.substitute(map)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.substitute(map.clone())?),
                Box::new(u.substitute(map)?),
            ),
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(map.clone())?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(map.clone())?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive {
                span,
                asc,
                label,
                body,
            } => Self::Recursive {
                span,
                asc,
                label,
                body: Box::new(body.substitute(map)?),
            },
            Self::Iterative {
                span,
                asc,
                label,
                body,
            } => Self::Iterative {
                span,
                asc,
                label,
                body: Box::new(body.substitute(map)?),
            },
            Self::Self_(span, label) => Self::Self_(span, label),
            Self::DualSelf(span, label) => Self::DualSelf(span, label),

            Self::Exists(span, mut name, mut body) => {
                while map.values().any(|t| t.contains_var(&name)) {
                    let old_name = name.clone();
                    name.string = arcstr::format!("{}'", name.string);
                    body = Box::new(body.substitute(BTreeMap::from([(
                        &old_name,
                        &Type::Var(name.span(), name.clone()),
                    )]))?);
                }
                let mut map = map;
                map.remove(&name);
                Self::Exists(span, name, Box::new(body.substitute(map)?))
            }
            Self::Forall(span, mut name, mut body) => {
                while map.values().any(|t| t.contains_var(&name)) {
                    let old_name = name.clone();
                    name.string = arcstr::format!("{}'", name.string);
                    body = Box::new(body.substitute(BTreeMap::from([(
                        &old_name,
                        &Type::Var(name.span(), name.clone()),
                    )]))?);
                }
                let mut map = map;
                map.remove(&name);
                Self::Forall(span, name, Box::new(body.substitute(map)?))
            }
        })
    }

    pub fn contains_self(&self, label: &Option<LocalName>) -> bool {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => false,
            Self::Var(_, _) | Self::DualVar(_, _) => false,
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                args.iter().any(|arg| arg.contains_self(label))
            }

            Self::Box(_, body) => body.contains_self(label),
            Self::DualBox(_, body) => body.contains_self(label),

            Self::Pair(_, t, u) => t.contains_self(label) || u.contains_self(label),
            Self::Function(_, t, u) => t.contains_self(label) || u.contains_self(label),
            Self::Either(_, branches) => branches.iter().any(|(_, typ)| typ.contains_self(label)),
            Self::Choice(_, branches) => branches.iter().any(|(_, typ)| typ.contains_self(label)),
            Self::Break(_) => false,
            Self::Continue(_) => false,

            Self::Recursive {
                label: label1,
                body,
                ..
            } => label1 != label && body.contains_self(label),
            Self::Iterative {
                label: label1,
                body,
                ..
            } => label1 != label && body.contains_self(label),
            Self::Self_(_, label1) | Self::DualSelf(_, label1) => label1 == label,

            Self::Exists(_, _, body) => body.contains_self(label),
            Self::Forall(_, _, body) => body.contains_self(label),
        }
    }

    pub fn contains_var(&self, var: &LocalName) -> bool {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => false,
            Self::Var(_, name) | Self::DualVar(_, name) => name == var,
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                args.iter().any(|arg| arg.contains_var(var))
            }

            Self::Box(_, body) | Self::DualBox(_, body) => body.contains_var(var),

            Self::Pair(_, t, u) => t.contains_var(var) || u.contains_var(var),
            Self::Function(_, t, u) => t.contains_var(var) || u.contains_var(var),
            Self::Either(_, branches) => branches.iter().any(|(_, typ)| typ.contains_var(var)),
            Self::Choice(_, branches) => branches.iter().any(|(_, typ)| typ.contains_var(var)),
            Self::Break(_) => false,
            Self::Continue(_) => false,

            Self::Recursive { body, .. } => body.contains_var(var),
            Self::Iterative { body, .. } => body.contains_var(var),
            Self::Self_(_, _) | Self::DualSelf(_, _) => false,

            Self::Exists(_, name, body) => name != var && body.contains_var(var),
            Self::Forall(_, name, body) => name != var && body.contains_var(var),
        }
    }
}
