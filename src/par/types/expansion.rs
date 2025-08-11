use super::super::language::LocalName;
use super::core::Type;
use super::error::TypeError;
use super::TypeDefs;
use crate::location::Span;
use indexmap::IndexSet;

impl Type {
    pub fn expand_recursive(
        asc: &IndexSet<Option<LocalName>>,
        label: &Option<LocalName>,
        body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        body.clone()
            .expand_recursive_helper(asc, label, body, type_defs)
    }

    fn expand_recursive_helper(
        self,
        top_asc: &IndexSet<Option<LocalName>>,
        top_label: &Option<LocalName>,
        top_body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Box(span, body) => Self::Box(
                span,
                Box::new(body.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::DualBox(span, body) => Self::DualBox(
                span,
                Box::new(body.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),

            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    }
                } else {
                    Self::Self_(span, label)
                }
            }
            Self::DualSelf(span, label) => {
                if &label == top_label {
                    (Self::Recursive {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    })
                    .dual(Span::None)
                } else {
                    Self::DualSelf(span, label)
                }
            }

            Self::Exists(loc, name, t) => Self::Exists(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Forall(loc, name, t) => Self::Forall(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn expand_iterative(
        span: &Span,
        asc: &IndexSet<Option<LocalName>>,
        label: &Option<LocalName>,
        body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        if !asc.is_empty() {
            return Err(TypeError::CannotUnrollAscendantIterative(
                span.clone(),
                label.clone(),
            ));
        }
        body.clone()
            .expand_iterative_helper(asc, label, body, type_defs)
    }

    fn expand_iterative_helper(
        self,
        top_asc: &IndexSet<Option<LocalName>>,
        top_label: &Option<LocalName>,
        top_body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Box(span, body) => Self::Box(
                span,
                Box::new(body.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::DualBox(span, body) => Self::DualBox(
                span,
                Box::new(body.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),

            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    }
                } else {
                    Self::Self_(span, label)
                }
            }
            Self::DualSelf(span, label) => {
                if &label == top_label {
                    (Self::Iterative {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    })
                    .dual(Span::None)
                } else {
                    Self::DualSelf(span, label)
                }
            }

            Self::Exists(loc, name, t) => Self::Exists(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Forall(loc, name, t) => Self::Forall(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn invalidate_ascendent(&mut self, label: &Option<LocalName>) {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => {}
            Self::Var(_, _) | Self::DualVar(_, _) => {}
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                for arg in args {
                    arg.invalidate_ascendent(label);
                }
            }
            Self::Box(_, body) | Self::DualBox(_, body) => {
                body.invalidate_ascendent(label);
            }
            Self::Pair(_, t, u) => {
                t.invalidate_ascendent(label);
                u.invalidate_ascendent(label);
            }
            Self::Function(_, t, u) => {
                t.invalidate_ascendent(label);
                u.invalidate_ascendent(label);
            }
            Self::Either(_, branches) => {
                for (_, t) in branches {
                    t.invalidate_ascendent(label);
                }
            }
            Self::Choice(_, branches) => {
                for (_, t) in branches {
                    t.invalidate_ascendent(label);
                }
            }
            Self::Break(_) => {}
            Self::Continue(_) => {}

            Self::Recursive {
                span: _,
                asc,
                label: _,
                body: t,
            } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Iterative {
                span: _,
                asc,
                label: _,
                body: t,
            } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Self_(_, _) | Self::DualSelf(_, _) => {}

            Self::Exists(_, _, t) => {
                t.invalidate_ascendent(label);
            }
            Self::Forall(_, _, t) => {
                t.invalidate_ascendent(label);
            }
        }
    }

    pub fn expand_definition(&self, type_defs: &TypeDefs) -> Result<Self, TypeError> {
        match self {
            Self::Name(span, name, args) => type_defs.get(span, name, args),
            Self::DualName(span, name, args) => type_defs.get_dual(span, name, args),
            _ => Ok(self.clone()),
        }
    }
}
