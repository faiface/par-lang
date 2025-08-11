use crate::location::Span;
use crate::par::language::LocalName;
use crate::par::types::{Type, TypeDefs, TypeError};
use indexmap::IndexSet;

impl Type {
    pub fn expand_definition(&self, type_defs: &TypeDefs) -> Result<Self, TypeError> {
        match self {
            Self::Name(span, name, args) => type_defs.get(span, name, args),
            Self::DualName(span, name, args) => type_defs.get_dual(span, name, args),
            _ => Ok(self.clone()),
        }
    }

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
}
