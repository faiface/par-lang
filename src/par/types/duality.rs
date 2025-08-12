use super::super::language::LocalName;
use super::core::Type;
use crate::location::Span;

impl Type {
    pub fn dual(self, span0: Span) -> Self {
        match self {
            Self::Primitive(span, p) => Self::DualPrimitive(span0.join(span), p),
            Self::DualPrimitive(span, p) => Self::Primitive(span0.join(span), p),

            Self::Var(span, name) => Self::DualVar(span0.join(span), name),
            Self::DualVar(span, name) => Self::Var(span0.join(span), name),

            Self::Name(span, name, args) => Self::DualName(span0.join(span), name, args),
            Self::DualName(span, name, args) => Self::Name(span0.join(span), name, args),

            Self::Box(span, body) => Self::DualBox(span, body),
            Self::DualBox(span, body) => Self::Box(span, body),

            Self::Pair(span, t, u) => {
                Self::Function(span0.join(span), t, Box::new(u.dual(Span::None)))
            }
            Self::Function(span, t, u) => {
                Self::Pair(span0.join(span), t, Box::new(u.dual(Span::None)))
            }
            Self::Either(span, branches) => Self::Choice(
                span0.join(span),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dual(Span::None)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Self::Either(
                span0.join(span),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch.clone(), t.dual(Span::None)))
                    .collect(),
            ),
            Self::Break(span) => Self::Continue(span0.join(span)),
            Self::Continue(span) => Self::Break(span0.join(span)),

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                let body = Box::new(t.dual(Span::None).dualize_self(&label));
                Self::Iterative {
                    span: span0.join(span),
                    asc,
                    label,
                    body,
                }
            }
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                let body = Box::new(t.dual(Span::None).dualize_self(&label));
                Self::Recursive {
                    span: span0.join(span),
                    asc,
                    label,
                    body,
                }
            }
            Self::Self_(span, label) => Self::DualSelf(span0.join(span), label),
            Self::DualSelf(span, label) => Self::Self_(span0.join(span), label),

            Self::Exists(span, name, t) => {
                Self::Forall(span0.join(span), name, Box::new(t.dual(Span::None)))
            }
            Self::Forall(span, name, t) => {
                Self::Exists(span0.join(span), name, Box::new(t.dual(Span::None)))
            }
        }
    }

    fn dualize_self(self, label: &Option<LocalName>) -> Self {
        match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.dualize_self(label))
                    .collect(),
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.dualize_self(label))
                    .collect(),
            ),

            Self::Box(span, body) => Self::Box(span, Box::new(body.dualize_self(label))),
            Self::DualBox(span, body) => Self::DualBox(span, Box::new(body.dualize_self(label))),

            Self::Pair(loc, t, u) => Self::Pair(
                loc.clone(),
                Box::new(t.dualize_self(label)),
                Box::new(u.dualize_self(label)),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc.clone(),
                Box::new(t.dualize_self(label)),
                Box::new(u.dualize_self(label)),
            ),
            Self::Either(span, branches) => Self::Either(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dualize_self(label)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Self::Choice(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dualize_self(label)))
                    .collect(),
            ),
            Self::Break(span) => Self::Break(span.clone()),
            Self::Continue(span) => Self::Continue(span.clone()),

            Self::Recursive {
                span,
                asc,
                label: label1,
                body: t,
            } => {
                if &label1 == label {
                    Self::Recursive {
                        span,
                        asc,
                        label: label1,
                        body: t,
                    }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label: label1,
                        body: Box::new(t.dualize_self(label)),
                    }
                }
            }
            Self::Iterative {
                span,
                asc,
                label: label1,
                body: t,
            } => {
                if &label1 == label {
                    Self::Iterative {
                        span,
                        asc,
                        label: label1,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label: label1,
                        body: Box::new(t.dualize_self(label)),
                    }
                }
            }
            Self::Self_(span, label1) => {
                if &label1 == label {
                    Self::DualSelf(span, label1)
                } else {
                    Self::Self_(span, label1)
                }
            }
            Self::DualSelf(span, label1) => {
                if &label1 == label {
                    Self::Self_(span, label1)
                } else {
                    Self::DualSelf(span, label1)
                }
            }

            Self::Exists(loc, name, t) => {
                Self::Exists(loc.clone(), name.clone(), Box::new(t.dualize_self(label)))
            }
            Self::Forall(loc, name, t) => {
                Self::Forall(loc.clone(), name.clone(), Box::new(t.dualize_self(label)))
            }
        }
    }
}
