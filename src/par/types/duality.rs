use super::super::language::LocalName;
use super::core::Type;
use crate::location::Span;
use crate::par::types::visit;

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

            Self::Pair(span, t, u, vars) => {
                Self::Function(span0.join(span), t, Box::new(u.dual(Span::None)), vars)
            }
            Self::Function(span, t, u, vars) => {
                Self::Pair(span0.join(span), t, Box::new(u.dual(Span::None)), vars)
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

            Type::Hole(span, name, hole) => Type::DualHole(span0.join(span), name, hole),
            Type::DualHole(span, name, hole) => Type::Hole(span0.join(span), name, hole),
        }
    }

    fn dualize_self(mut self, label: &Option<LocalName>) -> Self {
        fn inner(typ: &mut Type, target_label: &Option<LocalName>) -> Result<(), ()> {
            match typ {
                Type::Self_(span, label) if label == target_label => {
                    *typ = Type::DualSelf(span.clone(), label.clone());
                }
                Type::DualSelf(span, label) if label == target_label => {
                    *typ = Type::Self_(span.clone(), label.clone());
                }
                Type::Recursive { label, .. } | Type::Iterative { label, .. }
                    if label == target_label =>
                {
                    // our label is shadowed
                }
                _ => {
                    visit::continue_mut(typ, |child: &mut Type| inner(child, target_label))?;
                }
            }
            Ok(())
        }
        inner(&mut self, label).unwrap();
        self
    }
}
