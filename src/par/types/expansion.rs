use crate::location::Span;
use crate::par::language::LocalName;
use crate::par::types::visit;
use crate::par::types::{LoopId, Type, TypeDefs, TypeError};
use im::HashSet;

impl Type {
    pub fn expand_definition(&self, type_defs: &TypeDefs) -> Result<Self, TypeError> {
        match self {
            Self::Name(span, name, args) => type_defs.get(span, name, args),
            Self::DualName(span, name, args) => type_defs.get_dual(span, name, args),
            _ => Ok(self.clone()),
        }
    }

    pub fn expand_recursive(
        asc: &HashSet<LoopId>,
        label: &Option<LocalName>,
        body: &Self,
    ) -> Result<Self, TypeError> {
        let mut typ = body.clone();
        fn inner(
            typ: &mut Type,
            target_label: &Option<LocalName>,
            asc: &HashSet<LoopId>,
            body: &Type,
        ) -> Result<(), TypeError> {
            match typ {
                Type::Self_(span, label) if label == target_label => {
                    *typ = Type::Recursive {
                        span: span.clone(),
                        asc: asc.clone(),
                        label: label.clone(),
                        body: Box::new(body.clone()),
                    };
                }
                Type::DualSelf(span, label) if label == target_label => {
                    *typ = Type::Recursive {
                        span: span.clone(),
                        asc: asc.clone(),
                        label: label.clone(),
                        body: Box::new(body.clone()),
                    }
                    .dual(Span::None);
                }
                Type::Recursive { label, .. } | Type::Iterative { label, .. }
                    if label == target_label => {}
                _ => {
                    visit::continue_mut(typ, |child: &mut Type| {
                        inner(child, target_label, asc, body)
                    })?;
                }
            }
            Ok(())
        }

        inner(&mut typ, label, asc, body)?;
        Ok(typ)
    }

    pub fn expand_iterative(
        span: &Span,
        asc: &HashSet<LoopId>,
        label: &Option<LocalName>,
        body: &Self,
    ) -> Result<Self, TypeError> {
        if !asc.is_empty() {
            return Err(TypeError::CannotUnrollAscendantIterative(
                span.clone(),
                label.clone(),
            ));
        }

        Type::expand_iterative_unsafe(span, asc, label, body)
    }

    // This variant does not make sure asc isn't empty
    pub fn expand_iterative_unsafe(
        _span: &Span,
        asc: &HashSet<LoopId>,
        label: &Option<LocalName>,
        body: &Self,
    ) -> Result<Self, TypeError> {
        let mut typ = body.clone();
        fn inner(
            typ: &mut Type,
            target_label: &Option<LocalName>,
            asc: &HashSet<LoopId>,
            body: &Type,
        ) -> Result<(), TypeError> {
            match typ {
                Type::Self_(span, label) if label == target_label => {
                    *typ = Type::Iterative {
                        span: span.clone(),
                        asc: asc.clone(),
                        label: label.clone(),
                        body: Box::new(body.clone()),
                    };
                }
                Type::DualSelf(span, label) if label == target_label => {
                    *typ = Type::Iterative {
                        span: span.clone(),
                        asc: asc.clone(),
                        label: label.clone(),
                        body: Box::new(body.clone()),
                    }
                    .dual(Span::None);
                }
                Type::Recursive { label, .. } | Type::Iterative { label, .. }
                    if label == target_label =>
                {
                    // label is shadowed
                }
                _ => {
                    visit::continue_mut(typ, |child: &mut Type| {
                        inner(child, target_label, asc, body)
                    })?;
                }
            }
            Ok(())
        }

        inner(&mut typ, label, asc, body)?;
        Ok(typ)
    }

    #[allow(dead_code)]
    pub fn expand_fixpoint(&self) -> Result<Self, TypeError> {
        match self {
            Type::Recursive {
                asc, label, body, ..
            } => Self::expand_recursive(asc, label, body),
            Type::Iterative {
                span,
                asc,
                label,
                body,
            } => Self::expand_iterative(span, asc, label, body),
            _ => Ok(self.clone()),
        }
    }

    // This variant does not make sure iterative's asc isn't empty
    pub fn expand_fixpoint_unfounded(&self) -> Result<Self, TypeError> {
        match self {
            Type::Recursive {
                asc, label, body, ..
            } => Self::expand_recursive(asc, label, body),
            Type::Iterative {
                span,
                asc,
                label,
                body,
            } => Self::expand_iterative_unsafe(span, asc, label, body),
            _ => Ok(self.clone()),
        }
    }
}
