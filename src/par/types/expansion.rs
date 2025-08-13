use crate::location::Span;
use crate::par::language::LocalName;
use crate::par::types::visit;
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
    ) -> Result<Self, TypeError> {
        let mut typ = body.clone();
        fn inner(
            typ: &mut Type,
            target_label: &Option<LocalName>,
            asc: &IndexSet<Option<LocalName>>,
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
        asc: &IndexSet<Option<LocalName>>,
        label: &Option<LocalName>,
        body: &Self,
    ) -> Result<Self, TypeError> {
        if !asc.is_empty() {
            return Err(TypeError::CannotUnrollAscendantIterative(
                span.clone(),
                label.clone(),
            ));
        }

        let mut typ = body.clone();
        fn inner(
            typ: &mut Type,
            target_label: &Option<LocalName>,
            asc: &IndexSet<Option<LocalName>>,
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
}
