use crate::location::{Span, Spanning};
use crate::par::language::{GlobalName, LocalName};
use crate::par::types::{LoopId, Operation, Type};
use indexmap::IndexMap;
use miette::LabeledSpan;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum TypeError {
    TypeNameAlreadyDefined(Span, Span, GlobalName),
    NameAlreadyDeclared(Span, Span, GlobalName),
    NameAlreadyDefined(Span, Span, GlobalName),
    DeclaredButNotDefined(Span, GlobalName),
    NoMatchingRecursiveOrIterative(Span),
    SelfUsedInNegativePosition(Span),
    UnguardedRecursiveSelf(Span),
    UnguardedIterativeSelf(Span),
    TypeNameNotDefined(Span, GlobalName),
    TypeVariableNotDefined(Span, LocalName),
    DependencyCycle(Span, Vec<GlobalName>),
    WrongNumberOfTypeArgs(Span, GlobalName, usize, usize),
    GlobalNameNotDefined(Span, GlobalName),
    VariableDoesNotExist(Span, LocalName),
    ShadowedObligation(Span, LocalName),
    TypeMustBeKnownAtThisPoint(Span, #[allow(unused)] LocalName),
    ParameterTypeMustBeKnown(Span, LocalName),
    CannotAssignFromTo(Span, Type, Type),
    UnfulfilledObligations(Span, Vec<LocalName>),
    InvalidOperation(Span, #[allow(unused)] Operation, Type),
    InvalidBranch(Span, LocalName, Type),
    MissingBranch(Span, LocalName, Type),
    RedundantBranch(Span, LocalName, Type),
    TypesCannotBeUnified(Type, Type),
    NoSuchLoopPoint(Span, #[allow(unused)] Option<LocalName>),
    DoesNotDescendSubjectOfBegin(Span, #[allow(unused)] LoopId),
    CannotUnrollAscendantIterative(Span, #[allow(unused)] Option<LocalName>),
    LoopVariableNotPreserved(Span, LocalName),
    LoopVariableChangedType(Span, LocalName, Type, Type),
    CannotUseLinearVariableInBox(Span, LocalName),
    Telltypes(Span, IndexMap<LocalName, Type>),
}

fn two_labels_from_two_spans(
    code: &str,
    span1: &Span,
    span2: &Span,
    label1: impl Into<Option<String>>,
    label2: impl Into<Option<String>>,
) -> Vec<LabeledSpan> {
    use crate::playground::labels_from_span;
    let mut labels = labels_from_span(code, span1);
    let label1 = label1.into();
    let label2 = label2.into();
    labels.iter_mut().for_each(|x| x.set_label(label1.clone()));
    let mut labels2 = labels_from_span(code, span2);
    labels2.iter_mut().for_each(|x| x.set_label(label2.clone()));
    labels.extend(labels2);
    labels
}

impl TypeError {
    pub fn to_report(&self, source_code: Arc<str>) -> miette::Report {
        use crate::playground::labels_from_span;
        let code = &source_code;
        match self {
            Self::TypeNameAlreadyDefined(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already defined here".to_owned()),
                    "Type `{}` is already defined.", name
                )
            }
            Self::NameAlreadyDeclared(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already declared here".to_owned()),
                    "`{}` is already declared.",
                    name,
                )
            }
            Self::NameAlreadyDefined(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already defined here".to_owned()),
                    "`{}` is already defined",
                    name,
                )
            }
            Self::DeclaredButNotDefined(span,  name) => {
                let mut labels = labels_from_span(code, span);
                labels.iter_mut().for_each(|x| {
                    x.set_label(Some("declared here".to_owned()));
                });
                miette::miette!(
                    labels = labels,
                    "`{}` is declared, but is missing a corresponding definition.",
                    name
                )
            }
            Self::NoMatchingRecursiveOrIterative(span) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `self` has no matching `recursive` or `iterative`.",
                )
            }
            Self::SelfUsedInNegativePosition(span) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `self` is used in a negative position.\n\nNegative self-references are not allowed."
                )
            }
            Self::UnguardedRecursiveSelf(span) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This recursive's `self` is not guarded by an either.\n\nUnguarded self references are not allowed."
                )
            },
            Self::UnguardedIterativeSelf(span) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This iterative's `self` is not guarded by a choice.\n\nUnguarded self references are not allowed."
                )
            }
            Self::TypeNameNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type `{}` is not defined.", name)
            }
            Self::TypeVariableNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type variable `{}` is not defined.", name)
            }
            Self::DependencyCycle(span, deps) => {
                let labels = labels_from_span(code, span);
                let mut deps_str = String::new();
                for (i, dep) in deps.iter().enumerate() {
                    if i > 0 {
                        write!(&mut deps_str, " -> ").unwrap();
                    }
                    write!(&mut deps_str, "{}", dep).unwrap();
                }
                miette::miette!(
                    labels = labels,
                    "There is a dependency cycle:\n\n  {}\n\nDependency cycles are not allowed.",
                    deps_str
                )
            }
            Self::WrongNumberOfTypeArgs(span, name, required_number, provided_number) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Type `{}` has {} type arguments, but {} were provided.",
                    name,
                    required_number,
                    provided_number
                )
            }
            Self::GlobalNameNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "`{}` is not defined.", name)
            }
            Self::VariableDoesNotExist(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Variable `{}` does not exist.", name)
            }
            Self::ShadowedObligation(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Cannot re-assign `{}` before handling it.",
                    name,
                )
            }
            Self::TypeMustBeKnownAtThisPoint(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type must be known at this point.")
            }
            Self::ParameterTypeMustBeKnown(span, param) => {
                let labels = labels_from_span(code, span);

                // filter out internal pattern matching variables
                // issue #44: https://github.com/faiface/par-lang/issues/44
                if param.is_match() {
                    miette::miette!(
                        labels = labels,
                        help = "Consider adding a type annotation to the pattern, e.g., [(x : Type)y]",
                        "Type annotation required for pattern matching"
                    )
                } else {
                    miette::miette!(
                        labels = labels,
                        "Type of parameter `{}` must be known.",
                        param,
                    )
                }
            }
            Self::CannotAssignFromTo(span, from_type, to_type) => {
                let labels = labels_from_span(code, span);
                let (mut from_type_str, mut to_type_str) = (String::new(), String::new());
                from_type.pretty(&mut from_type_str, 1).unwrap();
                to_type.pretty(&mut to_type_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "This type was required:\n\n  {}\n\nBut an incompatible type was provided:\n\n  {}\n",
                    to_type_str,
                    from_type_str,
                )
            }
            Self::UnfulfilledObligations(span, names) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Cannot end this process before handling {}.",
                    names
                        .iter()
                        .enumerate()
                        .map(|(i, name)| if i == 0 {
                            format!("`{}`", name)
                        } else {
                            format!(", `{}`", name)
                        })
                        .collect::<String>()
                )
            }
            Self::InvalidOperation(span, _, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "This operation cannot be performed on:\n\n  {}\n",
                    typ_str
                )
            }
            Self::InvalidBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` is not available on:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::MissingBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` was not handled for:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::RedundantBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` is not possible for:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::TypesCannotBeUnified(typ1, typ2) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(
                        code,
                        &typ1.span(),
                        &typ2.span(),
                        "this".to_owned(),
                        "should operate on the same type as this".to_owned()
                    ),
                    "Operations cannot be performed on the same type."
                )
            }
            Self::NoSuchLoopPoint(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "There is no matching loop point in scope.")
            }
            Self::DoesNotDescendSubjectOfBegin(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `loop` may diverge. Value does not descend from the corresponding `begin`.\n\nIf this is intended, use `unfounded`.",
                )
            }
            Self::LoopVariableNotPreserved(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "`{}` is used by next iteration, but is no longer defined.",
                    name,
                )
            }
            Self::LoopVariableChangedType(span, name, loop_type, begin_type) => {
                let labels = labels_from_span(code, span);
                let (mut loop_type_str, mut begin_type_str) = (String::new(), String::new());
                loop_type.pretty(&mut loop_type_str, 1).unwrap();
                begin_type.pretty(&mut begin_type_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "For next iteration, `{}` is required to be:\n\n  {}\n\nBut it has an incompatible type:\n\n  {}\n",
                    name,
                    begin_type_str,
                    loop_type_str,
                )
            }
            Self::CannotUseLinearVariableInBox(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Cannot use linear variable `{}` in a `box` expression.", name)
            }
            Self::Telltypes(span, variables) => {
                let labels = labels_from_span(code, span);
                let mut buf = String::new();
                for (name, typ) in variables {
                    write!(&mut buf, "{}: ", name).unwrap();
                    typ.pretty(&mut buf, 0).unwrap();
                    write!(&mut buf, "\n\n").unwrap();
                }
                miette::miette! {
                    labels = labels,
                    "{}",
                    buf
                }
            }
            Self::CannotUnrollAscendantIterative(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `loop` may diverge. Operating on the loop variable is not allowed.\n\nIf this is intended, use `unfounded`.",
                )

            }
        }.with_source_code(source_code)
    }
}

impl TypeError {
    pub fn spans(&self) -> (Span, Option<Span>) {
        match self {
            Self::TypeNameAlreadyDefined(span1, span2, _)
            | Self::NameAlreadyDeclared(span1, span2, _)
            | Self::NameAlreadyDefined(span1, span2, _) => (span1.clone(), Some(span2.clone())),

            Self::DeclaredButNotDefined(span, _)
            | Self::NoMatchingRecursiveOrIterative(span)
            | Self::SelfUsedInNegativePosition(span)
            | Self::UnguardedRecursiveSelf(span)
            | Self::UnguardedIterativeSelf(span)
            | Self::TypeNameNotDefined(span, _)
            | Self::TypeVariableNotDefined(span, _)
            | Self::DependencyCycle(span, _)
            | Self::WrongNumberOfTypeArgs(span, _, _, _)
            | Self::GlobalNameNotDefined(span, _)
            | Self::VariableDoesNotExist(span, _)
            | Self::ShadowedObligation(span, _)
            | Self::TypeMustBeKnownAtThisPoint(span, _)
            | Self::ParameterTypeMustBeKnown(span, _)
            | Self::CannotAssignFromTo(span, _, _)
            | Self::UnfulfilledObligations(span, _)
            | Self::InvalidOperation(span, _, _)
            | Self::InvalidBranch(span, _, _)
            | Self::MissingBranch(span, _, _)
            | Self::RedundantBranch(span, _, _)
            | Self::NoSuchLoopPoint(span, _)
            | Self::DoesNotDescendSubjectOfBegin(span, _)
            | Self::LoopVariableNotPreserved(span, _)
            | Self::LoopVariableChangedType(span, _, _, _)
            | Self::CannotUseLinearVariableInBox(span, _)
            | Self::Telltypes(span, _)
            | Self::CannotUnrollAscendantIterative(span, _) => (span.clone(), None),

            Self::TypesCannotBeUnified(typ1, typ2) => (typ1.span(), Some(typ2.span())),
        }
    }
}
