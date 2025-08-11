use super::super::language::{GlobalName, LocalName};
use crate::location::{Span, Spanning};
use arcstr::ArcStr;
use indexmap::IndexSet;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Send,
    Receive,
    Signal,
    Case,
    Break,
    Continue,
    Begin,
    Loop,
    SendType,
    ReceiveType,
}

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(Span, PrimitiveType),
    DualPrimitive(Span, PrimitiveType),
    Var(Span, LocalName),
    DualVar(Span, LocalName),
    Name(Span, GlobalName, Vec<Self>),
    DualName(Span, GlobalName, Vec<Self>),
    Box(Span, Box<Self>),
    DualBox(Span, Box<Self>),
    Pair(Span, Box<Self>, Box<Self>),
    Function(Span, Box<Self>, Box<Self>),
    Either(Span, BTreeMap<LocalName, Self>),
    Choice(Span, BTreeMap<LocalName, Self>),
    Break(Span),
    Continue(Span),
    Recursive {
        span: Span,
        // The ascendents of the type (denoted by the names of the respective loop points):
        // If you `begin` on a `recursive`, and it expands, so its `self`s get replaced by new
        // `recursive`s, these new `recursive`s will have as their *ascendent* the original `recursive`.
        // This is for totality checking.
        asc: IndexSet<Option<LocalName>>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Iterative {
        span: Span,
        asc: IndexSet<Option<LocalName>>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Self_(Span, Option<LocalName>),
    DualSelf(Span, Option<LocalName>),
    Exists(Span, LocalName, Box<Self>),
    Forall(Span, LocalName, Box<Self>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Nat,
    Int,
    String,
    Char,
    Byte,
    Bytes,
}

#[allow(unused)]
impl Type {
    pub fn nat() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Nat)
    }

    pub fn int() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Int)
    }

    pub fn string() -> Self {
        Self::Primitive(Span::None, PrimitiveType::String)
    }

    pub fn char() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Char)
    }

    pub fn byte() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Byte)
    }

    pub fn bytes() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Bytes)
    }

    pub fn name(module: Option<&'static str>, primary: &'static str, args: Vec<Self>) -> Self {
        Self::Name(Span::None, GlobalName::external(module, primary), args)
    }

    pub fn var(name: &'static str) -> Self {
        Self::Var(
            Span::None,
            LocalName {
                span: Span::None,
                string: ArcStr::from(name),
            },
        )
    }

    pub fn box_(t: Self) -> Self {
        Self::Box(Span::None, Box::new(t))
    }

    pub fn pair(t: Self, u: Self) -> Self {
        Self::Pair(Span::None, Box::new(t), Box::new(u))
    }

    pub fn function(t: Self, u: Self) -> Self {
        Self::Function(Span::None, Box::new(t), Box::new(u))
    }

    pub fn either(branches: Vec<(&'static str, Self)>) -> Self {
        Self::Either(
            Span::None,
            branches
                .into_iter()
                .map(|(name, typ)| {
                    (
                        LocalName {
                            span: Span::None,
                            string: ArcStr::from(name),
                        },
                        typ,
                    )
                })
                .collect(),
        )
    }

    pub fn choice(branches: Vec<(&'static str, Self)>) -> Self {
        Self::Choice(
            Span::None,
            branches
                .into_iter()
                .map(|(name, typ)| {
                    (
                        LocalName {
                            span: Span::None,
                            string: ArcStr::from(name),
                        },
                        typ,
                    )
                })
                .collect(),
        )
    }

    pub fn break_() -> Self {
        Self::Break(Span::None)
    }

    pub fn continue_() -> Self {
        Self::Continue(Span::None)
    }

    pub fn recursive(label: Option<&'static str>, body: Self) -> Self {
        Self::Recursive {
            span: Span::None,
            asc: IndexSet::new(),
            label: label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
            body: Box::new(body),
        }
    }

    pub fn iterative(label: Option<&'static str>, body: Self) -> Self {
        Self::Iterative {
            span: Span::None,
            asc: IndexSet::new(),
            label: label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
            body: Box::new(body),
        }
    }

    pub fn self_(label: Option<&'static str>) -> Self {
        Self::Self_(
            Span::None,
            label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
        )
    }

    pub fn qualify(&mut self, module: Option<&str>) {
        match self {
            Self::Primitive(span, _) | Self::DualPrimitive(span, _) => *span = Span::None,
            Self::Var(span, _) | Self::DualVar(span, _) => *span = Span::None,
            Self::Name(span, name, args) | Self::DualName(span, name, args) => {
                *span = Span::None;
                name.qualify(module);
                for arg in args {
                    arg.qualify(module);
                }
            }
            Self::Box(span, body) | Self::DualBox(span, body) => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Pair(span, t, u) => {
                *span = Span::None;
                t.qualify(module);
                u.qualify(module);
            }
            Self::Function(span, t, u) => {
                *span = Span::None;
                t.qualify(module);
                u.qualify(module);
            }
            Self::Either(span, branches) => {
                *span = Span::None;
                for (_, typ) in branches {
                    typ.qualify(module);
                }
            }
            Self::Choice(span, branches) => {
                *span = Span::None;
                for (_, typ) in branches {
                    typ.qualify(module);
                }
            }
            Self::Break(span) => *span = Span::None,
            Self::Continue(span) => *span = Span::None,
            Self::Recursive { span, body, .. } => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Iterative { span, body, .. } => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Self_(span, _) | Self::DualSelf(span, _) => *span = Span::None,
            Self::Exists(span, _, body) => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Forall(span, _, body) => {
                *span = Span::None;
                body.qualify(module);
            }
        }
    }
}

impl Spanning for Type {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(span, _)
            | Self::DualPrimitive(span, _)
            | Self::Var(span, _)
            | Self::DualVar(span, _)
            | Self::Name(span, _, _)
            | Self::DualName(span, _, _)
            | Self::Box(span, _)
            | Self::DualBox(span, _)
            | Self::Pair(span, _, _)
            | Self::Function(span, _, _)
            | Self::Either(span, _)
            | Self::Choice(span, _)
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Self_(span, _)
            | Self::DualSelf(span, _)
            | Self::Exists(span, _, _)
            | Self::Forall(span, _, _) => span.clone(),
            Self::Recursive { span, .. } | Self::Iterative { span, .. } => span.clone(),
        }
    }
}
