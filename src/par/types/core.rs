use super::super::language::{GlobalName, LocalName};
use crate::location::{Span, Spanning};
use crate::par::types::visit::Polarity;
use crate::par::types::{visit, TypeDefs, TypeError};
use arcstr::ArcStr;
use indexmap::IndexSet;
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LoopId(u64);

static NEXT_LOOP_ID: AtomicU64 = AtomicU64::new(0);

impl LoopId {
    pub fn new() -> Self {
        let id = NEXT_LOOP_ID.fetch_add(1, Ordering::SeqCst);
        Self(id)
    }
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
        // The ascendents of the type (denoted by unique IDs generated for each loop):
        // If you `begin` on a `recursive`, and it expands, so its `self`s get replaced by new
        // `recursive`s, these new `recursive`s will have as their *ascendent* the original `recursive`.
        // This is for totality checking.
        asc: IndexSet<LoopId>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Iterative {
        span: Span,
        asc: IndexSet<LoopId>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Self_(Span, Option<LocalName>),
    DualSelf(Span, Option<LocalName>),
    Exists(Span, LocalName, Box<Self>),
    Forall(Span, LocalName, Box<Self>),
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

    pub fn forall(var: &'static str, body: Self) -> Self {
        Self::Forall(
            Span::None,
            LocalName {
                span: Span::None,
                string: ArcStr::from(var),
            },
            Box::new(body),
        )
    }
}

impl Type {
    pub fn qualify(&mut self, module: Option<&str>) {
        fn inner(typ: &mut Type, module: Option<&str>) -> Result<(), ()> {
            match typ {
                Type::Name(_, name, args) | Type::DualName(_, name, args) => {
                    name.qualify(module);
                    for arg in args {
                        inner(arg, module)?;
                    }
                }
                _ => {
                    visit::continue_mut(typ, |child: &mut Type| inner(child, module))?;
                }
            }
            Ok(())
        }
        inner(self, module).unwrap();
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
            | Self::Recursive { span, .. }
            | Self::Iterative { span, .. }
            | Self::Self_(span, _)
            | Self::DualSelf(span, _)
            | Self::Exists(span, _, _)
            | Self::Forall(span, _, _) => span.clone(),
        }
    }
}

impl Type {
    pub fn remove_asc(&mut self, defs: &TypeDefs) -> Result<(), TypeError> {
        fn inner(typ: &mut Type, polarity: Polarity, defs: &TypeDefs) -> Result<(), TypeError> {
            match (typ, polarity) {
                (Type::Recursive { asc, body, .. }, Polarity::Positive | Polarity::Neither)
                | (Type::Iterative { asc, body, .. }, Polarity::Negative | Polarity::Neither) => {
                    asc.clear();
                    inner(body, polarity, defs)?;
                }
                (
                    Type::Iterative {
                        span, asc, body, ..
                    },
                    Polarity::Positive | Polarity::Both,
                )
                | (
                    Type::Recursive {
                        span, asc, body, ..
                    },
                    Polarity::Negative | Polarity::Both,
                ) => {
                    if !asc.is_empty() {
                        return Err(TypeError::CannotUnrollAscendantIterative(
                            span.clone(),
                            None,
                        ));
                    }
                    inner(body, polarity, defs)?;
                }
                (typ, polarity) => visit::continue_mut_polarized(
                    typ,
                    polarity,
                    defs,
                    |child: &mut Type, polarity| inner(child, polarity, defs),
                )?,
            }
            Ok(())
        }
        inner(self, Polarity::Positive, defs)
    }
}
