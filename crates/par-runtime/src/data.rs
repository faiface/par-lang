use crate::primitive::Primitive;
use arcstr::ArcStr;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Data {
    Unit,
    Either(ArcStr, Box<Data>),
    Pair(Box<Data>, Box<Data>),
    Primitive(Primitive),
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "!"),
            Self::Pair(left, right) => write!(f, "({left}) {right}"),
            Self::Either(label, payload) => write!(f, ".{label} {payload}"),
            Self::Primitive(primitive) => primitive.pretty(f, 0),
        }
    }
}
