use crate::primitive::Primitive;
use arcstr::ArcStr;

#[derive(Clone, Debug, PartialEq)]
pub enum Data {
    Unit,
    Pair(Box<Data>, Box<Data>),
    Either(ArcStr, Box<Data>),
    Primitive(Primitive),
}
