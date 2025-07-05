use std::fmt::{self, Write};

use arcstr::Substr;
use num_bigint::BigInt;

use super::types::Type;

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(BigInt),
    String(Substr),
    Char(char),
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{:?}", s),
            Self::Char(c) => write!(f, "{:?}", c),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(n) if n >= &BigInt::ZERO => Type::nat(),
            Self::Int(_) => Type::int(),
            Self::String(_) => Type::string(),
            Self::Char(_) => Type::char(),
        }
    }
}
