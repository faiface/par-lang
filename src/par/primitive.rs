use std::fmt::{self, Write};

use arcstr::Substr;
use byteview::ByteView;
use num_bigint::BigInt;

use super::types::Type;

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(BigInt),
    String(Substr),
    Bytes(ByteView),
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{:?}", s),
            Self::Bytes(b) => {
                write!(f, "<<")?;
                for (i, &byte) in b.as_ref().iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", byte as i64)?;
                }
                write!(f, ">>")
            }
        }
    }

    pub fn pretty_string(&self) -> String {
        let mut buf = String::new();
        self.pretty(&mut buf, 0).unwrap();
        buf
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(n) if n >= &BigInt::ZERO => Type::nat(),
            Self::Int(_) => Type::int(),
            Self::String(s) if is_single_char(s) => Type::char(),
            Self::String(_) => Type::string(),
            Self::Bytes(b) if b.len() == 1 => Type::byte(),
            Self::Bytes(_) => Type::bytes(),
        }
    }
}

fn is_single_char(string: &str) -> bool {
    let mut chars = string.chars();
    chars.next().is_some() && chars.next().is_none()
}
