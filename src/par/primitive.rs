use std::{
    fmt::{self, Write},
    ops::RangeBounds,
};

use bytes::Bytes;
use num_bigint::BigInt;

use super::types::Type;

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(BigInt),
    String(ParString),
    Bytes(Bytes),
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{:?}", s.as_str()),
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
            Self::String(s) if is_single_char(s.as_str()) => Type::char(),
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParString {
    bytes: Bytes,
}

impl ParString {
    pub fn copy_from_slice(slice: &[u8]) -> ParString {
        let _ = std::str::from_utf8(slice).expect("ParString should be UTF8");
        Self {
            bytes: Bytes::copy_from_slice(slice),
        }
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.bytes).expect("ParString should be UTF8")
    }

    pub fn as_bytes(&self) -> Bytes {
        self.bytes.clone()
    }

    pub fn substr(&self, range: impl RangeBounds<usize>) -> Self {
        let bytes = self.bytes.slice(range);
        let _ = std::str::from_utf8(&bytes).expect("ParString should be UTF8");
        Self { bytes }
    }
}

impl<T: Into<Bytes>> From<T> for ParString {
    fn from(value: T) -> Self {
        Self {
            bytes: value.into(),
        }
    }
}
