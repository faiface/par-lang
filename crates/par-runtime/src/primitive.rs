use std::{
    cmp::Ordering,
    fmt::{self, Write},
    ops::RangeBounds,
};

use bytes::Bytes;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

fn scan_digit_run(input: &str, start: usize) -> Option<usize> {
    let bytes = input.as_bytes();
    if !matches!(bytes.get(start), Some(b'0'..=b'9')) {
        return None;
    }

    let mut idx = start + 1;
    while let Some(&byte) = bytes.get(idx) {
        match byte {
            b'0'..=b'9' => idx += 1,
            b'_' if matches!(bytes.get(idx + 1), Some(b'0'..=b'9')) => idx += 1,
            _ => break,
        }
    }

    Some(idx)
}

fn parse_decimal_float(input: &str) -> Option<f64> {
    let bytes = input.as_bytes();
    let mut idx = 0;

    if matches!(bytes.get(idx), Some(b'+' | b'-')) {
        idx += 1;
    }

    idx = scan_digit_run(input, idx)?;
    if !matches!(bytes.get(idx), Some(b'.')) {
        return None;
    }
    idx += 1;
    idx = scan_digit_run(input, idx)?;

    if matches!(bytes.get(idx), Some(b'e' | b'E')) {
        idx += 1;
        if matches!(bytes.get(idx), Some(b'+' | b'-')) {
            idx += 1;
        }
        idx = scan_digit_run(input, idx)?;
    }

    if idx != bytes.len() {
        return None;
    }

    let normalized: String = input.chars().filter(|&c| c != '_').collect();
    normalized.parse::<f64>().ok()
}

pub fn parse_float_text(input: &str) -> Option<f64> {
    match input {
        "NaN" => Some(f64::NAN),
        "Inf" | "Infinity" => Some(f64::INFINITY),
        "-Inf" | "-Infinity" => Some(f64::NEG_INFINITY),
        _ => parse_decimal_float(input),
    }
}

pub fn format_float(value: f64) -> String {
    if value.is_nan() {
        return String::from("NaN");
    }
    if value == f64::INFINITY {
        return String::from("Inf");
    }
    if value == f64::NEG_INFINITY {
        return String::from("-Inf");
    }

    let mut text = format!("{value:?}");
    if let Some(exp_idx) = text.find(['e', 'E']) {
        if !text[..exp_idx].contains('.') {
            text.insert_str(exp_idx, ".0");
        }
    } else if !text.contains('.') {
        text.push_str(".0");
    }
    text
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Primitive {
    Zero,
    Int(BigInt),
    Float(f64),
    String(ParString),
    Bytes(Bytes),
}

#[derive(Clone, Debug)]
pub enum Number {
    Zero,
    Int(BigInt),
    Float(f64),
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Number {}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Zero, Self::Zero) => Ordering::Equal,
            (Self::Int(left), Self::Int(right)) => left.cmp(right),
            (Self::Float(left), Self::Float(right)) => left.total_cmp(right),
            (left, right) => number_kind_rank(left).cmp(&number_kind_rank(right)),
        }
    }
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Zero => write!(f, "0"),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(value) => write!(f, "{}", format_float(*value)),
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

    #[cfg(feature = "playground")]
    pub fn pretty_string(&self) -> String {
        let mut buf = String::new();
        self.pretty(&mut buf, 0).unwrap();
        buf
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Primitive {}

impl PartialOrd for Primitive {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Primitive {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Zero, Self::Zero) => Ordering::Equal,
            (Self::Int(left), Self::Int(right)) => left.cmp(right),
            (Self::Float(left), Self::Float(right)) => left.total_cmp(right),
            (Self::String(left), Self::String(right)) => left.as_str().cmp(right.as_str()),
            (Self::Bytes(left), Self::Bytes(right)) => left.cmp(right),
            (left, right) => primitive_kind_rank(left).cmp(&primitive_kind_rank(right)),
        }
    }
}

fn primitive_kind_rank(primitive: &Primitive) -> u8 {
    match primitive {
        Primitive::Zero => 0,
        Primitive::Int(_) => 1,
        Primitive::Float(_) => 2,
        Primitive::String(_) => 3,
        Primitive::Bytes(_) => 4,
    }
}

fn number_kind_rank(number: &Number) -> u8 {
    match number {
        Number::Zero => 0,
        Number::Int(_) => 1,
        Number::Float(_) => 2,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
