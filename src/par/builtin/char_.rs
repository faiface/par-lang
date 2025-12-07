use std::sync::Arc;

use arcstr::literal;
use num_bigint::BigInt;

use crate::{
    par::{
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
    runtime::Handle,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef::external("Char", &[], Type::char())],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Equals",
                Type::function(
                    Type::char(),
                    Type::function(Type::char(), Type::name(Some("Bool"), "Bool", vec![])),
                ),
                |handle| Box::pin(char_equals(handle)),
            ),
            Definition::external(
                "Code",
                Type::function(Type::char(), Type::nat()),
                |handle| Box::pin(char_code(handle)),
            ),
            Definition::external(
                "Is",
                Type::function(
                    Type::char(),
                    Type::function(
                        Type::name(None, "Class", vec![]),
                        Type::name(Some("Bool"), "Bool", vec![]),
                    ),
                ),
                |handle| Box::pin(char_is(handle)),
            ),
        ],
    }
}

async fn char_equals(mut handle: Handle) {
    let x = handle.receive().await.char().await;
    let y = handle.receive().await.char().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_().await;
}

async fn char_code(mut handle: Handle) {
    let c = handle.receive().await.char().await;
    handle.provide_nat(BigInt::from(c as u32)).await;
}

async fn char_is(mut handle: Handle) {
    let ch = handle.receive().await.char().await;
    let class = CharClass::readback(handle.receive().await).await;
    if class.contains(ch) {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_().await;
}

#[derive(Debug, Clone)]
pub enum CharClass {
    Any,
    Char(char),
    Whitespace,
    AsciiAny,
    AsciiAlpha,
    AsciiAlphanum,
    AsciiDigit,
}

impl CharClass {
    pub async fn readback(mut handle: Handle) -> Self {
        match handle.case().await.as_str() {
            "any" => Self::Any,
            "ascii" => match handle.case().await.as_str() {
                "alpha" => Self::AsciiAlpha,
                "alphanum" => Self::AsciiAlphanum,
                "any" => Self::AsciiAny,
                "digit" => Self::AsciiDigit,
                _ => unreachable!(),
            },
            "char" => Self::Char(handle.char().await),
            "whitespace" => Self::Whitespace,
            _ => unreachable!(),
        }
    }

    pub fn contains(&self, ch: char) -> bool {
        match self {
            Self::Any => true,
            Self::Char(ch1) => ch == *ch1,
            Self::Whitespace => ch.is_whitespace(),
            Self::AsciiAny => ch.is_ascii(),
            Self::AsciiAlpha => ch.is_ascii_alphabetic(),
            Self::AsciiAlphanum => ch.is_ascii_alphanumeric(),
            Self::AsciiDigit => ch.is_ascii_digit(),
        }
    }
}
