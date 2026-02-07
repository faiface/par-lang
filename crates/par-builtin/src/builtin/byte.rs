use std::sync::Arc;

use arcstr::literal;
use num_bigint::BigInt;

use par_core::{
    frontend::{process, Definition, Module, Type, TypeDef},
    runtime::Handle,
};

pub(crate) fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef::external("Byte", &[], Type::byte())],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Equals",
                Type::function(
                    Type::byte(),
                    Type::function(Type::byte(), Type::name(Some("Bool"), "Bool", vec![])),
                ),
                |handle| Box::pin(byte_equals(handle)),
            ),
            Definition::external(
                "Code",
                Type::function(Type::byte(), Type::nat()),
                |handle| Box::pin(byte_code(handle)),
            ),
            Definition::external(
                "Is",
                Type::function(
                    Type::byte(),
                    Type::function(
                        Type::name(None, "Class", vec![]),
                        Type::name(Some("Bool"), "Bool", vec![]),
                    ),
                ),
                |handle| Box::pin(byte_is(handle)),
            ),
        ],
    }
}

async fn byte_equals(mut handle: Handle) {
    let x = handle.receive().byte().await;
    let y = handle.receive().byte().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

async fn byte_code(mut handle: Handle) {
    let c = handle.receive().byte().await;
    handle.provide_nat(BigInt::from(c))
}

async fn byte_is(mut handle: Handle) {
    let b = handle.receive().byte().await;
    let class = ByteClass::readback(handle.receive()).await;
    if class.contains(b) {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

#[derive(Debug, Clone)]
pub(crate) enum ByteClass {
    Any,
    Byte(u8),
    Range(u8, u8),
}

impl ByteClass {
    pub(super) async fn readback(mut handle: Handle) -> Self {
        match handle.case().await.as_str() {
            "any" => Self::Any,
            "byte" => Self::Byte(handle.byte().await),
            "range" => {
                let min = handle.receive().byte().await;
                let max = handle.receive().byte().await;
                handle.continue_();
                Self::Range(min, max)
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn contains(&self, b: u8) -> bool {
        match self {
            Self::Any => true,
            Self::Byte(b1) => b == *b1,
            Self::Range(min, max) => *min <= b && b <= *max,
        }
    }
}
