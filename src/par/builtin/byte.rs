use std::sync::Arc;

use arcstr::literal;
use num_bigint::BigInt;

use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
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
