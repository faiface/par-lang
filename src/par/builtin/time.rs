use crate::{
    runtime::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use num_bigint::BigInt;
use std::sync::Arc;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Now",
            Type::function(Type::break_(), Type::nat()),
            |handle| Box::pin(time_now(handle)),
        )],
    }
}

async fn time_now(mut handle: Handle) {
    // return current time in milliseconds since epoch
    handle.receive().continue_();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();
    handle.provide_nat(BigInt::from(now));
}
