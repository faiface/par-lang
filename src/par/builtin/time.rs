use crate::{
    icombs::readback::Handle,
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
        definitions: vec![Definition::external("Now", Type::nat(), |handle| {
            Box::pin(time_now(handle))
        })],
    }
}

async fn time_now(mut handle: Handle) {
    // return current time in milliseconds since epoch
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64;
    handle.provide_nat(BigInt::from(now));
}
