//! An experimental module adding the MIX axiom, which identifies unit and break.
//! There is no real intention of ever merging it to main, and it is only meant for exploartion of Par with MIX.
use crate::{
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
    runtime::Handle,
};
use std::sync::Arc;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Break",
                Type::function(Type::continue_(), Type::break_()),
                |handle| Box::pin(mix_break(handle)),
            ),
            Definition::external("Continuation", Type::continue_(), |handle| {
                Box::pin(mix_continuation(handle))
            }),
        ],
    }
}

async fn mix_break(mut handle: Handle) {
    // handle the continuation, and return a unit
    handle.receive().break_();
    handle.break_();
}

async fn mix_continuation(handle: Handle) {
    // Create a continuation ex-nihilo
    handle.continue_();
}
