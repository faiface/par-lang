use std::sync::Arc;

use crate::{
    runtime::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Log",
            Type::function(Type::string(), Type::break_()),
            |handle| Box::pin(debug_log(handle)),
        )],
    }
}

async fn debug_log(mut handle: Handle) {
    let string = handle.receive().string().await;
    println!("{}", string.as_str());
    handle.break_();
}
