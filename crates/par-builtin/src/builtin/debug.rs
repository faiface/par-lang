use std::sync::Arc;

use par_core::{
    frontend::{process, Definition, Module, Type},
    runtime::Handle,
};

pub(crate) fn external_module() -> Module<Arc<process::Expression<()>>> {
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
    eprintln!("{}", string.as_str());
    handle.break_();
}
