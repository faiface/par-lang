use std::sync::Arc;

use futures::FutureExt;

use par_core::frontend::{Definition, Module, Type, process};

pub(super) fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "BlackBox",
            Type::forall("a", Type::function(Type::var("a"), Type::var("a"))),
            |mut handle| {
                async move {
                    let x = handle.receive();
                    handle.link(x);
                }
                .boxed()
            },
        )],
    }
}
