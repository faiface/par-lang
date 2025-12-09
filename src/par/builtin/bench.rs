use std::sync::Arc;

use futures::{future::BoxFuture, FutureExt};
use tokio::sync::Mutex;

use crate::{
    location::Span,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
    runtime::Handle,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "BlackBox",
            Type::forall("a", Type::function(Type::var("a"), Type::var("a"))),
            |mut handle| {
                async move {
                    let x = handle.receive().await;
                    handle.link(x).await;
                }
                .boxed()
            },
        )],
    }
}
