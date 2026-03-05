use futures::FutureExt;

use par_runtime::registry::{DefinitionRef, ExternalDef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: "core",
        path: &[],
        module: "Bench",
        name: "BlackBox"
    },
    f: |mut handle| {
        async move {
            let x = handle.receive();
            handle.link(x);
        }
        .boxed()
    },
});
