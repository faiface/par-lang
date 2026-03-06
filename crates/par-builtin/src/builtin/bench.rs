use futures::FutureExt;

use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("core"),
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
