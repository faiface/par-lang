//package: core
use arcstr::literal;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Data",
        name: "ToString"
    },
    f: |handle| Box::pin(data_to_string(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Data",
        name: "Compare"
    },
    f: |handle| Box::pin(data_compare(handle)),
});

async fn data_to_string(mut handle: Handle) {
    let value = handle.receive().data().await;
    handle.provide_string(value.to_string().into());
}

async fn data_compare(mut handle: Handle) {
    let left = handle.receive().data().await;
    let right = handle.receive().data().await;
    match left.cmp(&right) {
        std::cmp::Ordering::Less => handle.signal(literal!("less")),
        std::cmp::Ordering::Equal => handle.signal(literal!("equal")),
        std::cmp::Ordering::Greater => handle.signal(literal!("greater")),
    }
    handle.break_();
}
