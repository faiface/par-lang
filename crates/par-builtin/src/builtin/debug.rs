use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

async fn debug_log(mut handle: Handle) {
    let string = handle.receive().string().await;
    eprintln!("{}", string.as_str());
    handle.break_();
}

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("basic"),
        path: &[],
        module: "Debug",
        name: "Log"
    },
    f: |handle| { Box::pin(debug_log(handle)) },
});
