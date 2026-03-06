use num_bigint::BigInt;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

async fn time_now(mut handle: Handle) {
    // return current time in milliseconds since epoch
    handle.receive().continue_();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();
    handle.provide_nat(BigInt::from(now));
}

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("basic"),
        path: &[],
        module: "Time",
        name: "Now"
    },
    f: |handle| Box::pin(time_now(handle)),
});
