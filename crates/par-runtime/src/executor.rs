use crate::flat::arena::Arena;
use crate::flat::reducer::Reducer;
use crate::flat::runtime::{PackagePtr, Runtime};
use crate::flat::stats::Rewrites;
use crate::readback::Handle;
use futures::future::RemoteHandle;
use futures::task::SpawnExt;
use std::sync::Arc;

pub async fn start_and_instantiate(
    arena: Arc<Arena>,
    package: PackagePtr,
) -> (Handle, RemoteHandle<Rewrites>) {
    let (reducer, net_handle) = Reducer::from(Runtime::from(arena.clone()));
    let spawner = reducer.spawner();
    let reducer_future = reducer.spawn_reducer();
    let handle =
        crate::flat::readback::Handle::from_package(arena.clone(), net_handle, package).unwrap();
    (
        Handle::from(handle),
        spawner
            .spawn_with_handle(async move {
                let reducer = reducer_future.await;
                reducer.runtime.rewrites
            })
            .unwrap(),
    )
}
