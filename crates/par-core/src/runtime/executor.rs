use futures::future::RemoteHandle;
use futures::task::SpawnExt;
use crate::frontend::language::GlobalName;
use crate::runtime::{Handle, Rewrites};
use crate::runtime_impl::Compiled;
use crate::runtime_impl::flat::reducer::Reducer;
use crate::runtime_impl::flat::runtime::Runtime;

pub async fn start_and_instantiate(
    compiled: &Compiled,
    name: &GlobalName,
) -> (Handle, RemoteHandle<Rewrites>) {
    let (reducer, net_handle) = Reducer::from(Runtime::from(compiled.code.arena.clone()));
    let spawner = reducer.spawner();
    let reducer_future = reducer.spawn_reducer();
    (
        Handle::from(compiled.code.instantiate(net_handle, name).unwrap()),
        spawner
            .spawn_with_handle(async move {
                let reducer = reducer_future.await;
                reducer.runtime.rewrites
            })
            .unwrap(),
    )
}
