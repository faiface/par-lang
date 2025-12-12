use std::{future::Future, sync::Arc};

use tokio::sync::Mutex;

use crate::{
    par::{
        builtin::list::readback_list,
        process,
        program::{Definition, Module},
        types::Type,
    },
    runtime::Handle,
};
use arcstr::literal;
use futures::future::BoxFuture;
use im::OrdMap;
use num_bigint::BigInt;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            // BoxMap.String : [type v] [List<(String) box v>] BoxMap<String, v>
            Definition::external(
                "String",
                Type::forall(
                    "v",
                    Type::function(
                        Type::name(
                            Some("List"),
                            "List",
                            vec![Type::pair(Type::string(), Type::box_(Type::var("v")))],
                        ),
                        Type::name(None, "BoxMap", vec![Type::string(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(boxmap_new(handle, Handle::string, Handle::provide_string)),
            ),
            // BoxMap.Bytes : [type v] [List<(Bytes) box v>] BoxMap<Bytes, v>
            Definition::external(
                "Bytes",
                Type::forall(
                    "v",
                    Type::function(
                        Type::name(
                            Some("List"),
                            "List",
                            vec![Type::pair(Type::bytes(), Type::box_(Type::var("v")))],
                        ),
                        Type::name(None, "BoxMap", vec![Type::bytes(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(boxmap_new(handle, Handle::bytes, Handle::provide_bytes)),
            ),
            // BoxMap.Int : [type v] [List<(Int) box v>] BoxMap<Int, v>
            Definition::external(
                "Int",
                Type::forall(
                    "v",
                    Type::function(
                        Type::name(
                            Some("List"),
                            "List",
                            vec![Type::pair(Type::int(), Type::box_(Type::var("v")))],
                        ),
                        Type::name(None, "BoxMap", vec![Type::int(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(boxmap_new(handle, Handle::int, Handle::provide_int)),
            ),
            // BoxMap.Nat : [type v] [List<(Nat) box v>] BoxMap<Nat, v>
            Definition::external(
                "Nat",
                Type::forall(
                    "v",
                    Type::function(
                        Type::name(
                            Some("List"),
                            "List",
                            vec![Type::pair(Type::nat(), Type::box_(Type::var("v")))],
                        ),
                        Type::name(None, "BoxMap", vec![Type::nat(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(boxmap_new(handle, Handle::nat, Handle::provide_nat)),
            ),
        ],
    }
}

async fn boxmap_new<K, F, G>(
    mut handle: Handle,
    read_key: impl Send + Sync + Copy + 'static + Fn(Handle) -> F,
    provide_key: impl Send + Sync + Copy + 'static + Fn(Handle, K) -> G,
) where
    K: Ord + Clone + Send + Sync + 'static,
    F: Send + 'static + Future<Output = K>,
    G: Send + 'static + Future<Output = ()>,
{
    let entries = readback_list(handle.receive().await, |mut handle| async {
        let key = read_key(handle.receive().await).await;
        let value = Arc::new(Mutex::new(handle));
        (key, value)
    })
    .await;

    let mut map: OrdMap<K, Arc<Mutex<Handle>>> = OrdMap::new();
    for (k, v) in entries {
        map.insert(k, v);
    }

    provide_boxmap(handle, read_key, provide_key, map).await;
}

fn provide_boxmap<K, F, G>(
    handle: Handle,
    read_key: impl Send + Sync + Copy + 'static + Fn(Handle) -> F,
    provide_key: impl Send + Sync + Copy + 'static + Fn(Handle, K) -> G,
    map: OrdMap<K, Arc<Mutex<Handle>>>,
) -> BoxFuture<'static, ()>
where
    K: Ord + Clone + Send + Sync + 'static,
    F: Send + 'static + Future<Output = K>,
    G: Send + 'static + Future<Output = ()>,
{
    Box::pin(handle.provide_box(move |mut handle| {
        let mut map = map.clone();
        async move {
            match handle.case().await.as_str() {
                "size" => {
                    return handle.provide_nat(BigInt::from(map.len())).await;
                }
                "keys" => {
                    for key in map.keys() {
                        handle.signal(literal!("item")).await;
                        provide_key(handle.send().await, key.clone()).await;
                    }
                    handle.signal(literal!("end")).await;
                    return handle.break_().await;
                }
                "list" => {
                    for (key, value) in map.iter() {
                        handle.signal(literal!("item")).await;
                        let mut pair = handle.send().await;
                        provide_key(pair.send().await, key.clone()).await;
                        pair.link(value.lock().await.duplicate().await).await;
                    }
                    handle.signal(literal!("end")).await;
                    return handle.break_().await;
                }
                "get" => {
                    let key = read_key(handle.receive().await).await;
                    match map.get(&key) {
                        Some(value) => {
                            handle.signal(literal!("ok")).await;
                            return handle.link(value.lock().await.duplicate().await).await;
                        }
                        None => {
                            handle.signal(literal!("err")).await;
                            return handle.break_().await;
                        }
                    }
                }
                "put" => {
                    let key = read_key(handle.receive().await).await;
                    let value = handle.receive().await;
                    map.insert(key, Arc::new(Mutex::new(value)));
                    return provide_boxmap(handle, read_key, provide_key, map).await;
                }
                "delete" => {
                    let key = read_key(handle.receive().await).await;
                    map.remove(&key);
                    return provide_boxmap(handle, read_key, provide_key, map).await;
                }
                _ => unreachable!(),
            }
        }
    }))
}
