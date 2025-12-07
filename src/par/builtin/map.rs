use std::{collections::BTreeMap, future::Future};

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
use num_bigint::BigInt;
use std::sync::Arc;

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            // Map.String : [type v] [List<(String) box v>] Map<String, v>
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
                        Type::name(None, "Map", vec![Type::string(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(map_new(handle, Handle::string, Handle::provide_string)),
            ),
            // Map.Int : [type v] [List<(Int) box v>] Map<Int, v>
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
                        Type::name(None, "Map", vec![Type::int(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(map_new(handle, Handle::int, Handle::provide_int)),
            ),
            // Map.Nat : [type v] [List<(Nat) box v>] Map<Nat, v>
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
                        Type::name(None, "Map", vec![Type::nat(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(map_new(handle, Handle::nat, Handle::provide_nat)),
            ),
        ],
    }
}

async fn map_new<K: Clone + Ord, F: Future<Output = K>, G: Send + 'static + Future<Output = ()>>(
    mut handle: Handle,
    read_key: impl Fn(Handle) -> F,
    provide_key: impl Fn(Handle, K) -> G,
) {
    let entries = readback_list(handle.receive().await, |mut handle| async {
        let key = read_key(handle.receive().await).await;
        let value = handle;
        (key, value)
    })
    .await;

    let mut map: BTreeMap<K, Handle> = BTreeMap::new();
    for (key, value) in entries {
        if let Some(old) = map.insert(key, value) {
            old.erase().await;
        }
    }
    provide_map(handle, map, read_key, provide_key).await;
}

async fn provide_map<
    K: Clone + Ord,
    F: Future<Output = K>,
    G: Send + 'static + Future<Output = ()>,
>(
    mut handle: Handle,
    mut map: BTreeMap<K, Handle>,
    read_key: impl Fn(Handle) -> F,
    provide_key: impl Fn(Handle, K) -> G,
) {
    loop {
        match handle.case().await.as_str() {
            "size" => {
                handle
                    .send()
                    .await
                    .provide_nat(BigInt::from(map.len()))
                    .await;
                continue;
            }
            "keys" => {
                let mut keys = handle.send().await;
                for key in map.keys() {
                    keys.signal(literal!("item")).await;
                    provide_key(keys.send().await, key.clone()).await;
                }
                keys.signal(literal!("end")).await;
                keys.break_().await;
                continue;
            }
            "list" => {
                for (key, value) in map.into_iter() {
                    handle.signal(literal!("item")).await;
                    let mut pair = handle.send().await;
                    provide_key(pair.send().await, key).await;
                    pair.link(value).await;
                }
                handle.signal(literal!("end")).await;
                return handle.break_().await;
            }
            "entry" => {
                let key = read_key(handle.receive().await).await;
                let removed = map.remove(&key);
                handle.send().await.concurrently(|mut handle| async move {
                    match removed {
                        Some(value) => {
                            handle.signal(literal!("ok")).await;
                            handle.link(value).await;
                        }
                        None => {
                            handle.signal(literal!("err")).await;
                            handle.break_().await;
                        }
                    }
                });
                match handle.case().await.as_str() {
                    "put" => {
                        let new_value = handle.receive().await;
                        map.insert(key, new_value);
                    }
                    "delete" => {}
                    _ => unreachable!(),
                }
                continue;
            }
            _ => unreachable!(),
        }
    }
}
