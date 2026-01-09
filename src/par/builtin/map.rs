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
            // Map.Bytes : [type v] [List<(Bytes) box v>] Map<Bytes, v>
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
                        Type::name(None, "Map", vec![Type::bytes(), Type::var("v")]),
                    ),
                ),
                |handle| Box::pin(map_new(handle, Handle::bytes, Handle::provide_bytes)),
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

async fn map_new<K: Clone + Ord, F: Future<Output = K>>(
    mut handle: Handle,
    read_key: impl Fn(Handle) -> F,
    provide_key: impl Fn(Handle, K),
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
            old.erase();
        }
    }
    provide_map(handle, map, read_key, provide_key).await;
}

async fn provide_map<K: Clone + Ord, F: Future<Output = K>>(
    mut handle: Handle,
    mut map: BTreeMap<K, Handle>,
    read_key: impl Fn(Handle) -> F,
    provide_key: impl Fn(Handle, K),
) {
    loop {
        match handle.case().await.as_str() {
            "size" => {
                handle.send().provide_nat(BigInt::from(map.len()));
                continue;
            }
            "keys" => {
                let mut keys = handle.send();
                for key in map.keys() {
                    keys.signal(literal!("item"));
                    provide_key(keys.send(), key.clone());
                }
                keys.signal(literal!("end"));
                keys.break_();
                continue;
            }
            "list" => {
                for (key, value) in map.into_iter() {
                    handle.signal(literal!("item"));
                    let mut pair = handle.send();
                    provide_key(pair.send(), key);
                    pair.link(value);
                }
                handle.signal(literal!("end"));
                return handle.break_();
            }
            "entry" => {
                let key = read_key(handle.receive().await).await;
                let removed = map.remove(&key);
                handle.send().concurrently(|mut handle| async move {
                    match removed {
                        Some(value) => {
                            handle.signal(literal!("ok"));
                            handle.link(value);
                        }
                        None => {
                            handle.signal(literal!("err"));
                            handle.break_();
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
