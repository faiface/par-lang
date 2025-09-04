use std::{collections::BTreeMap, future::Future};

use crate::{
    icombs::readback::Handle,
    par::{
        builtin::list::readback_list,
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use arcstr::literal;
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

async fn map_new<K: Ord, F: Future<Output = K>>(
    mut handle: Handle,
    read_key: impl Fn(Handle) -> F,
    provide_key: fn(Handle, K),
) {
    let entries = readback_list(handle.receive(), |mut handle| async {
        let key = read_key(handle.receive()).await;
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

async fn provide_map<K: Ord, F: Future<Output = K>>(
    mut handle: Handle,
    mut map: BTreeMap<K, Handle>,
    read_key: impl Fn(Handle) -> F,
    provide_key: fn(Handle, K),
) {
    loop {
        match handle.case().await.as_str() {
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
            "put" => {
                let key = read_key(handle.receive()).await;
                let value = handle.receive();
                match map.insert(key, value) {
                    Some(old) => {
                        let mut res = handle.send();
                        res.signal(literal!("err"));
                        res.link(old);
                    }
                    None => {
                        let mut res = handle.send();
                        res.signal(literal!("ok"));
                        res.break_();
                    }
                }
                continue;
            }
            "delete" => {
                let key = read_key(handle.receive()).await;
                match map.remove(&key) {
                    Some(old) => {
                        let mut res = handle.send();
                        res.signal(literal!("ok"));
                        res.link(old);
                    }
                    None => {
                        let mut res = handle.send();
                        res.signal(literal!("err"));
                        res.break_();
                    }
                }
                continue;
            }
            "get" => {
                let key = read_key(handle.receive()).await;
                let default_box = handle.receive();
                let value = match map.remove(&key) {
                    Some(value) => {
                        default_box.erase();
                        value
                    }
                    None => default_box,
                };
                handle.send().link(value);
                match handle.case().await.as_str() {
                    "put" => {
                        let new_value = handle.receive();
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
