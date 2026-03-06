//package: core
use std::{collections::BTreeMap, future::Future};

use crate::builtin::list::readback_list;
use arcstr::literal;
use num_bigint::BigInt;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("core"),
        path: &[],
        module: "Map",
        name: "OfString"
    },
    f: |handle| Box::pin(map_new(handle, Handle::string, Handle::provide_string)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("core"),
        path: &[],
        module: "Map",
        name: "OfBytes"
    },
    f: |handle| Box::pin(map_new(handle, Handle::bytes, Handle::provide_bytes)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("core"),
        path: &[],
        module: "Map",
        name: "OfInt"
    },
    f: |handle| Box::pin(map_new(handle, Handle::int, Handle::provide_int)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Package("core"),
        path: &[],
        module: "Map",
        name: "OfNat"
    },
    f: |handle| Box::pin(map_new(handle, Handle::nat, Handle::provide_nat)),
});

async fn map_new<K: Clone + Ord, F: Future<Output = K>>(
    mut handle: Handle,
    read_key: impl Fn(Handle) -> F,
    provide_key: impl Fn(Handle, K),
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
                let key = read_key(handle.receive()).await;
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
