//package: core
use std::{future::Future, sync::Arc};

use tokio::sync::Mutex;

use crate::builtin::list::readback_list;
use arcstr::literal;
use im::OrdMap;
use num_bigint::BigInt;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef};

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: "core",
        path: &[],
        module: "BoxMap",
        name: "OfString"
    },
    f: |handle| Box::pin(boxmap_new(handle, Handle::string, Handle::provide_string)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: "core",
        path: &[],
        module: "BoxMap",
        name: "OfBytes"
    },
    f: |handle| Box::pin(boxmap_new(handle, Handle::bytes, Handle::provide_bytes)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: "core",
        path: &[],
        module: "BoxMap",
        name: "OfInt"
    },
    f: |handle| Box::pin(boxmap_new(handle, Handle::int, Handle::provide_int)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: "core",
        path: &[],
        module: "BoxMap",
        name: "OfNat"
    },
    f: |handle| Box::pin(boxmap_new(handle, Handle::nat, Handle::provide_nat)),
});

async fn boxmap_new<K, F>(
    mut handle: Handle,
    read_key: impl Send + Sync + Copy + 'static + Fn(Handle) -> F,
    provide_key: impl Send + Sync + Copy + 'static + Fn(Handle, K),
) where
    K: Ord + Clone + Send + Sync + 'static,
    F: Send + 'static + Future<Output = K>,
{
    let entries = readback_list(handle.receive(), |mut handle| async move {
        let key = read_key(handle.receive()).await;
        let value = Arc::new(Mutex::new(handle.duplicate()));
        (key, value)
    })
    .await;

    let mut map: OrdMap<K, Arc<Mutex<Handle>>> = OrdMap::new();
    for (k, v) in entries {
        map.insert(k, v);
    }

    provide_boxmap(handle, read_key, provide_key, map);
}

fn provide_boxmap<K, F>(
    handle: Handle,
    read_key: impl Send + Sync + Copy + 'static + Fn(Handle) -> F,
    provide_key: impl Send + Sync + Copy + 'static + Fn(Handle, K),
    map: OrdMap<K, Arc<Mutex<Handle>>>,
) where
    K: Ord + Clone + Send + Sync + 'static,
    F: Send + 'static + Future<Output = K>,
{
    handle.provide_box(move |mut handle| {
        let mut map = map.clone();
        async move {
            match handle.case().await.as_str() {
                "size" => {
                    return handle.provide_nat(BigInt::from(map.len()));
                }
                "keys" => {
                    for key in map.keys() {
                        handle.signal(literal!("item"));
                        provide_key(handle.send(), key.clone());
                    }
                    handle.signal(literal!("end"));
                    return handle.break_();
                }
                "list" => {
                    for (key, value) in map.iter() {
                        handle.signal(literal!("item"));
                        let mut pair = handle.send();
                        provide_key(pair.send(), key.clone());
                        pair.link(value.lock().await.duplicate());
                    }
                    handle.signal(literal!("end"));
                    return handle.break_();
                }
                "get" => {
                    let key = read_key(handle.receive()).await;
                    match map.get(&key) {
                        Some(value) => {
                            handle.signal(literal!("ok"));
                            return handle.link(value.lock().await.duplicate());
                        }
                        None => {
                            handle.signal(literal!("err"));
                            return handle.break_();
                        }
                    }
                }
                "put" => {
                    let key = read_key(handle.receive()).await;
                    let value = handle.receive();
                    map.insert(key, Arc::new(Mutex::new(value)));
                    return provide_boxmap(handle, read_key, provide_key, map);
                }
                "delete" => {
                    let key = read_key(handle.receive()).await;
                    map.remove(&key);
                    return provide_boxmap(handle, read_key, provide_key, map);
                }
                _ => unreachable!(),
            }
        }
    })
}
