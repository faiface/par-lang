use std::{future::Future, sync::{Arc, Mutex}};

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
use im::OrdMap;

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

async fn boxmap_new<K, F>(
    mut handle: Handle,
    read_key: impl Send + Sync + Copy + 'static + Fn(Handle) -> F,
    provide_key: impl Send + Sync + Copy + 'static + Fn(Handle, K),
) where
    K: Ord + Clone + Send + Sync + 'static,
    F: Send + 'static + Future<Output = K>,
{
    let entries = readback_list(handle.receive(), |mut handle| async {
        let key = read_key(handle.receive()).await;
        let value = Arc::new(Mutex::new(handle));
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
            loop {
                match handle.case().await.as_str() {
                    "list" => {
                        for (k, v) in map.iter() {
                            handle.signal(literal!("item"));
                            let mut pair = handle.send();
                            provide_key(pair.send(), k.clone());
                            pair.link(v.lock().unwrap().duplicate());
                        }
                        handle.signal(literal!("end"));
                        return handle.break_();
                    }
                    "get" => {
                        let key = read_key(handle.receive()).await;
                        match map.get(&key) {
                            Some(value) => {
                                handle.signal(literal!("ok"));
                                return handle.link(value.lock().unwrap().duplicate());
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
        }
    });
}