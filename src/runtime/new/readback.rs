use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{Value, PackagePtr};
use tokio::sync::oneshot;
use crate::par::types::TypeDefs;
use crate::Type;

pub struct Handle {
    type_defs: TypeDefs,
    net: NetHandle,
    value: Value,
    t: Type,
}

impl Handle {
    pub async fn instantiate(net: NetHandle, package: PackagePtr, t: Type, type_defs: TypeDefs) -> Handle {
        let (tx, rx) = oneshot::channel();
        net.0.send(ReducerMessage::Instantiate(package, tx)).unwrap();
        let (root, captures) = rx.await.unwrap();
        Handle {
            net: net,
            value: root,
            t,
            type_defs
        }
    }
}
