use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{
    ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Shared, SyncShared, UserData, Value,
};
use crate::par::primitive::Primitive;
use crate::runtime::new::arena::Arena;
use arcstr::ArcStr;
use std::sync::Arc;

use crate::par::types::TypeDefs;
use crate::Type;
use tokio::sync::oneshot;

#[derive(Debug)]
pub enum Error {
    Cancelled,
    InvalidNode(Node),
    Panicked,
}

pub type Result<T> = core::result::Result<T, Error>;

pub struct Handle {
    net: NetHandle,
    arena: Arc<Arena>,
    node: Option<Node>,
}

impl Handle {
    fn child_handle_with(&self, node: Node) -> Self {
        Self {
            arena: self.arena.clone(),
            net: self.net.clone(),
            node: Some(node),
        }
    }
    pub fn create(net: NetHandle) -> (Node, impl std::future::Future<Output = Result<Node>>) {
        let (tx, rx) = oneshot::channel();

        (Node::Linear(Linear::Request(tx)), async {
            rx.await.map_err(|_| Error::Panicked)
        })
    }

    pub async fn instantiate(
        arena: Arc<Arena>,
        net: NetHandle,
        package: PackagePtr,
    ) -> Result<Handle> {
        let (tx, rx) = oneshot::channel::<(Node, Node)>();
        net.0
            .send(ReducerMessage::Instantiate(package, tx))
            .unwrap();
        let (root, captures) = rx.await.map_err(|_| Error::Panicked).unwrap();
        Ok(Handle {
            net: net,
            node: Some(root),
            arena,
        })
    }
    pub async fn provide_primitive(self, primitive: Primitive) {}
    pub async fn primitive(self) -> Result<Primitive> {
        let primitive = match self.node.unwrap() {
            Node::Global(_, Global::Value(Value::Primitive(p))) => p,
            Node::Linear(Linear::Value(Value::Primitive(p))) => p,
            Node::Shared(Shared::Sync(p)) => match &*p {
                SyncShared::Value(Value::Primitive(p)) => p.clone(),
                _ => return Err(Error::InvalidNode(Node::Shared(Shared::Sync(p)))),
            },
            node => return Err(Error::InvalidNode(node)),
        };
        Ok(primitive)
    }

    pub async fn send(&mut self) -> Result<Self> {
        match core::mem::take(&mut self.node).unwrap() {
            Node::Global(instance, Global::Destruct(GlobalCont::Par(left, right))) => {
                let left = Node::Global(instance.clone(), left.get(&self.arena).clone());
                let right = Node::Global(instance, right.get(&self.arena).clone());

                self.node = Some(left);

                Ok(self.child_handle_with(right))
            }
            node => {
                let (left, left_h) = Self::create(self.net.clone());
                let (right, right_h) = Self::create(self.net.clone());
                let other =
                    Node::Linear(Linear::Value(Value::Pair(Box::new(left), Box::new(right))));
                self.net.0.send(ReducerMessage::Redex(node, other));
                self.node = Some(left_h.await?);
                Ok(self.child_handle_with(right_h.await?))
            }
        }
    }

    pub async fn receive(&mut self) -> Self {
        todo!()
    }

    pub async fn signal(&mut self, chosen: ArcStr) {
        todo!()
    }

    pub async fn case(&mut self) -> ArcStr {
        todo!()
    }

    pub async fn break_(self) {
        todo!()
    }

    pub async fn continue_(self) {
        todo!()
    }
}
