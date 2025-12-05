use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{
    ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Shared, SyncShared, UserData, Value,
};
use crate::par::primitive::Primitive;
use crate::runtime::new::arena::Arena;
use crate::runtime::new::runtime::{GlobalValue, Linker};
use arcstr::ArcStr;
use std::sync::Arc;
use tokio::sync::watch::Receiver;

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

pub enum HandleNode {
    Empty,
    Present(Node),
    Waiting(oneshot::Receiver<Node>),
}

impl HandleNode {
    fn linked_pair() -> (Node, Self) {
        let (tx, rx) = oneshot::channel();

        (Node::Linear(Linear::Request(tx)), HandleNode::Waiting(rx))
    }

    async fn take(&mut self) -> Node {
        match core::mem::replace(self, HandleNode::Empty) {
            HandleNode::Empty => panic!("Tried to take out empty node!"),
            HandleNode::Present(node) => node,
            HandleNode::Waiting(receiver) => receiver.await.expect("Sender dropped!"),
        }
    }
    fn put(&mut self, node: Node) {
        *self = Self::Present(node);
    }
}

impl From<Node> for HandleNode {
    fn from(value: Node) -> Self {
        Self::Present(value)
    }
}
impl From<oneshot::Receiver<Node>> for HandleNode {
    fn from(value: oneshot::Receiver<Node>) -> Self {
        Self::Waiting(value)
    }
}

pub struct Handle {
    net: NetHandle,
    arena: Arc<Arena>,
    node: HandleNode,
}

impl Handle {
    fn child_handle_with(&self, node: HandleNode) -> Self {
        Self {
            arena: self.arena.clone(),
            net: self.net.clone(),
            node: node,
        }
    }
    pub fn from_node(arena: Arc<Arena>, net: NetHandle, node: Node) -> Self {
        Self {
            arena,
            net,
            node: HandleNode::Present(node),
        }
    }

    pub async fn from_package(
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
            node: root.into(),
            arena,
        })
    }

    pub async fn link_with(mut self, mut dual: Handle) {
        let this = self.node.take().await;
        self.link(this, dual.node.take().await);
    }

    pub async fn provide_external(mut self, ext: ExternalFn) {
        // TODO add fast variant.
        let node = self.node.take().await;
        self.link(node, Node::Linear(Linear::Value(Value::External(ext))));
    }
    pub async fn provide_primitive(mut self, primitive: Primitive) {
        // TODO add fast variant.
        let node = self.node.take().await;
        self.link(
            node,
            Node::Linear(Linear::Value(Value::Primitive(primitive))),
        );
    }
    pub async fn primitive(mut self) -> Result<Primitive> {
        let primitive = match self.node.take().await {
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
        match self.node.take().await {
            /* TODO fast path:
            Node::Global(instance, Global::Destruct(GlobalCont::Par(left, right))) => {
                let left = Node::Global(instance.clone(), left.get(&self.arena).clone());
                let right = Node::Global(instance, right.get(&self.arena).clone());

                self.node = Some(left);

                Ok(self.child_handle_with(right))
            }
            */
            node => {
                let (left, left_h) = HandleNode::linked_pair();
                let (right, right_h) = HandleNode::linked_pair();
                let other =
                    Node::Linear(Linear::Value(Value::Pair(Box::new(left), Box::new(right))));
                self.link(node, other);
                self.node = left_h;
                Ok(self.child_handle_with(right_h))
            }
        }
    }

    pub async fn receive(&mut self) -> Self {
        let node = self.node.take().await;
        let Value::Pair(a, b) = self.destruct(node).unwrap() else {
            unreachable!()
        };
        self.node = a.into();
        self.child_handle_with(b.into())
    }

    pub async fn signal(&mut self, chosen: ArcStr) -> Result<()> {
        match self.node.take().await {
            // TODO fast path
            node => {
                let (payload, payload_h) = HandleNode::linked_pair();
                let other = Node::Linear(Linear::Value(Value::Either(chosen, Box::new(payload))));
                self.link(node, other);
                self.node = payload_h;
                Ok(())
            }
        }
    }

    pub async fn case(&mut self) -> ArcStr {
        let node = self.node.take().await;
        let Value::Either(name, payload) = self.destruct(node).unwrap() else {
            unreachable!()
        };
        self.node = payload.into();
        name
    }

    pub async fn break_(mut self) -> Result<()> {
        match self.node.take().await {
            Node::Global(_, Global::Destruct(GlobalCont::Continue)) => Ok(()),
            node => {
                let other = Node::Linear(Linear::Value(Value::Break));
                self.link(node, other);
                Ok(())
            }
        }
    }

    pub async fn continue_(mut self) -> Result<()> {
        let node = self.node.take().await;
        let Value::Break = self.destruct(node).unwrap() else {
            unreachable!()
        };
        Ok(())
    }
}

impl Linker for Handle {
    fn arena(&self) -> Arc<Arena> {
        self.arena.clone()
    }
    fn link(&mut self, a: Node, b: Node) {
        self.net.0.send(ReducerMessage::Redex(a, b)).unwrap()
    }
}
