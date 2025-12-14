use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Value};
use crate::par::primitive::Primitive;
use crate::runtime::new::arena::Arena;
use crate::runtime::new::runtime::Linker;
use arcstr::ArcStr;
use futures::task::FutureObj;
use std::future::Future;
use std::sync::Arc;

use tokio::sync::oneshot;

#[derive(Debug)]
pub enum Error {
    InvalidNode(Node),
    InvalidValue(Value<Node>),
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
        let (root, _captures) = rx.await.map_err(|_| Error::Panicked).unwrap();
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
        self.link(node, Node::Linear(Linear::Value(Value::ExternalFn(ext))));
    }

    pub fn concurrently<F>(self, f: impl FnOnce(Self) -> F)
    where
        F: 'static + Send + Future<Output = ()>,
    {
        self.net
            .0
            .clone()
            .send(ReducerMessage::Spawn(FutureObj::from(Box::new(f(self)))))
            .unwrap();
    }
    pub async fn provide_external_closure<Fun, Fut>(mut self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        let node = self.node.take().await;
        self.link(
            node,
            Node::Linear(Linear::Value(Value::ExternalArc(
                super::runtime::ExternalArc(Arc::new(move |x| match x {
                    crate::runtime::Handle::New(x) => Box::pin(f(x)),
                    _ => panic!("Mixed runtime handles"),
                })),
            ))),
        );
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
        let primitive = match self.try_destruct().await {
            Value::Primitive(p) => p,
            node => return Err(Error::InvalidValue(node)),
        };
        Ok(primitive)
    }

    pub async fn send(&mut self) -> Self {
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
                self.child_handle_with(right_h)
            }
        }
    }

    pub async fn receive(&mut self) -> Self {
        let Value::Pair(a, b) = self.try_destruct().await else {
            unreachable!()
        };
        self.node = a.into();
        self.child_handle_with(b.into())
    }

    pub async fn signal(&mut self, chosen: ArcStr) {
        match self.node.take().await {
            // TODO fast path
            node => {
                let (payload, payload_h) = HandleNode::linked_pair();
                let chosen = self
                    .arena
                    .interned(chosen.as_str())
                    .expect("Sending a non-interned string!");
                let other = Node::Linear(Linear::Value(Value::Either(chosen, Box::new(payload))));
                self.link(node, other);
                self.node = payload_h;
            }
        }
    }

    pub async fn case(&mut self) -> ArcStr {
        let Value::Either(name, payload) = self.try_destruct().await else {
            unreachable!()
        };
        self.node = payload.into();
        self.arena.get(name).into()
    }

    pub async fn break_(mut self) -> () {
        match self.node.take().await {
            Node::Global(_, Global::Destruct(GlobalCont::Continue)) => (),
            node => {
                let other = Node::Linear(Linear::Value(Value::Break));
                self.link(node, other);
                ()
            }
        }
    }

    pub async fn continue_(mut self) -> () {
        let Value::Break = self.try_destruct().await else {
            unreachable!()
        };
        ()
    }
    pub async fn erase(self) -> () {
        todo!()
    }
    pub async fn duplicate(&mut self) -> Handle {
        todo!()
    }

    async fn retry(&mut self) {
        let (tx, rx) = oneshot::channel::<Node>();
        let mut old_node = core::mem::replace(&mut self.node, rx.into());
        let old_node = old_node.take().await;
        self.link(old_node, Node::Linear(Linear::Request(tx)));
    }

    async fn try_destruct(&mut self) -> Value<Node> {
        loop {
            let node = self.node.take().await;
            match self.destruct(node) {
                Ok(v) => {
                    return v;
                }
                Err(node) => {
                    self.node = node.into();
                    self.retry().await;
                }
            }
        }
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
