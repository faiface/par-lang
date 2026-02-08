use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Value};
use crate::par::primitive::Primitive;
use crate::runtime_impl::flat::arena::Arena;
use crate::runtime_impl::flat::runtime::Linker;
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

pub(crate) type Result<T> = core::result::Result<T, Error>;

pub(crate) enum HandleNode {
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
    fn new(&self, node: HandleNode) -> Self {
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

    pub fn from_package(arena: Arc<Arena>, net: NetHandle, package: PackagePtr) -> Result<Handle> {
        let (node, handle) = HandleNode::linked_pair();
        let mut handle = Handle {
            net: net,
            node: handle,
            arena,
        };
        let root = handle.instantiate_package_captures(
            package,
            Node::Linear(Linear::Value(Box::new(Value::Break))),
        );
        handle.link(root, node);
        Ok(handle)
    }

    pub fn link_with(self, mut dual: Handle) {
        self.concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(node, dual.node.take().await);
        });
    }

    pub fn provide_external(self, ext: ExternalFn) {
        // TODO add fast variant.
        self.concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(
                node,
                Node::Linear(Linear::Value(Box::new(Value::ExternalFn(ext)))),
            );
        });
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

    pub async fn await_ready(mut self) -> Self {
        let node = self.node.take().await;
        self.node = HandleNode::Present(node);
        self
    }

    pub fn provide_external_closure<Fun, Fut>(self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        self.concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(
                node,
                Node::Linear(Linear::Value(Box::new(Value::ExternalArc(
                    super::runtime::ExternalArc(Arc::new(move |handle| Box::pin(f(handle.handle)))),
                )))),
            );
        });
    }

    pub fn provide_primitive(self, primitive: Primitive) {
        // TODO add fast variant.
        self.concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(
                node,
                Node::Linear(Linear::Value(Box::new(Value::Primitive(primitive)))),
            );
        });
    }

    pub async fn primitive(mut self) -> Result<Primitive> {
        let primitive = match self.try_destruct().await {
            Value::Primitive(p) => p,
            node => return Err(Error::InvalidValue(node)),
        };
        Ok(primitive)
    }

    pub fn send(&mut self) -> Self {
        let (left, left_h) = HandleNode::linked_pair();
        let (right, right_h) = HandleNode::linked_pair();
        let par = core::mem::replace(&mut self.node, left_h);
        let times = Node::Linear(Linear::Value(Box::new(Value::Pair(left, right))));
        self.new(par).concurrently(|mut this| async move {
            let par = this.node.take().await;
            this.link(par, times);
        });
        self.new(right_h)
    }

    pub fn receive(&mut self) -> Self {
        let (left, left_h) = HandleNode::linked_pair();
        let (right, right_h) = HandleNode::linked_pair();
        let times = core::mem::replace(&mut self.node, left_h);
        self.new(times).concurrently(|mut this| async move {
            let Value::Pair(a, b) = this.try_destruct().await else {
                unreachable!();
            };
            this.link(left, a);
            this.link(right, b);
        });
        self.new(right_h)
    }

    pub fn signal(&mut self, chosen: ArcStr) {
        let (payload, payload_h) = HandleNode::linked_pair();
        let chosen = self.arena.interned(chosen.as_str()).unwrap_or_else(|| {
            // This happens when we send a signal that the program doesn't have
            // and that also isn't present in the types
            // It might still be handled by an "else" branch then
            eprintln!(
                "Attempted to signal a non-interned string: `{}`
                This is most likely type error with built in definitions.
                Sending an empty signal instead, which will always trigger an `else` branch.
                ",
                chosen
            );
            self.arena.empty_string()
        });
        let either = Node::Linear(Linear::Value(Box::new(Value::Either(chosen, payload))));
        let choice = core::mem::replace(&mut self.node, payload_h);
        self.new(choice).concurrently(|mut this| async move {
            let choice = this.node.take().await;
            this.link(choice, either);
        });
    }

    pub async fn case(&mut self) -> ArcStr {
        let Value::Either(name, payload) = self.try_destruct().await else {
            unreachable!()
        };
        self.node = payload.into();
        self.arena.get(name).into()
    }

    pub fn break_(self) {
        self.concurrently(|mut this| async move {
            match this.node.take().await {
                Node::Global(_, global_index)
                    if matches!(
                        this.arena.get(global_index),
                        Global::Destruct(GlobalCont::Continue)
                    ) =>
                {
                    ()
                }
                node => {
                    let other = Node::Linear(Linear::Value(Box::new(Value::Break)));
                    this.link(node, other);
                }
            }
        })
    }

    pub fn continue_(self) {
        self.concurrently(|mut this| async move {
            let Value::Break = this.try_destruct().await else {
                unreachable!()
            };
        })
    }

    pub fn erase(self) -> () {
        let (other, _) = self.create_share_hole();
        self.concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(node, other)
        })
    }

    pub fn duplicate(&mut self) -> Handle {
        let (other, shared) = self.create_share_hole();
        let node = core::mem::replace(&mut self.node, Node::Shared(shared.clone()).into());
        self.new(node).concurrently(|mut this| async move {
            let node = this.node.take().await;
            this.link(node, other)
        });
        self.new(Node::Shared(shared).into())
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
