use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Value};
use crate::data::Data;
use crate::flat::arena::{Arena, Index};
use crate::flat::runtime::Linker;
use crate::primitive::Primitive;
use arcstr::ArcStr;
use futures::task::FutureObj;
use std::future::Future;
use std::sync::{Arc, Mutex};

use crate::linker::Linked;
use tokio::sync::oneshot;

#[derive(Debug)]
pub enum Error {
    InvalidNode(Node<Linked>),
    InvalidValue(Value<Node<Linked>, Linked>),
    Panicked,
}

pub(crate) type Result<T> = core::result::Result<T, Error>;

#[derive(Clone)]
struct HandleLinker {
    net: NetHandle,
    arena: Arc<Arena<Linked>>,
}

pub struct Handle {
    linker: HandleLinker,
    node: Option<Node<Linked>>,
}

fn linked_pair() -> (Node<Linked>, Node<Linked>) {
    let mutex = Arc::new(Mutex::new(None));
    (
        Node::Linear(Linear::Variable(mutex.clone())),
        Node::Linear(Linear::Variable(mutex)),
    )
}

impl Handle {
    fn new(&self, node: Node<Linked>) -> Self {
        Self {
            linker: self.linker.clone(),
            node: Some(node),
        }
    }

    pub fn from_node(arena: Arc<Arena<Linked>>, net: NetHandle, node: Node<Linked>) -> Self {
        Self {
            linker: HandleLinker { arena, net },
            node: Some(node),
        }
    }

    pub fn from_package(
        arena: Arc<Arena<Linked>>,
        net: NetHandle,
        package: PackagePtr<Linked>,
    ) -> Result<Handle> {
        let mut linker = HandleLinker { arena, net };
        let root = linker.instantiate_package_captures(
            package,
            Node::Linear(Linear::Value(Box::new(Value::Break))),
        );
        Ok(Self {
            linker,
            node: Some(root),
        })
    }

    pub fn link_with(mut self, dual: Handle) {
        self.linker.link(self.node.unwrap(), dual.node.unwrap());
    }

    pub fn provide_external(mut self, ext: ExternalFn) {
        // TODO add fast variant.
        self.linker.link(
            self.node.unwrap(),
            Node::Linear(Linear::Value(Box::new(Value::ExternalFn(ext)))),
        );
    }

    pub fn concurrently<F>(self, f: impl FnOnce(Self) -> F)
    where
        F: 'static + Send + Future<Output = ()>,
    {
        self.linker
            .net
            .0
            .clone()
            .send(ReducerMessage::Spawn(FutureObj::from(Box::new(f(self)))))
            .unwrap();
    }

    pub async fn await_ready(mut self) -> Self {
        let value = self.destruct().await;
        self.new(Node::Linear(Linear::Value(Box::new(value))))
    }

    pub fn provide_external_closure<Fun, Fut>(mut self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        self.linker.link(
            self.node.unwrap(),
            Node::Linear(Linear::Value(Box::new(Value::ExternalArc(
                super::runtime::ExternalArc(Arc::new(move |handle| Box::pin(f(handle.handle)))),
            )))),
        );
    }

    pub fn provide_primitive(mut self, primitive: Primitive) {
        // TODO add fast variant.
        self.linker.link(
            self.node.unwrap(),
            Node::Linear(Linear::Value(Box::new(Value::Primitive(primitive)))),
        );
    }

    pub fn provide_data(mut self, data: &Data) {
        let node = self.data_node(data);
        self.linker.link(self.node.unwrap(), node);
    }

    pub async fn primitive(mut self) -> Result<Primitive> {
        let primitive = match self.destruct().await {
            Value::Primitive(p) => p,
            node => return Err(Error::InvalidValue(node)),
        };
        Ok(primitive)
    }

    pub async fn data(mut self) -> Result<Data> {
        let value = self.destruct().await;
        self.data_from_value(value).await
    }

    pub fn send(&mut self) -> Self {
        let (left, left_h) = linked_pair();
        let (right, right_h) = linked_pair();
        let par = core::mem::replace(&mut self.node, Some(left_h));
        let times = Node::Linear(Linear::Value(Box::new(Value::Pair(left, right))));
        self.linker.link(par.unwrap(), times);
        self.new(right_h)
    }

    pub fn send_data(&mut self, data: &Data) {
        let right = self.send();
        let left = core::mem::replace(self, right);
        left.provide_data(data);
    }

    pub fn receive(&mut self) -> Self {
        let (left, left_h) = linked_pair();
        let (right, right_h) = linked_pair();
        let times = core::mem::replace(&mut self.node, Some(left_h));
        let par = Node::Linear(Linear::Par(Box::new(left), Box::new(right)));
        self.linker.link(times.unwrap(), par);
        self.new(right_h)
    }

    pub async fn receive_data(&mut self) -> Result<Data> {
        let value = self.destruct().await;
        let Value::Pair(left, right) = value else {
            return Err(Error::InvalidValue(value));
        };
        self.node = Some(right);
        self.new(left).data().await
    }

    pub fn signal(&mut self, chosen: ArcStr) {
        let (payload, payload_h) = linked_pair();
        let chosen = self
            .linker
            .arena
            .interned(chosen.as_str())
            .unwrap_or_else(|| {
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
                self.linker.arena.empty_string()
            });
        let either = Node::Linear(Linear::Value(Box::new(Value::Either(chosen, payload))));
        let choice = core::mem::replace(&mut self.node, Some(payload_h));
        self.linker.link(choice.unwrap(), either);
    }

    pub async fn case(&mut self) -> ArcStr {
        let linker = self.linker.clone();

        let Value::Either(name, payload) = (*self).destruct().await else {
            unreachable!()
        };
        *self = Handle {
            linker,
            node: Some(payload),
        };
        self.linker.arena.get(name).into()
    }

    pub fn break_(mut self) {
        match self.node.unwrap() {
            Node::Global(_, global_index)
                if matches!(
                    self.linker.arena.get(global_index),
                    Global::Destruct(GlobalCont::Continue)
                ) =>
            {
                ()
            }
            Node::Linear(Linear::Continue) => (),
            node => {
                let other = Node::Linear(Linear::Value(Box::new(Value::Break)));
                self.linker.link(node, other);
            }
        }
    }

    pub fn continue_(mut self) {
        match self.node.unwrap() {
            Node::Global(_, global_index)
                if matches!(
                    self.linker.arena.get(global_index),
                    Global::Value(Value::Break)
                ) =>
            {
                ()
            }
            Node::Linear(Linear::Value(value)) if matches!(value.as_ref(), Value::Break) => (),
            node => {
                let other = Node::Linear(Linear::Continue);
                self.linker.link(node, other);
            }
        }
    }

    pub fn erase(mut self) -> () {
        let (other, _) = self.linker.create_share_hole();
        self.linker.link(self.node.unwrap(), other)
    }

    pub fn duplicate(&mut self) -> Handle {
        let (other, shared) = self.linker.create_share_hole();
        let node = core::mem::replace(&mut self.node, Node::Shared(shared.clone()).into());
        self.linker.link(node.unwrap(), other);
        self.new(Node::Shared(shared).into())
    }

    async fn destruct(&mut self) -> Value<Node<Linked>, Linked> {
        let node: Node<Linked> = std::mem::take(&mut self.node).unwrap();
        let (tx, rx) = oneshot::channel();
        self.linker.link(Node::Linear(Linear::Request(tx)), node);
        rx.await.unwrap()
    }

    fn data_node(&self, data: &Data) -> Node<Linked> {
        match data {
            Data::Unit => Node::Linear(Linear::Value(Box::new(Value::Break))),
            Data::Pair(left, right) => Node::Linear(Linear::Value(Box::new(Value::Pair(
                self.data_node(left),
                self.data_node(right),
            )))),
            Data::Either(label, payload) => Node::Linear(Linear::Value(Box::new(Value::Either(
                self.intern_signal(&label),
                self.data_node(payload),
            )))),
            Data::Primitive(primitive) => {
                Node::Linear(Linear::Value(Box::new(Value::Primitive(primitive.clone()))))
            }
        }
    }

    fn intern_signal(&self, chosen: &ArcStr) -> Index<Linked, str> {
        self.linker
            .arena
            .interned(chosen.as_str())
            .unwrap_or_else(|| {
                eprintln!(
                    "Attempted to provide non-interned signal data: `{}`
                This is most likely a type error with built in definitions.
                Providing an empty signal instead, which will always trigger an `else` branch.
                ",
                    chosen
                );
                self.linker.arena.empty_string()
            })
    }

    async fn data_from_value(&self, value: Value<Node<Linked>, Linked>) -> Result<Data> {
        match value {
            Value::Break => Ok(Data::Unit),
            Value::Pair(left, right) => {
                let left = Box::pin(self.new(left).data()).await?;
                let right = Box::pin(self.new(right).data()).await?;
                Ok(Data::Pair(Box::new(left), Box::new(right)))
            }
            Value::Either(name, payload) => {
                let payload = Box::pin(self.new(payload).data()).await?;
                Ok(Data::Either(
                    self.linker.arena.get(name).into(),
                    Box::new(payload),
                ))
            }
            Value::Primitive(primitive) => Ok(Data::Primitive(primitive)),
            other => Err(Error::InvalidValue(other)),
        }
    }
}

impl Linker for HandleLinker {
    fn link(&mut self, a: Node<Linked>, b: Node<Linked>) {
        self.net.0.send(ReducerMessage::Redex(a, b)).unwrap()
    }

    fn arena(&self) -> Arc<Arena<Linked>> {
        self.arena.clone()
    }
}
