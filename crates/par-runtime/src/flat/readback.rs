use super::reducer::{NetHandle, ReducerMessage};
use super::runtime::{
    ExternalFn, Global, GlobalCont, Linear, Node, PackagePtr, Value,
};
use crate::flat::arena::Arena;
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

async fn wait_for_mutex(mutex: Arc<Mutex<Option<Node<Linked>>>>) -> Node<Linked> {
    let res: std::result::Result<Node<fn(crate::readback::Handle) -> std::pin::Pin<Box<dyn Future<Output = ()> + Send + 'static>>>, oneshot::Receiver<Node<fn(crate::readback::Handle) -> std::pin::Pin<Box<dyn Future<Output = ()> + Send + 'static>>>>> = {
        let mut lock = mutex.lock().unwrap();
        match lock.take() {
            Some(node) => {
                Ok(node)
            },
            None => {
                let (tx, rx) = oneshot::channel::<Node<Linked>>();
                lock.replace(Node::Linear(Linear::Request(tx)));
                Err(rx)
            }
        }
    };

    match res {
        Ok(node) => node,
        Err(rx) => rx.await.expect("msg")
    }
}

impl Handle {
    fn new(&self,node: Node<Linked>) -> Self {
        Self {
            linker: self.linker.clone(),
            node: Some(node),
        }
    }

    pub fn from_node(arena: Arc<Arena<Linked>>, net: NetHandle, node: Node<Linked>) -> Self {
        Self {
            linker: HandleLinker {
                arena,
                net,
            },
            node: Some(node),
        }
    }

    pub fn from_package(
        arena: Arc<Arena<Linked>>,
        net: NetHandle,
        package: PackagePtr<Linked>,
    ) -> Result<Handle> {
        let mut linker = HandleLinker {
            arena,
            net,
        };
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
        self.linker.net
            .0
            .clone()
            .send(ReducerMessage::Spawn(FutureObj::from(Box::new(f(self)))))
            .unwrap();
    }

    pub async fn await_ready(mut self) -> Self {
        let mut node = self.node.unwrap();
        loop {
            match node {
                Node::Linear(Linear::Variable(mutex)) => {
                    node = wait_for_mutex(mutex.clone()).await;
                    continue;
                },
                _ => {
                    break;
                }
            }
        };
        self.node = Some(node);
        self
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

    pub async fn primitive(mut self) -> Result<Primitive> {
        let primitive = match self.destruct().await {
            Value::Primitive(p) => p,
            node => return Err(Error::InvalidValue(node)),
        };
        Ok(primitive)
    }

    pub fn send(&mut self) -> Self {
        let (left, left_h) = linked_pair();
        let (right, right_h) = linked_pair();
        let par = core::mem::replace(&mut self.node, Some(left_h));
        let times = Node::Linear(Linear::Value(Box::new(Value::Pair(left, right))));
        self.linker.link(par.unwrap(), times);
        self.new(right_h)
    }

    pub fn receive(&mut self) -> Self {
        let (left, left_h) = linked_pair();
        let (right, right_h) = linked_pair();
        let times = core::mem::replace(&mut self.node, Some(left_h));
        self.new(times.unwrap()).concurrently(|mut this| async move {
            let mut linker = this.linker.clone();
            let Value::Pair(a, b) = this.destruct().await else {
                unreachable!();
            };
            linker.link(left, a);
            linker.link(right, b);
        });
        self.new(right_h)
    }

    pub fn signal(&mut self, chosen: ArcStr) {
        let (payload, payload_h) = linked_pair();
        let chosen = self.linker.arena.interned(chosen.as_str()).unwrap_or_else(|| {
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
            node => {
                let other = Node::Linear(Linear::Value(Box::new(Value::Break)));
                self.linker.link(node, other);
            }
        }
    }

    pub fn continue_(self) {
        self.concurrently(|mut this| async move {
            let Value::Break = this.destruct().await else {
                unreachable!()
            };
        })
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

    // async fn retry(&mut self) {
    //     let (tx, rx) = oneshot::channel::<Node<Linked>>();
    //     let mut old_node = core::mem::replace(&mut self.node, rx.into());
    //     let old_node = old_node.take().await;
    //     self.link(old_node, Node::Linear(Linear::Request(tx)));
    // }

    async fn destruct(&mut self) -> Value<Node<Linked>, Linked> {
        let mut node: Node<Linked> = std::mem::take(&mut self.node).unwrap();
        loop {
            match node {
                Node::Linear(Linear::Variable(mutex)) => {
                    node = wait_for_mutex(mutex.clone()).await;
                    continue;
                },
                _ => {
                    match self.linker.destruct(node) {
                        Ok(v) => {
                            return v;
                        }
                        Err(node2) => {
                            node = node2;
                            continue
                        }
                    }
                }
            }
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
