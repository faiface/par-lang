use arcstr::ArcStr;
use num_bigint::BigInt;
use std::fmt::Debug;
use std::sync::OnceLock;

use crate::par::primitive::Primitive;
use std::collections::HashMap;

use super::arena::*;
use std::any::Any;
use std::sync::{Arc, Mutex};

pub type PackagePtr = Index<OnceLock<Package>>;
pub type GlobalPtr = Index<Global>;
pub type Str = ArcStr;

use tokio::sync::oneshot;

#[derive(Clone, Debug)]
pub struct Instance {
    vars: Arc<Mutex<Box<[Option<Node>]>>>,
}

#[derive(Debug)]
pub enum UserData {
    Primitive(Primitive),
    External(ExternalFn),
    Request(oneshot::Sender<Node>),
}

pub type ExternalFn = fn(
    crate::runtime::new::readback::Handle,
) -> std::pin::Pin<Box<dyn Send + std::future::Future<Output = ()>>>;

#[derive(Clone, Debug)]
pub struct Package {
    pub root: Global,
    pub captures: Global,
    pub num_vars: usize,
}

#[derive(Clone, Debug)]
pub enum Global {
    Variable(usize),
    LinearPackage(PackagePtr, GlobalPtr),
    GlobalPackage(PackagePtr, GlobalPtr),
    Destruct(GlobalCont),
    Value(GlobalValue),
    Fanout(Index<[Global]>),
}

trait AnyDebug: Any + Debug {}

#[derive(Debug)]
pub enum Node {
    Linear(Linear),
    Shared(Shared),
    Global(Instance, Global),
}

#[derive(Clone, Debug)]
pub enum Shared {
    Async(Arc<Mutex<SharedHole>>),
    Sync(Arc<SyncShared>),
}

// A "Value" parametrized by a "pointer type"
#[derive(Clone, Debug)]
pub enum Value<P> {
    Break,
    Pair(P, P),
    Either(Str, P),
    External(ExternalFn),
    Primitive(Primitive),
}

impl<P> Value<P> {
    fn map_leaves<Q>(self, mut f: impl FnMut(P) -> Option<Q>) -> Option<Value<Q>> {
        Some(match self {
            Value::Break => Value::Break,
            Value::Pair(a, b) => Value::Pair(f(a)?, f(b)?),
            Value::Either(s, v) => Value::Either(s, f(v)?),
            Value::External(e) => Value::External(e),
            Value::Primitive(primitive) => Value::Primitive(primitive),
        })
    }
}

#[derive(Clone, Debug)]
pub enum SyncShared {
    Package(PackagePtr, Shared),
    Value(Value<Shared>),
}

pub type GlobalValue = Value<GlobalPtr>;
#[derive(Debug)]
pub enum Linear {
    Value(Value<Box<Node>>),
    Request(oneshot::Sender<Node>),
    ShareHole(Arc<Mutex<SharedHole>>),
}

impl From<UserData> for Linear {
    fn from(this: UserData) -> Linear {
        match this {
            UserData::Primitive(p) => Linear::Value(Value::Primitive(p)),
            UserData::External(p) => Linear::Value(Value::External(p)),
            UserData::Request(p) => Linear::Request(p),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GlobalCont {
    Continue,
    Par(GlobalPtr, GlobalPtr),
    Choice(GlobalPtr, Arc<HashMap<ArcStr, PackagePtr>>),
}

#[derive(Debug)]
enum SharedHole {
    Filled(SyncShared),
    Unfilled(Vec<Node>),
}
pub struct Runtime {
    pub arena: Arc<Arena>,
    pub redexes: Vec<(Node, Node)>,
}

impl From<Arc<Arena>> for Runtime {
    fn from(arena: Arc<Arena>) -> Self {
        Self {
            arena,
            redexes: vec![],
        }
    }
}

macro_rules! sym {
    ($a: pat, $b: pat) => {
        ($a, $b) | ($b, $a)
    };
}

impl Runtime {
    fn set_var(&mut self, instance: Instance, index: usize, value: Node) {
        let mut lock = instance.vars.lock().unwrap();
        let slot = lock.get_mut(index).unwrap();
        match slot {
            Some(..) => {
                self.link(slot.take().unwrap(), value);
            }
            None => {
                *slot = Some(value);
            }
        }
    }
    pub fn link(&mut self, a: Node, b: Node) {
        self.redexes.push((a, b));
    }
    pub fn instantiate(&mut self, package: PackagePtr) -> (Node, Node) {
        let package = self.arena.get(package);
        let instance = Instance {
            vars: Arc::new(Mutex::new(
                Vec::from_iter(
                    std::iter::from_fn(|| Some(None)).take(package.get().unwrap().num_vars),
                )
                .into_boxed_slice(),
            )),
        };
        let package = package.get().unwrap();
        (
            Node::Global(instance.clone(), package.root.clone()),
            Node::Global(instance.clone(), package.captures.clone()),
        )
    }
    fn share(&mut self, a: Node) -> Option<Shared> {
        match a {
            Node::Linear(any) => None,
            Node::Shared(shared) => Some(shared),
            Node::Global(instance, Global::Destruct(..)) => None,
            Node::Global(instance, Global::LinearPackage(..)) => None,
            Node::Global(instance, Global::Fanout(..)) => None,
            Node::Global(instance, Global::GlobalPackage(package, captures)) => {
                let captures = Node::Global(instance, self.arena.get(captures).clone());
                let captures = self.share(captures)?;
                Some(Shared::Sync(Arc::new(SyncShared::Package(
                    package, captures,
                ))))
            }
            Node::Global(instance, Global::Value(value)) => Some(Shared::Sync(Arc::new(
                SyncShared::Value(value.map_leaves(|p| {
                    self.share(Node::Global(instance.clone(), self.arena.get(p).clone()))
                })?),
            ))),
            Node::Linear(Linear::Value(value)) => Some(Shared::Sync(Arc::new(SyncShared::Value(
                value.map_leaves(|p| self.share(*p))?,
            )))),
            Node::Global(instance, Global::Variable(id)) => {
                let mut lock = instance.vars.lock().unwrap();
                let slot = &mut lock[id];
                if let Some(slot) = slot.take() {
                    drop(lock);
                    self.share(slot)
                } else {
                    let state = Arc::new(Mutex::new(SharedHole::Unfilled(vec![])));
                    let hole = Node::Linear(Linear::ShareHole(state.clone()));
                    slot.replace(hole);
                    Some(Shared::Async(state))
                }
            }
        }
    }
    pub fn reduce(&mut self) -> Option<(UserData, Node)> {
        while let Some((a, b)) = self.redexes.pop() {
            if let Some(v) = self.interact(a, b) {
                return Some(v);
            }
        }
        None
    }
    fn link_with_hole(&mut self, hole: &mut SharedHole, value: Node) {
        match hole {
            SharedHole::Filled(sync_shared_value) => {
                self.link(
                    Node::Shared(Shared::Sync(Arc::new(sync_shared_value.clone()))),
                    value,
                );
            }
            SharedHole::Unfilled(values) => values.push(value),
        }
    }
    fn interact(&mut self, a: Node, b: Node) -> Option<(UserData, Node)> {
        match (a, b) {
            sym!(Node::Global(instance, Global::Variable(index)), value) => {
                self.set_var(instance, index, value)
            }
            sym!(
                Node::Global(instance, Global::LinearPackage(package, captures_in)),
                other
            ) => {
                let (root, captures) = self.instantiate(package);
                let captures_in = self.arena.get(captures_in).clone();
                self.link(captures, Node::Global(instance, captures_in));
                self.link(root, other);
            }
            sym!(Node::Linear(Linear::Request(request)), other) => {
                return Some((UserData::Request(request), other));
            }
            sym!(Node::Global(instance, Global::Fanout(destinations)), other) => {
                let other = self.share(other).unwrap();
                let destinations = self.arena.get(destinations);
                for dest in destinations {
                    self.redexes.push((
                        Node::Global(instance.clone(), dest.clone()),
                        Node::Shared(other.clone()),
                    ));
                }
            }
            sym!(Node::Linear(Linear::ShareHole(state)), other) => {
                let other = self.share(other).unwrap();
                match other {
                    Shared::Async(other) => self.link_with_hole(
                        &mut other.lock().unwrap(),
                        Node::Linear(Linear::ShareHole(state)),
                    ),
                    Shared::Sync(value) => {
                        let mut lock = state.lock().unwrap();
                        let SharedHole::Unfilled(values) =
                            core::mem::replace(&mut *lock, SharedHole::Filled((*value).clone()))
                        else {
                            unreachable!()
                        };
                        for i in values {
                            self.link(i, Node::Shared(Shared::Sync(value.clone())));
                        }
                    }
                }
            }
            sym!(
                Node::Global(instance, Global::GlobalPackage(package, captures_in)),
                other
            ) => {
                let (root, captures) = self.instantiate(package);
                let captures_in = self.arena.get(captures_in).clone();
                self.link(captures, Node::Global(instance, captures_in));
                self.link(root, other);
            }
            sym!(Node::Shared(Shared::Sync(x)), other)
                if matches!(&*x, SyncShared::Package(..)) =>
            {
                let SyncShared::Package(package, captures_in) = &*x else {
                    unreachable!()
                };
                let (root, captures) = self.instantiate(package.clone());
                self.link(captures, Node::Shared(captures_in.clone()));
                self.link(root, other);
            }
            sym!(Node::Shared(Shared::Async(state)), other) => {
                let mut lock = state.lock().unwrap();
                self.link_with_hole(&mut *lock, other);
            }
            sym!(Node::Shared(_), Node::Global(_, Global::Value(_)))
            | (Node::Shared(_), Node::Shared(_))
            | (Node::Global(_, Global::Value(_)), Node::Global(_, Global::Value(_))) => {
                unimplemented!("Value-value!");
            }
            _ => todo!(),
        };
        None
    }
}
