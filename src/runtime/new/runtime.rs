use arcstr::ArcStr;
use core::panic;
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

#[derive(Debug)]
pub struct InstanceInner(Mutex<Box<[Option<Node>]>>);
#[derive(Clone, Debug)]
pub struct Instance {
    vars: Arc<InstanceInner>,
}

impl Instance {
    fn identifier(&self) -> usize {
        (Arc::as_ptr(&self.vars) as usize >> 3) & 0xFF
    }
}

impl Drop for InstanceInner {
    fn drop(&mut self) {
        for i in self.0.lock().unwrap().as_mut().iter() {
            if !i.is_none() {
                panic!(
                    "Data was leaked in an Instance:
                    {i:?}
                "
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum UserData {
    Primitive(Primitive),
    ExternalFn(ExternalFn),
    ExternalArc(ExternalArc),
    Request(oneshot::Sender<Node>),
}

pub type ExternalFnRet = std::pin::Pin<Box<dyn Send + std::future::Future<Output = ()>>>;
pub type ExternalFn = fn(crate::runtime::Handle) -> ExternalFnRet;

#[derive(Clone)]
pub struct ExternalArc(pub Arc<dyn Send + Sync + Fn(crate::runtime::Handle) -> ExternalFnRet>);

impl Debug for ExternalArc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalArc").finish_non_exhaustive()
    }
}

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
    ExternalFn(ExternalFn),
    ExternalArc(ExternalArc),
    Primitive(Primitive),
}

impl<P> Value<P> {
    fn map_leaves<Q>(self, mut f: impl FnMut(P) -> Option<Q>) -> Option<Value<Q>> {
        Some(match self {
            Value::Break => Value::Break,
            Value::Pair(a, b) => Value::Pair(f(a)?, f(b)?),
            Value::Either(s, v) => Value::Either(s, f(v)?),
            Value::ExternalFn(e) => Value::ExternalFn(e),
            Value::ExternalArc(e) => Value::ExternalArc(e),
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
            UserData::ExternalFn(p) => Linear::Value(Value::ExternalFn(p)),
            UserData::ExternalArc(p) => Linear::Value(Value::ExternalArc(p)),
            UserData::Request(p) => Linear::Request(p),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GlobalCont {
    Continue,
    Par(GlobalPtr, GlobalPtr),
    Choice(
        GlobalPtr,
        Arc<HashMap<ArcStr, PackagePtr>>,
        Option<PackagePtr>,
    ),
}

#[derive(Debug)]
pub enum SharedHole {
    Filled(SyncShared),
    Unfilled(Vec<Node>),
}
pub struct Runtime {
    pub arena: Arc<Arena>,
    pub redexes: Vec<(Node, Node)>,
}

pub trait Linker {
    fn link(&mut self, a: Node, b: Node);
    fn arena(&self) -> Arc<Arena>;

    fn destruct(&mut self, node: Node) -> Result<Value<Node>, Node> {
        match node {
            Node::Linear(Linear::Value(v)) => Ok(v.map_leaves(|x| Some(*x)).unwrap()),
            Node::Shared(Shared::Sync(shared)) => match &*shared {
                SyncShared::Package(package, shared) => {
                    let node = self
                        .instantiate_with_captures(package.clone(), Node::Shared(shared.clone()));
                    self.destruct(node)
                }
                SyncShared::Value(shared) => Ok(shared
                    .clone()
                    .map_leaves(|x| Some(Node::Shared(x)))
                    .unwrap()),
            },
            Node::Global(instance, Global::Value(v)) => Ok(v
                .map_leaves(|x| Some(Node::Global(instance.clone(), self.arena().get(x).clone())))
                .unwrap()),
            node => Err(node),
        }
    }

    fn instantiate(&self, package: PackagePtr) -> (Node, Node) {
        let (a, b, _, _) = self.instantiate_with_extra_vars(package, 0);
        (a, b)
    }
    fn instantiate_with_extra_vars(
        &self,
        package: PackagePtr,
        extra_vars: usize,
    ) -> (Node, Node, Instance, usize) {
        let arena = self.arena();
        let package = arena.get(package);
        let num_vars = package.get().unwrap().num_vars;
        let instance = Instance {
            vars: Arc::new(InstanceInner(Mutex::new(
                Vec::from_iter(std::iter::from_fn(|| Some(None)).take(num_vars + extra_vars))
                    .into_boxed_slice(),
            ))),
        };
        println!("Instantiate {:x}", instance.identifier());
        let package = package.get().unwrap();
        (
            Node::Global(instance.clone(), package.root.clone()),
            Node::Global(instance.clone(), package.captures.clone()),
            instance,
            num_vars,
        )
    }
    fn instantiate_with_captures(&mut self, package: PackagePtr, captures: Node) -> Node {
        // TODO: This work like this now for compatibility with the old runtime, to make the
        // transpiler simpler
        // The correct approach is just linking the captures together.
        let (root, internal_captures, instance, extra_var_idx) =
            self.instantiate_with_extra_vars(package, 1);
        let var = Global::Variable(extra_var_idx);
        self.link(internal_captures, Node::Linear(Linear::Value(Value::Break)));
        self.link(
            root,
            Node::Linear(Linear::Value(Value::Pair(
                Box::new(captures),
                Box::new(Node::Global(instance.clone(), var.clone())),
            ))),
        );
        Node::Global(instance.clone(), var.clone())
    }

    fn enqueue_to_hole(&mut self, hole: &mut SharedHole, cont: Node) {
        match hole {
            SharedHole::Filled(sync_shared_value) => {
                self.link(
                    Node::Shared(Shared::Sync(Arc::new(sync_shared_value.clone()))),
                    cont,
                );
            }
            SharedHole::Unfilled(values) => values.push(cont),
        }
    }
    fn fill_hole(&mut self, hole: Arc<Mutex<SharedHole>>, value: Node) {
        let value = self.share(value).unwrap();
        match value {
            Shared::Async(value) => self.enqueue_to_hole(
                &mut value.lock().unwrap(),
                Node::Linear(Linear::ShareHole(hole)),
            ),
            Shared::Sync(value) => {
                let mut lock = hole.lock().unwrap();
                let SharedHole::Unfilled(continuations) =
                    core::mem::replace(&mut *lock, SharedHole::Filled((*value).clone()))
                else {
                    unreachable!()
                };
                for i in continuations {
                    self.link(i, Node::Shared(Shared::Sync(value.clone())));
                }
            }
        }
    }
    fn share(&self, node: Node) -> Option<Shared> {
        match node {
            Node::Shared(shared) => Some(shared),
            Node::Global(_instance, Global::Destruct(..)) => None,
            Node::Global(_instance, Global::LinearPackage(..)) => None,
            Node::Global(_instance, Global::Fanout(..)) => None,
            Node::Global(instance, Global::GlobalPackage(package, captures)) => {
                let captures = Node::Global(instance, self.arena().get(captures).clone());
                let captures = self.share(captures)?;
                Some(Shared::Sync(Arc::new(SyncShared::Package(
                    package, captures,
                ))))
            }
            Node::Global(instance, Global::Value(value)) => Some(Shared::Sync(Arc::new(
                SyncShared::Value(value.map_leaves(|p| {
                    self.share(Node::Global(instance.clone(), self.arena().get(p).clone()))
                })?),
            ))),
            Node::Linear(Linear::Value(value)) => Some(Shared::Sync(Arc::new(SyncShared::Value(
                value.map_leaves(|p| self.share(*p))?,
            )))),
            Node::Linear(Linear::Request(..)) => None,
            Node::Linear(Linear::ShareHole(..)) => None,
            Node::Global(instance, Global::Variable(id)) => {
                let mut lock = instance.vars.0.lock().unwrap();
                let slot = &mut lock[id];
                if let Some(slot) = slot.take() {
                    drop(lock);
                    Some(self.share(slot)?)
                } else {
                    let state = Arc::new(Mutex::new(SharedHole::Unfilled(vec![])));
                    let hole = Node::Linear(Linear::ShareHole(state.clone()));
                    slot.replace(hole);
                    Some(Shared::Async(state))
                }
            }
        }
    }
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

impl Linker for Runtime {
    fn link(&mut self, a: Node, b: Node) {
        self.redexes.push((a, b));
    }
    fn arena(&self) -> Arc<Arena> {
        self.arena.clone()
    }
}

impl Runtime {
    fn set_var(&mut self, instance: Instance, index: usize, value: Node) {
        let mut lock = instance.vars.0.lock().unwrap();
        println!("{:?} {:?}", index, &mut *lock);
        let slot = lock.get_mut(index).expect("Invalid index in variable!");
        match slot {
            Some(..) => {
                self.link(slot.take().unwrap(), value);
            }
            None => {
                *slot = Some(value);
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
    fn interact(&mut self, a: Node, b: Node) -> Option<(UserData, Node)> {
        println!(
            "Redex {:?} {:?} ",
            a.variant_tree_name(),
            b.variant_tree_name()
        );
        match (a, b) {
            sym!(Node::Global(instance, Global::Variable(index)), value) => {
                self.set_var(instance, index, value)
            }
            sym!(
                Node::Global(instance, Global::LinearPackage(package, captures_in)),
                other
            ) => {
                let root = self.instantiate_with_captures(
                    package,
                    Node::Global(instance, self.arena.get(captures_in).clone()),
                );
                self.link(root, other);
            }
            sym!(Node::Shared(Shared::Async(state)), other) => {
                let mut lock = state.lock().unwrap();
                self.enqueue_to_hole(&mut *lock, other);
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
            sym!(Node::Linear(Linear::ShareHole(hole)), other) => self.fill_hole(hole, other),
            sym!(
                Node::Global(instance, Global::GlobalPackage(package, captures_in)),
                other
            ) => {
                let root = self.instantiate_with_captures(
                    package,
                    Node::Global(instance, self.arena.get(captures_in).clone()),
                );
                self.link(root, other);
            }
            sym!(Node::Shared(Shared::Sync(x)), other)
                if matches!(&*x, SyncShared::Package(..)) =>
            {
                let SyncShared::Package(package, captures_in) = &*x else {
                    unreachable!()
                };
                let root = self
                    .instantiate_with_captures(package.clone(), Node::Shared(captures_in.clone()));
                self.link(root, other);
            }
            sym!(
                Node::Linear(Linear::Value(Value::ExternalFn(ext)))
                    | Node::Global(_, Global::Value(Value::ExternalFn(ext))),
                other
            ) => {
                return Some((UserData::ExternalFn(ext), other));
            }
            sym!(
                Node::Linear(Linear::Value(Value::ExternalArc(ext)))
                    | Node::Global(_, Global::Value(Value::ExternalArc(ext))),
                other
            ) => {
                return Some((UserData::ExternalArc(ext), other));
            }
            sym!(Node::Global(instance, Global::Destruct(destructor)), node) => {
                match (
                    self.destruct(node).expect("Continuation-continuation!"),
                    destructor,
                ) {
                    (Value::Break, GlobalCont::Continue) => (),
                    (Value::Pair(a0, a1), GlobalCont::Par(b0, b1)) => {
                        self.link(
                            a0,
                            Node::Global(instance.clone(), self.arena.get(b0).clone()),
                        );
                        self.link(a1, Node::Global(instance, self.arena.get(b1).clone()));
                    }
                    (
                        Value::Either(signal, payload),
                        GlobalCont::Choice(context, options, default),
                    ) => {
                        if let Some(package) = options.get(&signal) {
                            let root = self.instantiate_with_captures(
                                package.clone(),
                                Node::Global(instance, self.arena.get(context).clone()),
                            );
                            self.link(root, payload);
                        } else {
                            let root = self.instantiate_with_captures(
                                default.unwrap().clone(),
                                Node::Global(instance, self.arena.get(context).clone()),
                            );
                            // TODO: Optimize this; we're reconstructing the `Either` branch.
                            // This could make us lose sharing.
                            self.link(
                                Node::Linear(Linear::Value(Value::Either(
                                    signal,
                                    Box::new(payload),
                                ))),
                                root,
                            );
                        }
                    }
                    (a, b) => {
                        panic!("Unimplemented destruction between: {:?} {:?}", a, b)
                    }
                }
            }
            (a, b) => {
                panic!(
                    "Unimplemented reduction: {:?} {:?}",
                    a.variant_tree_name(),
                    b.variant_tree_name()
                )
            }
        };
        None
    }
}

impl Node {
    pub fn variant_tree_name(&self) -> String {
        match self {
            Node::Linear(l) => format!("Linear.{}", l.variant_tree_name()),
            Node::Shared(s) => format!("Shared.{}", s.variant_tree_name()),
            Node::Global(i, g) => {
                format!("Global@{:x}.{}", i.identifier(), g.variant_tree_name())
            }
        }
    }
}

impl Linear {
    pub fn variant_tree_name(&self) -> String {
        match self {
            Linear::Value(v) => format!("Value({})", v.variant_tree_name()),
            Linear::Request(_) => "Request".to_owned(),
            Linear::ShareHole(_) => "ShareHole".to_owned(),
        }
    }
}

impl Shared {
    pub fn variant_tree_name(&self) -> String {
        match self {
            Shared::Async(_) => "Async".to_owned(),
            Shared::Sync(sync) => format!("Sync({})", sync.variant_tree_name()),
        }
    }
}

impl SyncShared {
    pub fn variant_tree_name(&self) -> String {
        match self {
            SyncShared::Package(_, inner) => format!("Package({})", inner.variant_tree_name()),
            SyncShared::Value(v) => format!("Value({})", v.variant_tree_name()),
        }
    }
}

impl Global {
    pub fn variant_tree_name(&self) -> String {
        match self {
            Global::Variable(_) => "Variable".into(),

            Global::LinearPackage(_, _g) => "LinearPackage".into(),

            Global::GlobalPackage(_, _g) => "GlobalPackage".into(),

            Global::Destruct(c) => format!("Destruct({})", c.variant_tree_name()),

            Global::Value(v) => format!("Value({})", v.variant_tree_name()),

            Global::Fanout(_idx) => "Fanout".into(),
        }
    }
}

impl GlobalCont {
    pub fn variant_tree_name(&self) -> String {
        match self {
            GlobalCont::Continue => "Continue".into(),

            GlobalCont::Par(_a, _b) => "Par".into(),

            GlobalCont::Choice(_ptr, ..) => "Choice".into(),
        }
    }
}

impl<P> Value<P> {
    pub fn variant_tree_name(&self) -> String {
        match self {
            Value::Break => "Break".into(),

            Value::Pair(_a, _b) => "Pair".into(),

            Value::Either(_, _p) => "Either".into(),

            Value::ExternalFn(_) => "ExternalFn".into(),
            Value::ExternalArc(_) => "ExternalArc".into(),

            Value::Primitive(_) => "Primitive".into(),
        }
    }
}
