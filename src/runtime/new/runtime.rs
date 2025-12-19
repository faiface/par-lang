//! This is the V2 runtime for the Par language
//!
//! Unlike with the old runtime, program state is strictly separated with
//! immutable global program code. The global nodes are stored in an [`Arena`].
//! Global nodes are attached to a piece of state which is a pointer to its [`Instance`]
//!
//! An `Instance` is an array of variable slots; which might be either empty or filled with a [`Value`]
//! Instances are used to store program state within each global package, and also
//! for communication within each global packages.
//!
//! Global nodes are all stored contiguously, and a definition's state is usually within the same Instance, which means
//! that the runtime enjoys cache locality.
//!
//! Additionally, state can be stored in either [`Shared`] nodes or [`Linear`] nodes.
//!
//! `Linear` nodes are temporarily created by readback to interact with the program. They are usually stored in `Box`es
//!  Additionally, ShareHoles are a type of linear value which is created when attempting to duplicate a global variable.
//!  It is roughly the dual of a fanout node.
//!
//! `Shared` nodes are created when a node is duplicated. They are reference-counted and thus, copying them is cheap. They are destroyed
//! by interacting them with a Global node that destructures values, such as a Par node or a Choice node. This destructures the shared node,
//! but the children will still be reference-counted. This allows cheap copying of runtime values, which is something the old
//! runtime was not able to do.
//!
//! The runtime is handled by the [`Runtime`] struct. It tracks the program's redexes, which
//! are pairs of nodes that interact with each other, and reduces them until the program is finished.
//! The `Runtime` does not know what to do with IO operations; that is handled by the `Reducer`

use arcstr::ArcStr;
use core::panic;
use std::fmt::Debug;
use std::sync::OnceLock;

use crate::runtime::new::show::Shower;
use crate::{par::primitive::Primitive, runtime::new::show::Showable};
use std::collections::HashMap;

use super::arena::*;
use crate::runtime::new::stats::Rewrites;
use crate::runtime::old::net::FanBehavior;
use std::sync::{Arc, Mutex};

use tokio::sync::oneshot;

pub type PackagePtr = Index<OnceLock<Package>>;
pub type GlobalPtr = Index<Global>;
pub type Str = ArcStr;

#[derive(Debug)]
struct InstanceInner(Mutex<Box<[Option<Node>]>>);

#[derive(Clone, Debug)]
/// An `Instance` stores the state associated to an instance of a Global node.
///
/// Instances can be cheaply cloned shallowly because they are reference counter; this creates another instance pointing to the same underyling
/// [`InstanceInner`].
///
/// They act as the backing store for Global nodes. Specifically, [`Global::Variable`] nodes
/// are indices into the array inside the `Instance`. Each instance of a global node
/// also holds an Instance. It is in this instance where the state of the variables is stored
///
/// ## Lifetime of an `Instance`
///
/// They are created empty by `Linker::instantiate`, with a fixed length. Initially, all variables are empty.
/// Then, calls to `Runtime::set_var` slowly fill up and empty the instance. Each variable slot is filled once and taken out of once.
/// (I'm not sure if it's possible for a variable slot to never be used, but it definitely can't happen twice)
/// At the end of the lifetime of Instances, all of them go out of scope and are eventually dropped. Because of the way
/// the runtime is designed, all slots inside of it must be empty. This is when the `Instance` is destroyed.
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
        // This is a debugging tool to detect leaks.
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
/// User data; this is data created by the external
pub enum UserData {
    /// An external function without captures. This is created externally
    /// and the function takes in a handle to the value it interacts with
    ExternalFn(ExternalFn),
    /// An external function with shared captured. This is created externally
    ExternalArc(ExternalArc),
    /// A one-timer request for a value. The value it interacts with will be send through the sender.
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
/// A package is a `Global` subgraph that is isolated from the rest of the program
/// It does not use the same Instance as its environment, and so it requires a
/// separate Instance to be created when it is expanded.
pub struct Package {
    pub root: Global,
    pub captures: Global,
    /// How large the Instance must be.
    pub num_vars: usize,

    pub debug_name: String,
    // TODO: Store this inline in the arena.
    pub redexes: Index<[(Global, Global)]>,
}

#[derive(Clone, Debug)]
pub enum Global {
    Variable(usize),
    Package(PackagePtr, GlobalPtr, FanBehavior),
    /// Destruct attempts to convert the interacting node into a value,
    /// and then carries out a negative operation on it according to its variant
    /// This node is created from the Continue, Case, and Receive commands.
    Destruct(GlobalCont),
    Value(GlobalValue),
    /// Fanout; turn the value it interacts with into a nonlinear value.
    /// This node is created whenever a Par variable is used
    /// nonlinearly
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

/// A "Value" parametrized by a "pointer type P". This is used to avoid code duplication between
/// different value containers
/// Values are either positive types or copyable external functions. They can all be duplicated if their
/// subnodes are duplicable too.
///
/// P is the type of children. Usually, this will be `GlobalPtr`, `Shared`, or `Box<Node>`
///
/// There are [`Linear`] values, [`Shared`] values, and [`Global`] values.
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
    pub fn map_leaves<Q>(self, mut f: impl FnMut(P) -> Option<Q>) -> Option<Value<Q>> {
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
            UserData::ExternalFn(p) => Linear::Value(Value::ExternalFn(p)),
            UserData::ExternalArc(p) => Linear::Value(Value::ExternalArc(p)),
            UserData::Request(p) => Linear::Request(p),
        }
    }
}

#[derive(Clone, Debug)]
/// A "global continuation"; a negative node stored in the global array
/// When it interacts with anything, it attempts to destructure it.
pub enum GlobalCont {
    /// The continue node; created in continue commands (`value?`)
    Continue,
    /// The par node; created in receive commands (`value[a]`)
    Par(GlobalPtr, GlobalPtr),
    /// The choice node; created in case commands (`value.case { ... }`)
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
    pub rewrites: Rewrites,
}

/// This trait is implemented by everything that knows how to link two nodes together
/// and that holds a pointer to the arena. This prevents duplication between [`Runtime`] and
/// [`Handle`], which are the two implementors of this trait.
pub trait Linker {
    fn link(&mut self, a: Node, b: Node);
    fn arena(&self) -> Arc<Arena>;

    fn show<'a, 'b>(&'b self, node: &'a Node) -> String {
        let arena_ref = self.arena();
        format!("{}", Showable(node, &mut Shower::from_arena(&arena_ref)))
    }
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

    fn instantiate(&mut self, package: PackagePtr) -> (Node, Node) {
        let (a, b, _, _) = self.instantiate_with_extra_vars(package, 0);
        (a, b)
    }
    fn instantiate_with_extra_vars(
        &mut self,
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
        let package = package.get().unwrap();
        self.arena()
            .get(package.redexes.clone())
            .iter()
            .for_each(|(a, b)| {
                self.link(
                    Node::Global(instance.clone(), a.clone()),
                    Node::Global(instance.clone(), b.clone()),
                );
            });
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
        let (root, internal_captures, _, _) = self.instantiate_with_extra_vars(package, 0);
        self.link(internal_captures, captures);
        root
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
    /// Recusrively turn a node into a `Shared` node which allows duplication
    /// This is done whenever a node needs to be duplicated. This function may return None if the node can't be duplicated.
    /// This is the case for linear nodes and negative types.
    fn share(&mut self, node: Node) -> Option<Shared> {
        self.share_inner(node)
    }
    fn share_inner(&mut self, node: Node) -> Option<Shared> {
        stacker::maybe_grow(32 * 1024, 1024 * 1024, move || match node {
            Node::Shared(shared) => Some(shared),
            Node::Global(_instance, Global::Destruct(..)) => None,
            Node::Global(_instance, Global::Fanout(..)) => None,
            Node::Global(instance, Global::Package(package, captures, FanBehavior::Expand)) => {
                let captures = self.arena().get(captures).clone();
                let root =
                    self.instantiate_with_captures(package, Node::Global(instance, captures));
                self.share_inner(root)
            }
            Node::Global(instance, Global::Package(package, captures, FanBehavior::Propagate)) => {
                let captures = Node::Global(instance, self.arena().get(captures).clone());
                let captures = self.share_inner(captures)?;
                Some(Shared::Sync(Arc::new(SyncShared::Package(
                    package, captures,
                ))))
            }
            Node::Global(instance, Global::Value(value)) => Some(Shared::Sync(Arc::new(
                SyncShared::Value(value.map_leaves(|p| {
                    self.share_inner(Node::Global(instance.clone(), self.arena().get(p).clone()))
                })?),
            ))),
            Node::Linear(Linear::Value(value)) => Some(Shared::Sync(Arc::new(SyncShared::Value(
                value.map_leaves(|p| self.share_inner(*p))?,
            )))),
            Node::Linear(Linear::Request(h)) => {
                let (hole, shared) = self.create_share_hole();
                h.send(hole).unwrap();
                Some(shared)
            }
            Node::Linear(Linear::ShareHole(..)) => None,
            Node::Global(instance, Global::Variable(id)) => {
                let mut lock = instance.vars.0.lock().unwrap();
                let slot = &mut lock[id];
                if let Some(slot) = slot.take() {
                    drop(lock);
                    Some(self.share_inner(slot)?)
                } else {
                    let (hole, shared) = self.create_share_hole();
                    slot.replace(hole);
                    Some(shared)
                }
            }
        })
    }
    fn create_share_hole(&self) -> (Node, Shared) {
        let state = Arc::new(Mutex::new(SharedHole::Unfilled(vec![])));
        let hole = Node::Linear(Linear::ShareHole(state.clone()));
        (hole, Shared::Async(state))
    }
}

impl From<Arc<Arena>> for Runtime {
    fn from(arena: Arc<Arena>) -> Self {
        Self {
            arena,
            redexes: vec![],
            rewrites: Rewrites::default(),
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
    pub fn status(&self) {
        println!("Runtime status");
        for (a, b) in &self.redexes {
            println!("  {} ~ {}", self.show(&a), self.show(&b));
        }
    }
    /// Reduce all redexes in the net until a redex requires external action.
    /// Returns `Some` if there is such redex; returns `None` if no
    /// external action is needed and the net is in normal form.
    ///
    /// This function is analogous to a "VM enter"
    pub fn reduce(&mut self) -> Option<(UserData, Node)> {
        while let Some((a, b)) = self.redexes.pop() {
            if let Some(v) = self.interact(a, b) {
                return Some(v);
            }
        }
        None
    }
    fn interact_set_var(&mut self, instance: Instance, index: usize, value: Node) {
        self.set_var(instance, index, value)
    }
    fn interact_fanout(&mut self, instance: Instance, destinations: Index<[Global]>, other: Node) {
        self.rewrites.fanout += 1;
        let other = self.share(other).unwrap();
        let destinations = self.arena.get(destinations);
        for dest in destinations {
            self.redexes.push((
                Node::Global(instance.clone(), dest.clone()),
                Node::Shared(other.clone()),
            ));
        }
    }
    fn interact_instantiate(
        &mut self,
        instance: Instance,
        package: PackagePtr,
        captures_in: GlobalPtr,
        other: Node,
    ) {
        self.rewrites.instantiate += 1;
        let root = self.instantiate_with_captures(
            package,
            Node::Global(instance, self.arena.get(captures_in).clone()),
        );
        self.link(root, other);
    }
    fn interact(&mut self, a: Node, b: Node) -> Option<(UserData, Node)> {
        match (a, b) {
            sym!(Node::Global(instance, Global::Variable(index)), value) => {
                self.interact_set_var(instance, index, value)
            }
            sym!(
                Node::Global(
                    instance,
                    Global::Package(package, captures_in, FanBehavior::Expand)
                ),
                other
            ) => self.interact_instantiate(instance, package, captures_in, other),
            sym!(Node::Shared(Shared::Async(state)), other) => {
                let mut lock = state.lock().unwrap();
                self.enqueue_to_hole(&mut *lock, other);
            }
            sym!(Node::Linear(Linear::Request(request)), other) => {
                self.rewrites.ext_send += 1;
                return Some((UserData::Request(request), other));
            }
            sym!(Node::Global(instance, Global::Fanout(destinations)), other) => {
                self.interact_fanout(instance, destinations, other)
            }
            sym!(Node::Linear(Linear::ShareHole(hole)), other) => self.fill_hole(hole, other),
            sym!(
                Node::Global(instance, Global::Package(package, captures_in, _)),
                other
            ) => self.interact_instantiate(instance, package, captures_in, other),
            sym!(Node::Shared(Shared::Sync(x)), other)
                if matches!(&*x, SyncShared::Package(..)) =>
            {
                self.rewrites.instantiate += 1;
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
                self.rewrites.ext_call += 1;
                return Some((UserData::ExternalFn(ext), other));
            }
            sym!(
                Node::Linear(Linear::Value(Value::ExternalArc(ext)))
                    | Node::Global(_, Global::Value(Value::ExternalArc(ext))),
                other
            ) => {
                self.rewrites.ext_call += 1;
                return Some((UserData::ExternalArc(ext), other));
            }
            sym!(Node::Shared(Shared::Sync(shared)), other)
                if matches!(shared.as_ref(), SyncShared::Value(Value::ExternalArc(_))) =>
            {
                self.rewrites.ext_call += 1;
                let SyncShared::Value(Value::ExternalArc(arc)) = shared.as_ref() else {
                    unreachable!()
                };
                return Some((UserData::ExternalArc(arc.clone()), other));
            }
            sym!(Node::Shared(Shared::Sync(shared)), other)
                if matches!(shared.as_ref(), SyncShared::Value(Value::ExternalFn(_))) =>
            {
                self.rewrites.ext_call += 1;
                let SyncShared::Value(Value::ExternalFn(arc)) = shared.as_ref() else {
                    unreachable!()
                };
                return Some((UserData::ExternalFn(arc.clone()), other));
            }
            sym!(Node::Global(instance, Global::Destruct(destructor)), node) => {
                match (
                    self.destruct(node).expect("Continuation-continuation!"),
                    destructor,
                ) {
                    (Value::Break, GlobalCont::Continue) => {
                        self.rewrites.r#continue += 1;
                    }
                    (Value::Pair(a0, a1), GlobalCont::Par(b0, b1)) => {
                        self.rewrites.receive += 1;
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
                        self.rewrites.r#match += 1;
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
                    a.variant_name(),
                    b.variant_name()
                )
            }
        };
        None
    }
}

impl Node {
    pub fn variant_name(&self) -> String {
        match self {
            Node::Linear(l) => format!("Linear.{}", l.variant_name()),
            Node::Shared(s) => format!("Shared.{}", s.variant_name()),
            Node::Global(i, g) => {
                format!("Global@{:x}.{}", i.identifier(), g.variant_name())
            }
        }
    }
}

impl Linear {
    pub fn variant_name(&self) -> String {
        match self {
            Linear::Value(v) => format!("Value({})", v.variant_name()),
            Linear::Request(_) => "Request".to_owned(),
            Linear::ShareHole(_) => "ShareHole".to_owned(),
        }
    }
}

impl Shared {
    pub fn variant_name(&self) -> String {
        match self {
            Shared::Async(_) => "Async".to_owned(),
            Shared::Sync(sync) => format!("Sync({})", sync.variant_name()),
        }
    }
}

impl SyncShared {
    pub fn variant_name(&self) -> String {
        match self {
            SyncShared::Package(_, inner) => format!("Package({})", inner.variant_name()),
            SyncShared::Value(v) => format!("Value({})", v.variant_name()),
        }
    }
}

impl Global {
    pub fn variant_name(&self) -> String {
        match self {
            Global::Variable(_) => "Variable".into(),

            Global::Package(_, _, _) => "Package".into(),

            Global::Destruct(c) => format!("Destruct({})", c.variant_name()),

            Global::Value(v) => format!("Value({})", v.variant_name()),

            Global::Fanout(_idx) => "Fanout".into(),
        }
    }
}

impl GlobalCont {
    pub fn variant_name(&self) -> String {
        match self {
            GlobalCont::Continue => "Continue".into(),

            GlobalCont::Par(_a, _b) => "Par".into(),

            GlobalCont::Choice(_ptr, ..) => "Choice".into(),
        }
    }
}

impl<P> Value<P> {
    pub fn variant_name(&self) -> String {
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
