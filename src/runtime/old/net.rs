//! This module contains a simple implementation for Interaction Combinators.
//! It is not performant; it's mainly here to act as a storage and interchange format

use std::collections::{BTreeMap, HashMap, VecDeque};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::time::{Duration, Instant};

use arcstr::ArcStr;
use bytes::Bytes;
use futures::channel::{mpsc, oneshot};
use futures::future::RemoteHandle;
use futures::task::{Spawn, SpawnExt};
use futures::StreamExt;
use indexmap::IndexMap;
use num_bigint::BigInt;

use crate::par::primitive::{ParString, Primitive};

use super::readback::{private::NetWrapper, Handle};

pub type VarId = usize;

pub fn number_to_string(mut number: usize) -> String {
    let mut result = String::new();
    number += 1;
    while number > 0 {
        let remainder = (number - 1) % 26;
        let character = (b'a' + remainder as u8) as char;
        result.insert(0, character);
        number = (number - 1) / 26;
    }
    result
}

/// A `Tree` corresponds to a port that is the root of a tree of interaction combinators.
/// The `Tree` enum itself contains the whole tree, although it some parts of it might be inside
/// half-linked `Tree::Var`s
pub enum Tree {
    Break,
    Continue,
    Era,
    Par(Box<Tree>, Box<Tree>),
    Times(Box<Tree>, Box<Tree>),
    Dup(Box<Tree>, Box<Tree>),
    Box_(Box<Tree>, usize),
    Signal(ArcStr, Box<Tree>),
    Choice(Box<Tree>, Arc<HashMap<ArcStr, usize>>, Option<usize>),
    Var(usize),
    Package(usize),

    SignalRequest(oneshot::Sender<(ArcStr, Box<Tree>)>),

    Primitive(Primitive),
    IntRequest(oneshot::Sender<BigInt>),
    StringRequest(oneshot::Sender<ParString>),
    BytesRequest(oneshot::Sender<Bytes>),

    External(fn(crate::runtime::Handle) -> Pin<Box<dyn Send + Future<Output = ()>>>),
    ExternalBox(
        Arc<
            dyn Send
                + Sync
                + Fn(crate::runtime::Handle) -> Pin<Box<dyn Send + Future<Output = ()>>>,
        >,
    ),
}

impl Tree {
    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        match self {
            Self::Var(x) => *x = m(*x),
            Self::Par(a, b) | Self::Times(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            Self::Box_(context, _) => {
                context.map_vars(m);
            }
            Self::Signal(_, payload) => {
                payload.map_vars(m);
            }
            Self::Choice(context, _, _) => {
                context.map_vars(m);
            }
            Self::Dup(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            Self::Era
            | Self::Break
            | Self::Continue
            | Self::Package(_)
            | Self::Primitive(_)
            | Self::SignalRequest(_)
            | Self::IntRequest(_)
            | Self::StringRequest(_)
            | Self::BytesRequest(_)
            | Self::External(_)
            | Self::ExternalBox(_) => {}
        }
    }
}

impl core::fmt::Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Era => f.debug_tuple("Era").finish(),
            Self::Break => f.debug_tuple("Break").finish(),
            Self::Continue => f.debug_tuple("Continue").finish(),
            Self::Times(a, b) => f.debug_tuple("Times").field(a).field(b).finish(),
            Self::Par(a, b) => f.debug_tuple("Par").field(a).field(b).finish(),
            Self::Dup(a, b) => f.debug_tuple("Dup").field(a).field(b).finish(),
            Self::Box_(context, package) => {
                f.debug_tuple("Box").field(context).field(package).finish()
            }
            Self::Signal(signal, payload) => f
                .debug_tuple("Signal")
                .field(signal)
                .field(payload)
                .finish(),
            Self::Choice(context, branches, else_branch) => f
                .debug_tuple("Choice")
                .field(context)
                .field(branches)
                .field(else_branch)
                .finish(),
            Self::Var(id) => f.debug_tuple("Var").field(id).finish(),
            Self::Package(id) => f.debug_tuple("Package").field(id).finish(),
            Self::Primitive(p) => f.debug_tuple("Primitive").field(p).finish(),
            Self::SignalRequest(_) => f.debug_tuple("SignalRequest").field(&"<channel>").finish(),
            Self::IntRequest(_) => f.debug_tuple("IntRequest").field(&"<channel>").finish(),
            Self::StringRequest(_) => f.debug_tuple("StringRequest").field(&"<channel>").finish(),
            Self::BytesRequest(_) => f.debug_tuple("BytesRequest").field(&"<channel>").finish(),
            Self::External(_) => f.debug_tuple("External").field(&"<function>").finish(),
            Self::ExternalBox(_) => f.debug_tuple("ExternalBox").field(&"<closure>").finish(),
        }
    }
}

impl Clone for Tree {
    fn clone(&self) -> Self {
        match self {
            Self::Era => Self::Era,
            Self::Break => Self::Break,
            Self::Continue => Self::Continue,
            Self::Times(a, b) => Self::Times(a.clone(), b.clone()),
            Self::Par(a, b) => Self::Par(a.clone(), b.clone()),
            Self::Dup(a, b) => Self::Dup(a.clone(), b.clone()),
            Self::Box_(context, package) => Self::Box_(context.clone(), package.clone()),
            Self::Signal(signal, payload) => Self::Signal(signal.clone(), payload.clone()),
            Self::Choice(context, branches, else_branch) => {
                Self::Choice(context.clone(), Arc::clone(branches), else_branch.clone())
            }
            Self::Var(id) => Self::Var(id.clone()),
            Self::Package(id) => Self::Package(id.clone()),
            Self::Primitive(p) => Self::Primitive(p.clone()),
            Self::SignalRequest(_) => panic!("cannot clone Tree::SignalRequest"),
            Self::IntRequest(_) => panic!("cannot clone Tree::IntRequest"),
            Self::StringRequest(_) => panic!("cannot clone Tree::StringRequest"),
            Self::BytesRequest(_) => panic!("cannot clone Tree::BytesRequest"),
            Self::External(f) => Self::External(*f),
            Self::ExternalBox(f) => Self::ExternalBox(Arc::clone(f)),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Rewrites {
    pub commute: u128,
    pub annihilate: u128,
    pub signal: u128,
    pub era: u128,
    pub resp: u128,
    pub expand: u128,
    pub derelict: u128,
    last_busy_start: Option<Instant>,
    pub busy_duration: Duration,
}

impl core::ops::Add<Rewrites> for Rewrites {
    type Output = Rewrites;

    fn add(self, rhs: Rewrites) -> Self::Output {
        Self {
            commute: self.commute + rhs.commute,
            annihilate: self.annihilate + rhs.annihilate,
            signal: self.signal + rhs.signal,
            era: self.era + rhs.era,
            expand: self.expand + rhs.expand,
            resp: self.resp + rhs.resp,
            derelict: self.derelict + rhs.derelict,
            last_busy_start: match (self.last_busy_start, rhs.last_busy_start) {
                (None, None) => None,
                (Some(t), None) | (None, Some(t)) => Some(t),
                (Some(t1), Some(t2)) => Some(t1.min(t2)),
            },
            busy_duration: self.busy_duration + rhs.busy_duration,
        }
    }
}

#[allow(unused)]
impl Rewrites {
    pub fn total(&self) -> u128 {
        self.commute + self.annihilate + self.signal + self.expand + self.era + self.resp
    }

    pub fn total_per_second(&self) -> u128 {
        let micros = self.busy_duration.as_micros();
        if micros == 0 {
            return 0;
        }
        (self.total() as u128 * 1_000_000) / micros
    }
}

#[derive(Default, Clone)]
/// A Net represents the current state of the runtime
/// It contains a list of active pairs, as well as a list of free ports.
/// It also stores a map of variables, which records whether variables were linked by either of their sides
pub struct Net {
    pub ports: VecDeque<Tree>,
    pub redexes: VecDeque<(Tree, Tree)>,
    pub variables: Variables,
    pub packages: Arc<IndexMap<usize, Net>>,
    pub rewrites: Rewrites,
    waiting_for_reducer: Vec<(Tree, Tree)>,
    reducer: Option<Reducer>,
}

pub(crate) enum ReducerMessage {
    Ping,
}

#[derive(Clone)]
struct Reducer {
    net: Weak<Mutex<Net>>,
    spawner: Arc<dyn Spawn + Send + Sync>,
    notify: mpsc::UnboundedSender<ReducerMessage>,
    handle_count: Arc<AtomicUsize>,
}

impl Reducer {
    fn spawn_external(
        &self,
        f: impl Fn(Handle) -> Pin<Box<dyn Send + Future<Output = ()>>>,
        tree: Tree,
    ) {
        if let Some(net) = self.net.upgrade() {
            let notify = self.notify.clone();
            let handle_count = self.handle_count.clone();
            let handle = Handle::new(net, notify, handle_count, tree);
            let future = f(handle);
            self.spawner.spawn(future).expect("spawn failed");
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Variables {
    vars: Vec<VarState>,
    free: Vec<VarId>,
}

#[derive(Debug, Clone)]
pub enum VarState {
    Free,
    Linked(Tree),
}

impl Variables {
    pub fn get(&self, id: VarId) -> Option<&VarState> {
        self.vars.get(id)
    }

    pub fn remove_linked(&mut self, id: VarId) -> Result<Tree, &mut VarState> {
        while self.vars.len() <= id {
            self.vars.push(VarState::Free);
        }

        match self.vars.get_mut(id) {
            Some(state) => match state {
                VarState::Linked(tree) => {
                    self.free.push(id);
                    let tree = std::mem::replace(tree, Tree::Era);
                    let _ = std::mem::replace(state, VarState::Free);
                    Ok(tree)
                }
                _ => Err(state),
            },
            None => unreachable!(),
        }
    }

    pub fn alloc(&mut self) -> VarId {
        match self.free.pop() {
            Some(id) => id,
            None => {
                self.vars.push(VarState::Free);
                self.vars.len() - 1
            }
        }
    }
}

impl Net {
    pub fn start_reducer(
        mut self,
        spawner: Arc<dyn Spawn + Send + Sync>,
    ) -> (NetWrapper, RemoteHandle<()>) {
        if self.reducer.is_some() {
            panic!("reducer already started");
        }

        self.redexes.extend(self.waiting_for_reducer.drain(..));

        let (notify, mut resume) = mpsc::unbounded();
        let net = Arc::new(Mutex::new(self));
        let weak_net = Arc::downgrade(&net);
        let handle_count = Arc::new(AtomicUsize::new(0));
        net.lock().unwrap().reducer = Some(Reducer {
            net: weak_net,
            spawner: Arc::clone(&spawner),
            notify: notify.clone(),
            handle_count: handle_count.clone(),
        });

        let net_wrapper = NetWrapper::new(Arc::clone(&net), notify.clone(), handle_count.clone());

        let net = Arc::clone(&net);
        let future = spawner
            .spawn_with_handle(async move {
                loop {
                    {
                        let mut lock = net.lock().expect("lock failed");
                        while lock.reduce_one() {}
                        if handle_count.load(Ordering::SeqCst) == 0 {
                            break;
                        }
                    }

                    match resume.next().await {
                        Some(ReducerMessage::Ping) => continue,
                        None => break,
                    }
                }
            })
            .expect("spawn failed");
        (net_wrapper, future)
    }

    pub fn spawn(&self, fut: Pin<Box<dyn Send + Future<Output = ()>>>) {
        let Some(reducer) = &self.reducer else {
            panic!("reducer not started");
        };
        reducer.spawner.spawn(fut).expect("spawn failed");
    }

    pub fn notify_reducer(&self) {
        let Some(reducer) = &self.reducer else {
            panic!("reducer not started");
        };
        let res = reducer.notify.unbounded_send(ReducerMessage::Ping);
        if res.is_err() {
            println!("Warning: Failed to ping reducer");
        }
        // .expect("notify ping failed");
    }

    fn interact(&mut self, a: Tree, b: Tree) {
        macro_rules! sym {
            ($a: pat, $b: pat) => {
                ($a, $b) | ($b, $a)
            };
        }

        use Tree::*;
        match (a, b) {
            (a @ Var(..), b @ _) | (a @ _, b @ Var(..)) => {
                // link anyway
                self.link(a, b);
            }
            sym!(Era | Continue, Break) | sym!(Break | Continue, Era) => {
                self.rewrites.era += 1;
            }
            sym!(Times(a0, a1), Era) | sym!(Par(a0, a1), Era) | sym!(Dup(a0, a1), Era) => {
                self.link(*a0, Era);
                self.link(*a1, Era);
                self.rewrites.era += 1;
            }
            sym!(Times(a0, a1), Par(b0, b1)) | (Dup(a0, a1), Dup(b0, b1)) => {
                self.link(*a0, *b0);
                self.link(*a1, *b1);
                self.rewrites.annihilate += 1;
            }
            sym!(Dup(a0, a1), Break) => {
                self.link(*a0, Break);
                self.link(*a1, Break);
                self.rewrites.era += 1;
            }
            sym!(Dup(a0, a1), Continue) => {
                self.link(*a0, Continue);
                self.link(*a1, Continue);
                self.rewrites.era += 1;
            }
            sym!(Times(a0, a1), Dup(b0, b1)) => {
                let (a00, b00) = self.create_wire();
                let (a01, b01) = self.create_wire();
                let (a10, b10) = self.create_wire();
                let (a11, b11) = self.create_wire();
                self.link(*a0, Tree::Dup(Box::new(a00), Box::new(a01)));
                self.link(*a1, Tree::Dup(Box::new(a10), Box::new(a11)));
                self.link(*b0, Tree::Times(Box::new(b00), Box::new(b10)));
                self.link(*b1, Tree::Times(Box::new(b01), Box::new(b11)));
                self.rewrites.commute += 1;
            }
            sym!(Par(a0, a1), Dup(b0, b1)) => {
                let (a00, b00) = self.create_wire();
                let (a01, b01) = self.create_wire();
                let (a10, b10) = self.create_wire();
                let (a11, b11) = self.create_wire();
                self.link(*a0, Tree::Dup(Box::new(a00), Box::new(a01)));
                self.link(*a1, Tree::Dup(Box::new(a10), Box::new(a11)));
                self.link(*b0, Tree::Par(Box::new(b00), Box::new(b10)));
                self.link(*b1, Tree::Par(Box::new(b01), Box::new(b11)));
                self.rewrites.commute += 1;
            }
            (Box_(context, _), Era) | (Era, Box_(context, _)) => {
                self.link(*context, Tree::Era);
                self.rewrites.era += 1;
            }
            (Box_(context, package), Dup(b0, b1)) | (Dup(b0, b1), Box_(context, package)) => {
                let (a00, b00) = self.create_wire();
                let (a10, b10) = self.create_wire();
                self.link(*context, Tree::Dup(Box::new(b00), Box::new(b10)));
                self.link(*b0, Tree::Box_(Box::new(a00), package));
                self.link(*b1, Tree::Box_(Box::new(a10), package));
                self.rewrites.commute += 1;
            }
            (Box_(context, package), a) | (a, Box_(context, package)) => {
                self.link(Tree::Times(context, Box::new(a)), Tree::Package(package));
                self.rewrites.derelict += 1;
            }
            (Signal(signal, payload), Choice(context, branches, else_branch))
            | (Choice(context, branches, else_branch), Signal(signal, payload)) => {
                match branches.get(&signal).copied() {
                    Some(package_id) => {
                        self.link(Tree::Times(context, payload), Tree::Package(package_id));
                    }
                    None => self.link(
                        Tree::Times(context, Box::new(Signal(signal, payload))),
                        Tree::Package(else_branch.unwrap()),
                    ),
                }
                self.rewrites.signal += 1;
            }
            (Signal(_, payload), Era) | (Era, Signal(_, payload)) => {
                self.link(*payload, Era);
                self.rewrites.era += 1;
            }
            (Signal(signal, payload), Dup(b0, b1)) | (Dup(b0, b1), Signal(signal, payload)) => {
                let (a00, b00) = self.create_wire();
                let (a10, b10) = self.create_wire();
                self.link(*payload, Tree::Dup(Box::new(b00), Box::new(b10)));
                self.link(*b0, Tree::Signal(signal.clone(), Box::new(a00)));
                self.link(*b1, Tree::Signal(signal, Box::new(a10)));
                self.rewrites.commute += 1;
            }
            (Package(_), Era) | (Era, Package(_)) => {
                self.rewrites.era += 1;
            }
            (Package(id), Dup(a, b)) | (Dup(a, b), Package(id)) => {
                self.link(*a, Package(id));
                self.link(*b, Package(id));
                self.rewrites.commute += 1;
            }
            (Package(_), Package(_)) => {
                unreachable!("Packages should not interact with packages");
            }
            (Package(id), a) | (a, Package(id)) => {
                let b = self.dereference_package(id);
                self.interact(a, b);
                self.rewrites.expand += 1;
            }
            (Primitive(p), a) | (a, Primitive(p)) => {
                self.primitive_interact(p, a);
            }
            (SignalRequest(tx), Signal(signal, payload))
            | (Signal(signal, payload), SignalRequest(tx)) => {
                tx.send((signal, payload)).expect("receiver dropped");
                self.rewrites.resp += 1;
            }
            (External(f), a) | (a, External(f)) => match &self.reducer {
                Some(reducer) => reducer.spawn_external(|x| f(crate::runtime::Handle::Old(x)), a),
                None => self.waiting_for_reducer.push((External(f), a)),
            },
            (ExternalBox(_), Era) | (Era, ExternalBox(_)) => {
                self.rewrites.era += 1;
            }
            (ExternalBox(f), Dup(a, b)) | (Dup(a, b), ExternalBox(f)) => {
                self.link(*a, ExternalBox(Arc::clone(&f)));
                self.link(*b, ExternalBox(f));
                self.rewrites.commute += 1;
            }
            (ExternalBox(f), a) | (a, ExternalBox(f)) => match &self.reducer {
                Some(reducer) => {
                    reducer.spawn_external(|x| f(crate::runtime::Handle::Old(x)), a);
                }
                None => self.waiting_for_reducer.push((ExternalBox(f), a)),
            },
            (a, b) => panic!("Invalid combinator interaction: {:?} <> {:?}", a, b),
        }
    }

    fn primitive_interact(&mut self, p: Primitive, tree: Tree) {
        match (p, tree) {
            (Primitive::Int(i), Tree::IntRequest(resp)) => {
                resp.send(i).expect("receiver dropped");
                self.rewrites.resp += 1;
            }
            (Primitive::String(s), Tree::StringRequest(resp)) => {
                resp.send(s).expect("receiver dropped");
                self.rewrites.resp += 1;
            }
            (Primitive::Bytes(b), Tree::BytesRequest(resp)) => {
                resp.send(b).expect("receiver dropped");
                self.rewrites.resp += 1;
            }
            (Primitive::String(s), Tree::BytesRequest(resp)) => {
                resp.send(s.as_bytes()).expect("receiver dropped");
                self.rewrites.resp += 1;
            }

            (_, Tree::Era) => {
                self.rewrites.era += 1;
            }
            (p, Tree::Dup(a, b)) => {
                self.link(Tree::Primitive(p.clone()), *a);
                self.link(Tree::Primitive(p), *b);
                self.rewrites.commute += 1;
            }

            (p, tree) => unreachable!("Invalid primitive interaction of {:?} with {:?}", p, tree),
        }
    }

    fn dereference_package(&mut self, package: usize) -> Tree {
        let net = self
            .packages
            .get(&package)
            .unwrap_or_else(|| panic!("Unknown package with ID {}", package))
            .clone();
        self.inject_net(net)
    }

    pub fn inject_net(&mut self, mut net: Net) -> Tree {
        // Now, we have to freshen all variables in the tree
        let mut allocated = HashMap::new();
        net.map_vars(&mut |id| {
            *allocated
                .entry(id)
                .or_insert_with(|| self.variables.alloc())
        });
        self.redexes.append(&mut net.redexes);
        if self.reducer.is_some() {
            self.redexes.extend(net.waiting_for_reducer.drain(..));
        } else {
            self.waiting_for_reducer
                .append(&mut net.waiting_for_reducer);
        }
        for (id, state) in net.variables.vars.drain(..).enumerate() {
            if let Some(new_id) = allocated.get(&id) {
                self.variables.vars[*new_id] = state
            }
        }
        self.rewrites = core::mem::take(&mut self.rewrites) + net.rewrites;
        net.ports.pop_back().unwrap()
    }

    /// Returns whether a reduction was carried out
    pub fn reduce_one(&mut self) -> bool {
        if self.rewrites.last_busy_start.is_none() {
            self.rewrites.last_busy_start = Some(Instant::now());
        }

        let reduced = if let Some((a, b)) = self.redexes.pop_front() {
            self.interact(a, b);
            true
        } else {
            false
        };

        if !reduced {
            if let Some(last_busy_start) = self.rewrites.last_busy_start.take() {
                self.rewrites.busy_duration += last_busy_start.elapsed();
            }
        }

        reduced
    }

    /// Where vars occur in the given tree which already have been linked from the other side, finish linking them.
    pub fn substitute_tree(&mut self, tree: &mut Tree) {
        match tree {
            Tree::Times(a, b) | Tree::Par(a, b) | Tree::Dup(a, b) => {
                self.substitute_tree(a);
                self.substitute_tree(b);
            }
            Tree::Box_(context, _) => self.substitute_tree(context),
            Tree::Signal(_, payload) => self.substitute_tree(payload),
            Tree::Choice(context, _, _) => self.substitute_tree(context),
            Tree::Var(id) => {
                if let Ok(mut a) = self.variables.remove_linked(*id) {
                    self.substitute_tree(&mut a);
                    *tree = a;
                }
            }
            Tree::Era
            | Tree::Continue
            | Tree::Break
            | Tree::Package(_)
            | Tree::Primitive(_)
            | Tree::SignalRequest(_)
            | Tree::IntRequest(_)
            | Tree::StringRequest(_)
            | Tree::BytesRequest(_)
            | Tree::External(_)
            | Tree::ExternalBox(_) => {}
        }
    }

    pub fn normal(&mut self) {
        while self.reduce_one() {}
        // dereference all variables
        let mut ports = core::mem::take(&mut self.ports);
        ports.iter_mut().for_each(|x| self.substitute_tree(x));
        self.ports = ports;
    }

    pub fn link(&mut self, a: Tree, b: Tree) {
        match (a, b) {
            (Tree::Var(id), y) | (y, Tree::Var(id)) => match self.variables.remove_linked(id) {
                Ok(x) => {
                    self.link(x, y);
                }
                Err(state) => {
                    *state = VarState::Linked(y);
                }
            },
            (Tree::Era, y) | (y, Tree::Era) => self.redexes.push_front((Tree::Era, y)),
            (x, y) => self.redexes.push_back((x, y)),
        }
        /*if let Tree::Var(id) = a {
            match self.variables.remove_linked(id) {
                Ok(a) => {
                    self.link(a, b);
                }
                Err(state) => {
                    *state = VarState::Linked(b);
                }
            }
        } else if let Tree::Var(id) = b {
            self.link(Tree::Var(id), a)
        } else {
            self.redexes.push_back((a, b))
        }*/
    }

    pub fn create_wire(&mut self) -> (Tree, Tree) {
        let id = self.variables.alloc();
        (Tree::Var(id), Tree::Var(id))
    }

    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        for port in &mut self.ports {
            port.map_vars(m);
        }
        for (a, b) in &mut self.redexes {
            a.map_vars(m);
            b.map_vars(m);
        }
        for (a, b) in &mut self.waiting_for_reducer {
            a.map_vars(m);
            b.map_vars(m);
        }
        for state in &mut self.variables.vars {
            if let VarState::Linked(tree) = state {
                tree.map_vars(m);
            }
        }
        //for free in &mut self.variables.free {
        //    *free = m(*free);
        //}
    }

    pub fn show(&self) -> String {
        self.show_indent(0)
    }

    pub fn show_indent(&self, indent: usize) -> String {
        use core::fmt::Write;
        let indent_string = "    ".repeat(indent);
        let mut s = String::new();
        for i in &self.ports {
            write!(&mut s, "{}{}\n", indent_string, self.show_tree(i)).unwrap();
        }
        for (a, b) in self.redexes.iter().chain(self.waiting_for_reducer.iter()) {
            write!(
                &mut s,
                "{}{} ~ {}\n",
                indent_string,
                self.show_tree(a),
                self.show_tree(b)
            )
            .unwrap();
        }
        s
    }

    pub fn show_tree(&self, t: &Tree) -> String {
        match t {
            Tree::Var(id) => {
                if let Some(VarState::Linked(b)) = self.variables.get(*id) {
                    self.show_tree(b)
                } else {
                    number_to_string(*id)
                }
            }
            Tree::Era => format!("*"),
            Tree::Break => format!("!"),
            Tree::Continue => format!("?"),
            Tree::Par(a, b) => format!("[{}] {}", self.show_tree(b), self.show_tree(a)),
            Tree::Times(a, b) => format!("({}) {}", self.show_tree(b), self.show_tree(a)),
            Tree::Dup(a, b) => format!("{{{} {}}}", self.show_tree(a), self.show_tree(b)),
            Tree::Box_(context, package) => format!("box({} {})", self.show_tree(context), package),
            Tree::Signal(signal, payload) => {
                format!("signal({} {})", signal, self.show_tree(payload))
            }
            Tree::Choice(context, branches, else_branch) => {
                format!(
                    "choice({} {:?} {:?})",
                    self.show_tree(context),
                    branches,
                    else_branch
                )
            }
            Tree::Package(id) => format!("@{}", id),

            Tree::Primitive(Primitive::Int(i)) => format!("primitive({})", i),
            Tree::Primitive(Primitive::String(s)) => format!("primitive({:?})", s),
            Tree::Primitive(Primitive::Bytes(b)) => format!("primitive({:?})", b),

            Tree::SignalRequest(_) => format!("<signal request>"),
            Tree::IntRequest(_) => format!("<int request>"),
            Tree::StringRequest(_) => format!("<string request>"),
            Tree::BytesRequest(_) => format!("<bytes request>"),

            Tree::External(_) => format!("<external>"),
            Tree::ExternalBox(_) => format!("<external box>"),
        }
    }

    fn assert_no_vicious(&self) {
        for (var, tree) in self.variables.vars.iter().enumerate() {
            if let VarState::Linked(tree) = tree {
                self.assert_tree_not_contains(tree, &var);
            }
        }
    }

    fn assert_tree_not_contains(&self, tree: &Tree, idx: &usize) {
        match tree {
            Tree::Par(a, b) | Tree::Times(a, b) | Tree::Dup(a, b) => {
                self.assert_tree_not_contains(a, idx);
                self.assert_tree_not_contains(b, idx);
            }
            Tree::Box_(context, _) => {
                self.assert_tree_not_contains(context, idx);
            }
            Tree::Signal(_, payload) => {
                self.assert_tree_not_contains(payload, idx);
            }
            Tree::Choice(context, _, _) => {
                self.assert_tree_not_contains(context, idx);
            }
            Tree::Var(id) => {
                if id == idx {
                    panic!("Vicious circle detected");
                }
                if let Some(VarState::Linked(tree)) = self.variables.get(*id) {
                    self.assert_tree_not_contains(tree, idx);
                }
            }
            Tree::Era
            | Tree::Continue
            | Tree::Break
            | Tree::Package(_)
            | Tree::Primitive(_)
            | Tree::SignalRequest(_)
            | Tree::IntRequest(_)
            | Tree::StringRequest(_)
            | Tree::BytesRequest(_)
            | Tree::External(_)
            | Tree::ExternalBox(_) => {}
        }
    }

    pub fn assert_valid_with<'a>(&self, iter: impl Iterator<Item = &'a Tree>) {
        self.assert_no_vicious();

        let mut vars = vec![];
        for (a, b) in &self.redexes {
            vars.append(&mut self.assert_tree_valid(a));
            vars.append(&mut self.assert_tree_valid(b));
        }
        for (a, b) in &self.waiting_for_reducer {
            vars.append(&mut self.assert_tree_valid(a));
            vars.append(&mut self.assert_tree_valid(b));
        }
        for tree in &self.ports {
            vars.append(&mut self.assert_tree_valid(tree));
        }
        for tree in iter {
            vars.append(&mut self.assert_tree_valid(tree));
        }

        let vars_counter = vars.into_iter().fold(BTreeMap::new(), |mut acc, x| {
            *acc.entry(x).or_insert(0) += 1;
            acc
        });
        for (var, count) in vars_counter {
            if count != 2 {
                let name = number_to_string(var.clone());
                println!("net = {}", self.show());
                println!("num ports {}", self.ports.len());
                panic!("Variable {name} was used {count} times");
            }
        }

        // Perhaps we should check that all packages are valid too
        // Right now this creates nonsensical error messages
        // And in any case, each package is checked when it is created
    }

    pub fn assert_valid(&self) {
        self.assert_valid_with(std::iter::empty());
    }

    fn assert_tree_valid(&self, tree: &Tree) -> Vec<usize> {
        match tree {
            Tree::Times(a, b) | Tree::Par(a, b) | Tree::Dup(a, b) => {
                let mut a = self.assert_tree_valid(a.as_ref());
                let mut b = self.assert_tree_valid(b.as_ref());
                a.append(&mut b);
                a
            }
            Tree::Box_(context, _) => self.assert_tree_valid(context),
            Tree::Signal(_, payload) => self.assert_tree_valid(payload),
            Tree::Choice(context, _, _) => self.assert_tree_valid(context),
            Tree::Era | Tree::Continue | Tree::Break => {
                vec![]
            }
            Tree::Var(idx) => {
                if let Some(VarState::Linked(tree)) = self.variables.get(*idx) {
                    self.assert_tree_valid(tree)
                } else {
                    vec![idx.clone()]
                }
            }
            Tree::Package(idx) => {
                if self.packages.get(idx).is_some() {
                    vec![]
                } else {
                    panic!("Package with id {idx} is not found")
                }
            }
            Tree::Primitive(_) => vec![],
            Tree::SignalRequest(_) => vec![],
            Tree::IntRequest(_) => vec![],
            Tree::StringRequest(_) => vec![],
            Tree::BytesRequest(_) => vec![],
            Tree::External(_) => vec![],
            Tree::ExternalBox(_) => vec![],
        }
    }
}
