use arcstr::literal;
use byteview::ByteView;
use num_bigint::BigInt;
use std::{cmp::Ordering, sync::Arc};

use crate::{
    icombs::readback::Handle,
    par::{
        builtin::{byte::ByteClass, list::readback_list},
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef::external("Bytes", &[], Type::bytes())],
        declarations: vec![],
        definitions: vec![
            Definition::external("Builder", Type::name(None, "Builder", vec![]), |handle| {
                Box::pin(bytes_builder(handle))
            }),
            Definition::external(
                "Reader",
                Type::function(
                    Type::bytes(),
                    Type::name(None, "Reader", vec![Type::either(vec![])]),
                ),
                |handle| Box::pin(bytes_reader(handle)),
            ),
        ],
    }
}

async fn bytes_builder(mut handle: Handle) {
    let mut buf = Vec::<u8>::new();
    loop {
        match handle.case().await.as_str() {
            "add" => {
                buf.extend(handle.receive().bytes().await.as_ref());
            }
            "build" => {
                handle.provide_bytes(ByteView::from(buf));
                break;
            }
            _ => unreachable!(),
        }
    }
}

async fn bytes_reader(mut handle: Handle) {
    let mut remainder = handle.receive().bytes().await;
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
            }
            "byte" => match remainder.get(0) {
                Some(b) => {
                    handle.signal(literal!("byte"));
                    handle.send().provide_byte(*b);
                    remainder = remainder.slice(1..);
                }
                None => {
                    handle.signal(literal!("end"));
                    handle.signal(literal!("ok"));
                    handle.break_();
                    return;
                }
            },
            "match" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                if remainder.is_empty() {
                    handle.signal(literal!("end"));
                    handle.signal(literal!("ok"));
                    handle.break_();
                    return;
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                let mut best_match = None;
                for (pos, &b) in remainder.iter().enumerate() {
                    match (m.leftmost_feasible_split(pos), best_match) {
                        (Some(fi), Some((bi, _))) if fi > bi => break,
                        (None, _) => break,
                        _ => {}
                    }
                    m.advance(pos, b);
                    match (m.leftmost_accepting_split(), best_match) {
                        (Some(ai), Some((bi, _))) if ai <= bi => best_match = Some((ai, pos + 1)),
                        (Some(ai), None) => best_match = Some((ai, pos + 1)),
                        _ => {}
                    }
                }

                match best_match {
                    Some((i, j)) => {
                        handle.signal(literal!("match"));
                        handle.send().provide_bytes(remainder.slice(..i));
                        handle.send().provide_bytes(remainder.slice(i..j));
                        remainder = remainder.slice(j..);
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "matchEnd" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                if remainder.is_empty() {
                    handle.signal(literal!("end"));
                    handle.signal(literal!("ok"));
                    handle.break_();
                    return;
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                for (pos, &b) in remainder.iter().enumerate() {
                    if m.accepts() == None {
                        break;
                    }
                    m.advance(pos, b);
                }

                match m.leftmost_accepting_split() {
                    Some(i) => {
                        handle.signal(literal!("match"));
                        handle.send().provide_bytes(remainder.slice(..i));
                        handle.send().provide_bytes(remainder.slice(i..));
                        handle.break_();
                        return;
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => {
                handle.signal(literal!("ok"));
                handle.provide_bytes(remainder);
                return;
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    Nil,
    All,
    Empty,
    Min(BigInt),
    Max(BigInt),
    Bytes(ByteView),
    One(ByteClass),
    Non(ByteClass),
    Concat(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Repeat(Box<Self>),
    Repeat1(Box<Self>),
}

impl Pattern {
    pub(crate) async fn readback(mut handle: Handle) -> Box<Self> {
        match handle.case().await.as_str() {
            "and" => {
                // .and List<self>
                let mut conj = Box::new(Self::All);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    conj = Box::new(Self::And(p, conj));
                }
                conj
            }
            "concat" => {
                // .concat List<self>
                let mut conc = Box::new(Self::Empty);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    conc = Box::new(Self::Concat(p, conc));
                }
                conc
            }
            "empty" => {
                // .empty!
                handle.break_();
                Box::new(Self::Empty)
            }
            "min" => {
                // .min Nat
                let n = handle.nat().await;
                Box::new(Self::Min(n))
            }
            "max" => {
                // .max Nat
                let n = handle.nat().await;
                Box::new(Self::Max(n))
            }
            "non" => {
                // .non Byte.Class
                let class = ByteClass::readback(handle).await;
                Box::new(Self::Non(class))
            }
            "one" => {
                // .one Byte.Class
                let class = ByteClass::readback(handle).await;
                Box::new(Self::One(class))
            }
            "or" => {
                // .or List<self>,
                let mut disj = Box::new(Self::Nil);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    disj = Box::new(Self::Or(p, disj));
                }
                disj
            }
            "repeat" => {
                // .repeat self
                let p = Box::pin(Self::readback(handle)).await;
                Box::new(Self::Repeat(p))
            }
            "repeat1" => {
                // .repeat1 self
                let p = Box::pin(Self::readback(handle)).await;
                Box::new(Self::Repeat1(p))
            }
            "str" => {
                // .bytes Bytes
                let bs = handle.bytes().await;
                Box::new(Self::Bytes(bs))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Machine {
    pattern: Box<Pattern>,
    inner: MachineInner,
}

impl Machine {
    pub(crate) fn start(pattern: Box<Pattern>) -> Self {
        let inner = MachineInner::start(&pattern, 0);
        Self { pattern, inner }
    }

    pub(crate) fn accepts(&self) -> Option<bool> {
        self.inner.accepts(&self.pattern)
    }

    pub(crate) fn advance(&mut self, pos: usize, b: u8) {
        self.inner.advance(&self.pattern, pos, b);
    }

    pub(crate) fn leftmost_accepting_split(&self) -> Option<usize> {
        let Pattern::Concat(_, p2) = self.pattern.as_ref() else {
            return None;
        };
        let State::Concat(_, heap) = &self.inner.state else {
            return None;
        };
        heap.iter()
            .filter(|m2| m2.accepts(p2) == Some(true))
            .map(|m2| m2.start)
            .min()
    }

    pub(crate) fn leftmost_feasible_split(&self, pos: usize) -> Option<usize> {
        let State::Concat(_, heap) = &self.inner.state else {
            return None;
        };
        heap.iter().map(|m2| m2.start).min().or(Some(pos))
    }
}

#[derive(Debug)]
struct MachineInner {
    state: State,
    start: usize,
}

impl MachineInner {
    fn start(pattern: &Pattern, start: usize) -> Self {
        let state = match pattern {
            Pattern::Nil => State::Halt,

            Pattern::All => State::Init,

            Pattern::Empty => State::Init,

            Pattern::Min(_) => State::Index(0),
            Pattern::Max(_) => State::Index(0),

            Pattern::Bytes(_) => State::Index(0),

            Pattern::One(_) => State::Index(0),
            Pattern::Non(_) => State::Index(0),

            Pattern::Concat(p1, p2) => {
                let prefix = Self::start(p1, start);
                let suffixes = if prefix.accepts(p1) == Some(true) {
                    vec![Self::start(p2, start)]
                } else {
                    vec![]
                };
                State::Concat(Box::new(prefix), suffixes)
            }

            Pattern::And(p1, p2) | Pattern::Or(p1, p2) => State::Pair(
                Box::new(Self::start(p1, start)),
                Box::new(Self::start(p2, start)),
            ),

            Pattern::Repeat(_) => State::Init,
            Pattern::Repeat1(p) => State::Heap(vec![Self::start(p, start)]),
        };

        Self { state, start }
    }

    fn accepts(&self, pattern: &Pattern) -> Option<bool> {
        match (pattern, &self.state) {
            (_, State::Halt) => None,

            (Pattern::All, State::Init) => Some(true),

            (Pattern::Empty, State::Init) => Some(true),

            (Pattern::Min(n), State::Index(i)) => Some(&BigInt::from(*i) >= n),
            (Pattern::Max(n), State::Index(i)) => Some(&BigInt::from(*i) <= n),

            (Pattern::Bytes(s), State::Index(i)) => Some(s.len() == *i),

            (Pattern::One(_), State::Index(i)) => Some(*i == 1),
            (Pattern::Non(_), State::Index(i)) => Some(*i == 1),

            (Pattern::Concat(p1, p2), State::Concat(m1, heap)) => heap
                .iter()
                .filter_map(|m2| m2.accepts(p2))
                .max()
                .or_else(|| m1.accepts(p1).map(|_| false)),

            (Pattern::And(p1, p2), State::Pair(m1, m2)) => match (m1.accepts(p1), m2.accepts(p2)) {
                (Some(a1), Some(a2)) => Some(a1 && a2),
                (None, _) | (_, None) => None,
            },

            (Pattern::Or(p1, p2), State::Pair(m1, m2)) => match (m1.accepts(p1), m2.accepts(p2)) {
                (Some(a1), Some(a2)) => Some(a1 || a2),
                (None, a) | (a, None) => a,
            },

            (Pattern::Repeat(_), State::Init) => Some(true),
            (Pattern::Repeat(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (Pattern::Repeat1(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (p, s) => unreachable!("invalid combination of pattern {:?} and state {:?}", p, s),
        }
    }

    fn advance(&mut self, pattern: &Pattern, pos: usize, b: u8) {
        match (pattern, &mut self.state) {
            (_, State::Halt) => {}

            (Pattern::All, State::Init) => {}

            (Pattern::Empty, State::Init) => self.state = State::Halt,

            (Pattern::Min(_), State::Index(i)) => *i += 1,
            (Pattern::Max(n), State::Index(i)) => {
                if &BigInt::from(*i) < n {
                    *i += 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::Bytes(s), State::Index(i)) => {
                if s.get(*i) == Some(&b) {
                    *i += 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::One(class), State::Index(i)) => {
                if *i == 0 && class.contains(b) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }
            (Pattern::Non(class), State::Index(i)) => {
                if *i == 0 && !class.contains(b) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::Concat(p1, p2), State::Concat(m1, heap)) => {
                m1.advance(p1, pos, b);
                for m2 in heap.iter_mut() {
                    m2.advance(p2, pos, b);
                }
                heap.retain(|m2| m2.state != State::Halt);
                if m1.accepts(p1) == Some(true) {
                    heap.push(Self::start(p2, pos + 1));
                }
                heap.sort_by_key(|m| m.start);
                heap.sort();
                heap.dedup();
                if m1.state == State::Halt && heap.is_empty() {
                    self.state = State::Halt;
                }
            }

            (Pattern::And(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, b);
                m2.advance(p2, pos, b);
                if m1.state == State::Halt || m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (Pattern::Or(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, b);
                m2.advance(p2, pos, b);
                if m1.state == State::Halt && m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (Pattern::Repeat(p), State::Init) => {
                let mut m = Self::start(p, pos);
                m.advance(p, pos, b);
                self.state = State::Heap(vec![m])
            }
            (Pattern::Repeat(p) | Pattern::Repeat1(p), State::Heap(heap)) => {
                if heap.iter().any(|m| m.accepts(p) == Some(true)) {
                    heap.push(Self::start(p, pos));
                }
                for m in heap.iter_mut() {
                    m.advance(p, pos, b);
                }
                heap.retain(|m| m.state != State::Halt);
                heap.sort_by_key(|m| m.start);
                heap.sort();
                heap.dedup();
                if heap.is_empty() {
                    self.state = State::Halt;
                }
            }

            (p, s) => unreachable!("invalid combination of pattern {:?} and state {:?}", p, s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum State {
    Init,
    Halt,
    Index(usize),
    Pair(Box<MachineInner>, Box<MachineInner>),
    Heap(Vec<MachineInner>),
    Concat(Box<MachineInner>, Vec<MachineInner>),
}

impl PartialEq for MachineInner {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}
impl Eq for MachineInner {}
impl PartialOrd for MachineInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.state.partial_cmp(&other.state)
    }
}
impl Ord for MachineInner {
    fn cmp(&self, other: &Self) -> Ordering {
        self.state.cmp(&other.state)
    }
}
