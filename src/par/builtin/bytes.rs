use arcstr::literal;
use bytes::Bytes;
use futures::{channel::mpsc, StreamExt};
use num_bigint::BigInt;
use std::{
    cmp::Ordering,
    sync::{
        atomic::{AtomicBool, Ordering as AtomicOrdering},
        Arc, Mutex,
    },
};
use tokio::sync::Notify;

use crate::{
    par::{
        builtin::{
            byte::ByteClass,
            list::readback_list,
            parser::{provide_bytes_parser, ReaderRemainder},
        },
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
    runtime::Handle,
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
                "Parser",
                Type::function(
                    Type::bytes(),
                    Type::name(None, "Parser", vec![Type::either(vec![])]),
                ),
                |handle| Box::pin(bytes_parser(handle)),
            ),
            Definition::external(
                "ParserFromReader",
                Type::forall(
                    "e",
                    Type::function(
                        Type::name(None, "Reader", vec![Type::var("e")]),
                        Type::name(None, "Parser", vec![Type::var("e")]),
                    ),
                ),
                |handle| Box::pin(bytes_parser_from_reader(handle)),
            ),
            Definition::external(
                "Reader",
                Type::function(
                    Type::bytes(),
                    Type::name(None, "Reader", vec![Type::either(vec![])]),
                ),
                |handle| Box::pin(bytes_reader(handle)),
            ),
            Definition::external(
                "EmptyReader",
                Type::name(None, "Reader", vec![Type::either(vec![])]),
                |handle| Box::pin(bytes_empty_reader(handle)),
            ),
            Definition::external(
                "PipeReader",
                Type::forall(
                    "e",
                    Type::function(
                        Type::function(
                            Type::name(None, "Writer", vec![Type::break_()]),
                            Type::either(vec![("ok", Type::break_()), ("err", Type::var("e"))]),
                        ),
                        Type::name(None, "Reader", vec![Type::var("e")]),
                    ),
                ),
                |handle| Box::pin(bytes_pipe_reader(handle)),
            ),
            Definition::external(
                "Length",
                Type::function(Type::bytes(), Type::nat()),
                |handle| Box::pin(bytes_length(handle)),
            ),
            Definition::external(
                "Equals",
                Type::function(
                    Type::bytes(),
                    Type::function(Type::bytes(), Type::name(Some("Bool"), "Bool", vec![])),
                ),
                |handle| Box::pin(bytes_equals(handle)),
            ),
            Definition::external(
                "Compare",
                Type::function(
                    Type::bytes(),
                    Type::function(
                        Type::bytes(),
                        Type::name(Some("Ordering"), "Ordering", vec![]),
                    ),
                ),
                |handle| Box::pin(bytes_compare(handle)),
            ),
        ],
    }
}

async fn bytes_builder(mut handle: Handle) {
    let mut buf = Vec::<u8>::new();
    loop {
        match handle.case().await.as_str() {
            "add" => {
                buf.extend(handle.receive().await.bytes().await.as_ref());
            }
            "build" => {
                handle.provide_bytes(Bytes::from(buf)).await;
                break;
            }
            _ => unreachable!(),
        }
    }
}

async fn bytes_parser(mut handle: Handle) {
    let remainder = handle.receive().await.bytes().await;
    provide_bytes_parser(handle, remainder).await;
}

async fn bytes_parser_from_reader(mut handle: Handle) {
    let reader = handle.receive().await;
    provide_bytes_parser(handle, ReaderRemainder::new(reader)).await;
}

async fn bytes_reader(mut handle: Handle) {
    let bytes = handle.receive().await.bytes().await;
    provide_bytes_reader_from_bytes(handle, bytes).await;
}

async fn bytes_empty_reader(mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok")).await;
                return handle.break_().await;
            }
            "read" => {
                handle.signal(literal!("ok")).await;
                handle.signal(literal!("end")).await;
                return handle.break_().await;
            }
            _ => unreachable!(),
        }
    }
}

async fn bytes_equals(mut handle: Handle) {
    let left = handle.receive().await.bytes().await;
    let right = handle.receive().await.bytes().await;
    if left == right {
        handle.signal(literal!("true")).await;
    } else {
        handle.signal(literal!("false")).await;
    }
    handle.break_().await;
}

async fn bytes_compare(mut handle: Handle) {
    let left = handle.receive().await.bytes().await;
    let right = handle.receive().await.bytes().await;
    match left.cmp(&right) {
        Ordering::Equal => handle.signal(literal!("equal")).await,
        Ordering::Greater => handle.signal(literal!("greater")).await,
        Ordering::Less => handle.signal(literal!("less")).await,
    }
    handle.break_().await;
}

async fn provide_bytes_reader_from_bytes(mut handle: Handle, bytes: Bytes) {
    let mut offset = 0usize;
    let len = bytes.len();
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok")).await;
                return handle.break_().await;
            }
            "read" => {
                if offset >= len {
                    handle.signal(literal!("ok")).await;
                    handle.signal(literal!("end")).await;
                    return handle.break_().await;
                }
                let chunk = bytes.slice(offset..len);
                offset = len;
                handle.signal(literal!("ok")).await;
                handle.signal(literal!("chunk")).await;
                handle.send().await.provide_bytes(chunk).await;
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
enum PipeMessage {
    Chunk(Bytes),
}

struct PipeReaderState {
    sender: Mutex<Option<mpsc::UnboundedSender<PipeMessage>>>,
    reader_closed: AtomicBool,
    result_ready: AtomicBool,
    error: Mutex<Option<Handle>>,
    notify: Notify,
}

impl PipeReaderState {
    fn new(sender: mpsc::UnboundedSender<PipeMessage>) -> Arc<Self> {
        Arc::new(Self {
            sender: Mutex::new(Some(sender)),
            reader_closed: AtomicBool::new(false),
            result_ready: AtomicBool::new(false),
            error: Mutex::new(None),
            notify: Notify::new(),
        })
    }

    fn take_sender(&self) -> Option<mpsc::UnboundedSender<PipeMessage>> {
        self.sender.lock().expect("lock failed").take()
    }

    fn reader_closed(&self) -> bool {
        self.reader_closed.load(AtomicOrdering::SeqCst)
    }

    fn mark_reader_closed(&self) {
        if !self.reader_closed.swap(true, AtomicOrdering::SeqCst) {
            self.take_sender();
        }
    }

    fn error_present(&self) -> bool {
        self.error.lock().expect("lock failed").is_some()
    }

    fn set_result_ok(&self) {
        self.result_ready.store(true, AtomicOrdering::SeqCst);
        self.notify.notify_waiters();
    }

    async fn set_result_err(&self, err: Handle) {
        // This tricky is necessary to avoid holding `guard` while awaiting.
        if let Some(err) = {
            let mut guard = self.error.lock().expect("lock failed");
            if guard.is_none() {
                *guard = Some(err);
                None
            } else {
                Some(err)
            }
        } {
            err.erase().await;
        }
        self.result_ready.store(true, AtomicOrdering::SeqCst);
        self.notify.notify_waiters();
        self.take_sender();
    }

    async fn wait_result(&self) {
        while !self.result_ready.load(AtomicOrdering::SeqCst) && !self.error_present() {
            self.notify.notified().await;
        }
    }

    fn take_error(&self) -> Option<Handle> {
        self.error.lock().expect("lock failed").take()
    }
}

async fn bytes_pipe_reader(mut handle: Handle) {
    let mut closure = handle.receive().await;

    let (tx, rx) = mpsc::unbounded::<PipeMessage>();
    let state = PipeReaderState::new(tx);

    let writer_state = Arc::clone(&state);
    closure.send().await.concurrently(move |handle| async move {
        provide_pipe_reader_writer(handle, writer_state).await;
    });

    let result_state = Arc::clone(&state);
    closure.concurrently(move |mut handle| async move {
        match handle.case().await.as_str() {
            "ok" => {
                result_state.set_result_ok();
                handle.break_().await;
            }
            "err" => {
                result_state.set_result_err(handle).await;
            }
            _ => unreachable!(),
        }
    });

    provide_pipe_reader_output(handle, rx, state).await;
}

async fn provide_pipe_reader_writer(mut handle: Handle, state: Arc<PipeReaderState>) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                if state.reader_closed() || state.error_present() {
                    handle.signal(literal!("err")).await;
                    return handle.break_().await;
                }
                state.take_sender();
                handle.signal(literal!("ok")).await;
                return handle.break_().await;
            }
            "flush" => {
                if state.reader_closed() || state.error_present() {
                    handle.signal(literal!("err")).await;
                    return handle.break_().await;
                }
                handle.signal(literal!("ok")).await;
            }
            "write" => {
                let bytes = handle.receive().await.bytes().await;
                if write_chunk(&state, bytes) {
                    handle.signal(literal!("ok")).await;
                    continue;
                }
                handle.signal(literal!("err")).await;
                return handle.break_().await;
            }
            _ => unreachable!(),
        }
    }
}

fn write_chunk(state: &PipeReaderState, bytes: Bytes) -> bool {
    if state.reader_closed() || state.error_present() {
        return false;
    }
    match state.sender.lock().expect("lock failed").as_ref() {
        Some(sender) => sender.unbounded_send(PipeMessage::Chunk(bytes)).is_ok(),
        None => false,
    }
}

async fn provide_pipe_reader_output(
    mut handle: Handle,
    mut rx: mpsc::UnboundedReceiver<PipeMessage>,
    state: Arc<PipeReaderState>,
) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                state.mark_reader_closed();
                state.wait_result().await;
                if let Some(err) = state.take_error() {
                    handle.signal(literal!("err")).await;
                    handle.link(err).await;
                    return;
                } else {
                    handle.signal(literal!("ok")).await;
                    return handle.break_().await;
                }
            }
            "read" => match rx.next().await {
                Some(PipeMessage::Chunk(bytes)) => {
                    handle.signal(literal!("ok")).await;
                    handle.signal(literal!("chunk")).await;
                    handle.send().await.provide_bytes(bytes).await;
                }
                None => {
                    state.mark_reader_closed();
                    state.wait_result().await;
                    if let Some(err) = state.take_error() {
                        handle.signal(literal!("err")).await;
                        handle.link(err).await;
                        return;
                    } else {
                        handle.signal(literal!("ok")).await;
                        handle.signal(literal!("end")).await;
                        return handle.break_().await;
                    }
                }
            },
            _ => unreachable!(),
        }
    }
}

async fn bytes_length(mut handle: Handle) {
    let bytes = handle.receive().await.bytes().await;
    handle.provide_nat(BigInt::from(bytes.len())).await;
}

#[derive(Debug, Clone)]
pub(crate) enum BytesPattern {
    Nil,
    All,
    Empty,
    Min(BigInt),
    Max(BigInt),
    Bytes(Bytes),
    One(ByteClass),
    Non(ByteClass),
    Concat(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Repeat(Box<Self>),
    Repeat1(Box<Self>),
}

impl BytesPattern {
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
                handle.break_().await;
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
            "bytes" => {
                // .bytes Bytes
                let bs = handle.bytes().await;
                Box::new(Self::Bytes(bs))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct BytesMachine {
    pattern: Box<BytesPattern>,
    inner: MachineInner,
}

impl BytesMachine {
    pub(crate) fn start(pattern: Box<BytesPattern>) -> Self {
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
        let BytesPattern::Concat(_, p2) = self.pattern.as_ref() else {
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
    fn start(pattern: &BytesPattern, start: usize) -> Self {
        let state = match pattern {
            BytesPattern::Nil => State::Halt,

            BytesPattern::All => State::Init,

            BytesPattern::Empty => State::Init,

            BytesPattern::Min(_) => State::Index(0),
            BytesPattern::Max(_) => State::Index(0),

            BytesPattern::Bytes(_) => State::Index(0),

            BytesPattern::One(_) => State::Index(0),
            BytesPattern::Non(_) => State::Index(0),

            BytesPattern::Concat(p1, p2) => {
                let prefix = Self::start(p1, start);
                let suffixes = if prefix.accepts(p1) == Some(true) {
                    vec![Self::start(p2, start)]
                } else {
                    vec![]
                };
                State::Concat(Box::new(prefix), suffixes)
            }

            BytesPattern::And(p1, p2) | BytesPattern::Or(p1, p2) => State::Pair(
                Box::new(Self::start(p1, start)),
                Box::new(Self::start(p2, start)),
            ),

            BytesPattern::Repeat(_) => State::Init,
            BytesPattern::Repeat1(p) => State::Heap(vec![Self::start(p, start)]),
        };

        Self { state, start }
    }

    fn accepts(&self, pattern: &BytesPattern) -> Option<bool> {
        match (pattern, &self.state) {
            (_, State::Halt) => None,

            (BytesPattern::All, State::Init) => Some(true),

            (BytesPattern::Empty, State::Init) => Some(true),

            (BytesPattern::Min(n), State::Index(i)) => Some(&BigInt::from(*i) >= n),
            (BytesPattern::Max(n), State::Index(i)) => Some(&BigInt::from(*i) <= n),

            (BytesPattern::Bytes(s), State::Index(i)) => Some(s.len() == *i),

            (BytesPattern::One(_), State::Index(i)) => Some(*i == 1),
            (BytesPattern::Non(_), State::Index(i)) => Some(*i == 1),

            (BytesPattern::Concat(p1, p2), State::Concat(m1, heap)) => heap
                .iter()
                .filter_map(|m2| m2.accepts(p2))
                .max()
                .or_else(|| m1.accepts(p1).map(|_| false)),

            (BytesPattern::And(p1, p2), State::Pair(m1, m2)) => {
                match (m1.accepts(p1), m2.accepts(p2)) {
                    (Some(a1), Some(a2)) => Some(a1 && a2),
                    (None, _) | (_, None) => None,
                }
            }

            (BytesPattern::Or(p1, p2), State::Pair(m1, m2)) => {
                match (m1.accepts(p1), m2.accepts(p2)) {
                    (Some(a1), Some(a2)) => Some(a1 || a2),
                    (None, a) | (a, None) => a,
                }
            }

            (BytesPattern::Repeat(_), State::Init) => Some(true),
            (BytesPattern::Repeat(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (BytesPattern::Repeat1(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (p, s) => unreachable!("invalid combination of pattern {:?} and state {:?}", p, s),
        }
    }

    fn advance(&mut self, pattern: &BytesPattern, pos: usize, b: u8) {
        match (pattern, &mut self.state) {
            (_, State::Halt) => {}

            (BytesPattern::All, State::Init) => {}

            (BytesPattern::Empty, State::Init) => self.state = State::Halt,

            (BytesPattern::Min(_), State::Index(i)) => *i += 1,
            (BytesPattern::Max(n), State::Index(i)) => {
                if &BigInt::from(*i) < n {
                    *i += 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (BytesPattern::Bytes(s), State::Index(i)) => {
                if s.get(*i) == Some(&b) {
                    *i += 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (BytesPattern::One(class), State::Index(i)) => {
                if *i == 0 && class.contains(b) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }
            (BytesPattern::Non(class), State::Index(i)) => {
                if *i == 0 && !class.contains(b) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (BytesPattern::Concat(p1, p2), State::Concat(m1, heap)) => {
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

            (BytesPattern::And(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, b);
                m2.advance(p2, pos, b);
                if m1.state == State::Halt || m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (BytesPattern::Or(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, b);
                m2.advance(p2, pos, b);
                if m1.state == State::Halt && m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (BytesPattern::Repeat(p), State::Init) => {
                let mut m = Self::start(p, pos);
                m.advance(p, pos, b);
                self.state = State::Heap(vec![m])
            }
            (BytesPattern::Repeat(p) | BytesPattern::Repeat1(p), State::Heap(heap)) => {
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
