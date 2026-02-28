use super::readback::Handle;
use crate::runtime_impl::flat::runtime::{Linear, Node, Runtime, UserData};
use crate::spawn::TokioSpawn;
use futures::future::RemoteHandle;
use futures::task::{FutureObj, Spawn, SpawnExt};
use std::sync::Arc;
use std::sync::atomic::AtomicUsize;
use std::time::Instant;
use tokio::sync::mpsc;

pub enum ReducerMessage {
    Redex(Node, Node),
    Spawn(FutureObj<'static, ()>),
    Dropped(usize),
    Created(usize),
}

pub struct NetHandle(
    pub mpsc::UnboundedSender<ReducerMessage>,
    pub usize,
    pub Arc<AtomicUsize>,
);

impl Clone for NetHandle {
    fn clone(&self) -> Self {
        let new = Self(
            self.0.clone(),
            self.2.fetch_add(1, std::sync::atomic::Ordering::AcqRel),
            self.2.clone(),
        );
        new.0.send(ReducerMessage::Created(new.1)).unwrap();
        new
    }
}

impl Drop for NetHandle {
    fn drop(&mut self) {
        let _ = self.0.send(ReducerMessage::Dropped(self.1));
    }
}

pub(crate) struct Reducer {
    pub runtime: Runtime,
    spawner: Arc<dyn Spawn + Send + Sync>,
    inbox: mpsc::UnboundedReceiver<ReducerMessage>,
    sender: mpsc::WeakUnboundedSender<ReducerMessage>,
    num_handles: Arc<AtomicUsize>,
}

impl Reducer {
    pub(crate) fn from(runtime: Runtime) -> (Self, NetHandle) {
        let (tx, rx) = mpsc::unbounded_channel();
        let num_handles = Arc::new(AtomicUsize::new(0));
        (
            Self {
                runtime,
                spawner: Arc::new(TokioSpawn::new()),
                inbox: rx,
                sender: tx.downgrade(),
                num_handles: num_handles.clone(),
            },
            NetHandle(tx, 0, num_handles),
        )
    }
    // this function should only be called inside run, to avoid race conditions
    fn net_handle(&mut self) -> NetHandle {
        if let Some(sender) = self.sender.upgrade() {
            NetHandle(
                sender,
                self.num_handles
                    .fetch_add(1, std::sync::atomic::Ordering::AcqRel),
                self.num_handles.clone(),
            )
        } else {
            // all senders have been dropped, so we can just create a new one channel
            let (tx, rx) = mpsc::unbounded_channel();
            self.inbox = rx;
            self.sender = tx.downgrade();
            NetHandle(tx, 0, self.num_handles.clone())
        }
    }
    fn handle_message(&mut self, msg: ReducerMessage) {
        match msg {
            ReducerMessage::Redex(a, b) => {
                self.runtime.redexes.push((a, b));
            }
            ReducerMessage::Spawn(s) => {
                self.spawner.spawn_obj(s).unwrap();
            }
            ReducerMessage::Dropped(_) => {}
            ReducerMessage::Created(_) => {}
        }
    }
    pub(crate) async fn run(&mut self) {
        loop {
            loop {
                if !self.runtime.redexes.is_empty() {
                    let start = Instant::now();
                    if let Some((a, b)) = self.runtime.reduce() {
                        match (a, b) {
                            (UserData::Request(a), b) => {
                                a.send(b).unwrap();
                            }
                            (a, Node::Linear(Linear::Request(b))) => {
                                b.send(Node::Linear(a.into())).unwrap();
                            }
                            (UserData::ExternalFn(f), other) => {
                                let handle = Handle::from_node(
                                    self.runtime.arena.clone(),
                                    self.net_handle(),
                                    other,
                                );
                                self.spawner.spawn(f(handle.into())).unwrap();
                            }
                            (UserData::ExternalArc(f), other) => {
                                let handle = Handle::from_node(
                                    self.runtime.arena.clone(),
                                    self.net_handle(),
                                    other,
                                );
                                self.spawner.spawn((f.0).as_ref()(handle.into())).unwrap();
                            }
                        }
                    }
                    self.runtime.rewrites.net_duration += start.elapsed();
                } else {
                    match self.inbox.try_recv() {
                        Ok(msg) => {
                            self.handle_message(msg);
                        }
                        _ => {
                            break;
                        }
                    }
                }
            }
            match self.inbox.recv().await {
                Some(msg) => {
                    self.handle_message(msg);
                }
                _ => {
                    break;
                }
            }
        }
    }
    pub(crate) fn spawn_reducer(mut self) -> RemoteHandle<Self> {
        self.spawner
            .clone()
            .spawn_with_handle(async move {
                self.run().await;
                self
            })
            .unwrap()
    }
    pub(crate) fn spawner(&self) -> Arc<dyn Spawn + Send + Sync> {
        self.spawner.clone()
    }
}
