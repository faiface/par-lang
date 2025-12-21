use super::readback::Handle;
use crate::runtime::new::runtime::{Linear, Node, Runtime, UserData};
use crate::runtime::new::stats::Rewrites;
use crate::TokioSpawn;
use futures::future::RemoteHandle;
use futures::task::{FutureObj, Spawn, SpawnExt};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
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

pub struct Reducer {
    pub runtime: Runtime,
    spawner: Arc<dyn Spawn + Send + Sync>,
    inbox: mpsc::UnboundedReceiver<ReducerMessage>,
    handle: NetHandle,
}

impl Reducer {
    pub fn from(runtime: Runtime) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        Self {
            runtime,
            spawner: Arc::new(TokioSpawn::new()),
            inbox: rx,
            handle: NetHandle(tx, 0, Arc::new(1.into())),
        }
    }
    pub fn net_handle(&mut self) -> NetHandle {
        self.handle.clone()
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
    pub async fn run(&mut self) {
        loop {
            loop {
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
                            self.spawner
                                .spawn(f(crate::runtime::Handle::New(handle)))
                                .unwrap();
                        }
                        (UserData::ExternalArc(f), other) => {
                            let handle = Handle::from_node(
                                self.runtime.arena.clone(),
                                self.net_handle(),
                                other,
                            );
                            self.spawner
                                .spawn((f.0).as_ref()(crate::runtime::Handle::New(handle)))
                                .unwrap();
                        }
                    }
                } else if let Ok(msg) = self.inbox.try_recv() {
                    self.handle_message(msg);
                } else {
                    break;
                }
            }
            if self.inbox.sender_strong_count() == 1 {
                break;
            }
            if let Some(msg) = self.inbox.recv().await {
                self.handle_message(msg);
            } else {
                break;
            }
        }
    }
    pub fn spawn_reducer(mut self) -> RemoteHandle<Self> {
        self.spawner
            .clone()
            .spawn_with_handle(async move {
                self.run().await;
                self
            })
            .unwrap()
    }
    pub fn stats(&self) -> Rewrites {
        self.runtime.rewrites.clone()
    }
    pub fn spawner(&self) -> Arc<dyn Spawn + Send + Sync> {
        self.spawner.clone()
    }
}
