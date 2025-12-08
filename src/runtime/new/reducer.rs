use super::readback::Handle;
use crate::runtime::new::runtime::{Global, Linear, Node, PackagePtr, Runtime, UserData};
use crate::TokioSpawn;
use futures::future::RemoteHandle;
use futures::task::{FutureObj, Spawn, SpawnExt};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

pub enum ReducerMessage {
    Redex(Node, Node),
    Spawn(FutureObj<'static, ()>),
    Instantiate(PackagePtr, oneshot::Sender<(Node, Node)>),
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
        let mut new = Self(
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
        self.0.send(ReducerMessage::Dropped(self.1)).unwrap();
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
                println!(
                    "Handle -> Reducer: {:?} ~ {:?}",
                    a.variant_tree_name(),
                    b.variant_tree_name()
                );
                self.runtime.redexes.push((a, b));
            }
            ReducerMessage::Spawn(s) => {
                println!("Handle -> Reducer: Spawn");
                self.spawner.spawn_obj(s).unwrap();
            }
            ReducerMessage::Instantiate(package, ret) => {
                println!("Handle -> Reducer: Instantiate");
                use crate::runtime::new::runtime::Linker;
                ret.send(self.runtime.instantiate(package)).unwrap();
            }
            ReducerMessage::Dropped(id) => {
                println!("Handle -> Reducer: Dropped {}", id);
            }
            ReducerMessage::Created(id) => {
                println!("Handle -> Reducer: Created {}", id);
            }
        }
    }
    pub async fn run(&mut self) {
        loop {
            loop {
                println!("Reducer State: Reducing");
                if let Some((a, b)) = self.runtime.reduce() {
                    match (a, b) {
                        (UserData::Request(a), b) => {
                            println!("Runtime -> Reducer: Fulfill request");
                            a.send(b).unwrap();
                        }
                        (a, Node::Linear(Linear::Request(b))) => {
                            println!("Runtime -> Reducer: Fulfill request");
                            b.send(Node::Linear(a.into())).unwrap();
                        }
                        (UserData::Primitive(p), Node::Global(instance, Global::Fanout(out))) => {
                            for i in self.runtime.arena.get(out) {
                                self.runtime.redexes.push((
                                    Node::Linear(UserData::Primitive(p.clone()).into()),
                                    Node::Global(instance.clone(), i.clone()),
                                ));
                            }
                        }
                        (UserData::ExternalFn(f), other) => {
                            println!("Runtime -> Reducer: Run ExternalFn");
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
                            println!("Runtime -> Reducer: Run ExternalArc");
                            let handle = Handle::from_node(
                                self.runtime.arena.clone(),
                                self.net_handle(),
                                other,
                            );
                            self.spawner
                                .spawn((f.0).as_ref()(crate::runtime::Handle::New(handle)))
                                .unwrap();
                        }
                        _ => todo!(),
                    }
                } else if let Ok(msg) = self.inbox.try_recv() {
                    self.handle_message(msg);
                } else {
                    break;
                }
            }
            println!("Reducer State: Waiting for a message");
            if self.inbox.sender_strong_count() == 1 {
                break;
            }
            if let Some(msg) = self.inbox.recv().await {
                self.handle_message(msg);
            } else {
                break;
            }
        }
        println!("Reducer exited cleanly");
    }
    pub fn spawn_reducer(mut self) -> RemoteHandle<()> {
        self.spawner
            .clone()
            .spawn_with_handle(async move {
                self.run().await;
            })
            .unwrap()
    }
}
