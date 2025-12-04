use super::readback::Handle;
use crate::runtime::new::runtime::{
    ExternalFn, Global, Linear, Node, PackagePtr, Runtime, UserData,
};
use crate::TokioSpawn;
use futures::task::{FutureObj, Spawn, SpawnExt};
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

pub enum ReducerMessage {
    Redex(Node, Node),
    Spawn(FutureObj<'static, ()>),
    Instantiate(PackagePtr, oneshot::Sender<(Node, Node)>),
}

#[derive(Clone)]
pub struct NetHandle(pub mpsc::UnboundedSender<ReducerMessage>);

pub struct Reducer {
    pub runtime: Runtime,
    spawner: Arc<dyn Spawn + Send + Sync>,
    inbox: mpsc::UnboundedReceiver<ReducerMessage>,
    inbox_tx: mpsc::UnboundedSender<ReducerMessage>,
}

impl Reducer {
    pub fn from(runtime: Runtime) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        Self {
            runtime,
            spawner: Arc::new(TokioSpawn::new()),
            inbox: rx,
            inbox_tx: tx,
        }
    }
    pub fn net_handle(&mut self) -> NetHandle {
        NetHandle(self.inbox_tx.clone())
    }
    fn handle_message(&mut self, msg: ReducerMessage) {
        match msg {
            ReducerMessage::Redex(a, b) => {
                println!(
                    "Received redex {:?} ~ {:?}",
                    a.variant_tree_name(),
                    b.variant_tree_name()
                );
                self.runtime.redexes.push((a, b));
            }
            ReducerMessage::Spawn(s) => {
                self.spawner.spawn_obj(s).unwrap();
            }
            ReducerMessage::Instantiate(package, ret) => {
                use crate::runtime::new::runtime::Linker;
                ret.send(self.runtime.instantiate(package)).unwrap();
            }
        }
    }
    pub async fn run(&mut self) {
        loop {
            loop {
                if let Some((a, b)) = self.runtime.reduce() {
                    match (a, b) {
                        (UserData::Request(a), b) => {
                            println!("Send request");
                            a.send(b).unwrap();
                        }
                        (a, Node::Linear(Linear::Request(b))) => {
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
                        (UserData::External(f), other) => {
                            let handle = Handle::from_node(
                                self.runtime.arena.clone(),
                                self.net_handle(),
                                other,
                            );
                            self.spawner.spawn(f(handle)).unwrap();
                        }
                        _ => todo!(),
                    }
                } else if let Ok(msg) = self.inbox.try_recv() {
                    self.handle_message(msg);
                } else {
                    break;
                }
            }
            if let Some(msg) = self.inbox.recv().await {
                self.handle_message(msg);
            } else {
                break;
            }
        }
        println!("Finished mail loop");
    }
}
