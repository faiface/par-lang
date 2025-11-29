use crate::runtime::new::runtime::{Runtime, Value, External, NonlinearUdata, Global, PackagePtr};
use std::sync::Arc;
use crate::TokioSpawn;
use tokio::sync::oneshot;
use futures::task::{Spawn, FutureObj, SpawnExt};
use tokio::sync::mpsc;
use super::readback::Handle;

pub enum ReducerMessage {
    Redex(Value, Value),
    Spawn(FutureObj<'static, ()>),
    Instantiate(PackagePtr, oneshot::Sender<(Value, Value)>),
}
pub struct NetHandle(pub mpsc::UnboundedSender<ReducerMessage>);

pub struct Reducer {
    pub runtime: Runtime,
    spawner: Arc<dyn Spawn + Send + Sync>,
    inbox: mpsc::UnboundedReceiver<ReducerMessage>,
    inbox_tx:  mpsc::UnboundedSender<ReducerMessage>,
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
                self.runtime.redexes.push((a, b));
            },
            ReducerMessage::Spawn(s) => {
                self.spawner.spawn_obj(s);
            },
            ReducerMessage::Instantiate(package, ret) => {
                ret.send(self.runtime.instantiate(package)).unwrap();
            }
        }
    }
    pub async fn run(&mut self) {
        loop {
            loop {
                if let Some((a, b)) = self.runtime.reduce() {
                    match (a, b) {
                        (External::Request(a), b) => {
                            a.send(b).unwrap();
                        }
                        (a, Value::External(External::Request(b))) => {
                            b.send(Value::External(a));
                        }
                        (External::NonlinearUdata(x), Value::Global(instance, Global::Fanout(out))) => {
                            for i in self.runtime.arena.get(out) {
                                self.runtime.redexes.push((Value::External(External::NonlinearUdata(x.clone())), Value::Global(instance.clone(), i.clone())));
                            }
                        },
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
    }
}
