use futures::{
    channel::oneshot::{channel, Receiver, Sender},
    executor::LocalPool,
    future::{Either, LocalBoxFuture},
    task::{LocalSpawnExt, SpawnExt},
    FutureExt, SinkExt, StreamExt,
};
use std::{
    any::Any,
    fmt::Debug,
    future::{poll_fn, Future},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
    task::{Context, RawWakerVTable, Waker},
};

use crate::par::{
    parse::{Loc, Name},
    types::Type,
};

use super::{compiler::TypedTree, Tree};

#[derive(Default, Debug)]
pub struct CoroState {
    executor: Mutex<futures::executor::LocalPool>,
    pub(crate) net: Mutex<Option<super::net::Net>>,
    new_redex: Mutex<Vec<futures::channel::mpsc::Sender<()>>>,
    new_var: AtomicUsize,
}

struct WaitingReadbackExt(Tree);
struct ReadbackExt(Sender<PortContents>);

#[derive(Default, Clone, Debug)]
pub struct SharedState {
    pub(crate) shared: Arc<CoroState>,
}

#[derive(Debug)]
pub enum PortContents {
    Aux(Sender<PortContents>),
    AuxLazy(Sender<PortContents>),
    AuxLazyPicked(usize),

    // Waiting :)
    Waiting(Tree),
    // A tree, which should be readback further, if necessary
    Tree(Tree),
}

impl SharedState {
    pub fn make_oneshot_ext_from_tx(&self, tx: Sender<PortContents>) -> Tree {
        let shared = self.shared.clone();
        Tree::ext(
            move |net, tree, ext| {
                let ext: Box<ReadbackExt> = ext.downcast().unwrap();
                let tx = ext.0;
                match tree {
                    Err(ext) => {
                        if ext.is::<WaitingReadbackExt>() {
                            let ext: Box<WaitingReadbackExt> = ext.downcast().unwrap();
                            tx.send(PortContents::Waiting(ext.0)).unwrap();
                        } else if ext.is::<ReadbackExt>() {
                            tx.send(*ext.downcast().unwrap()).unwrap();
                        } else {
                            unreachable!()
                        }
                    }
                    Ok(Tree::Ext(f, ext)) => {
                        if ext.is::<WaitingReadbackExt>() {
                            let ext: Box<WaitingReadbackExt> = ext.downcast().unwrap();
                            tx.send(PortContents::Waiting(ext.0)).unwrap();
                        } else if ext.is::<ReadbackExt>() {
                            f(net, Err(Box::new(PortContents::Aux(tx))), ext);
                        } else {
                            unreachable!()
                        }
                    }
                    Ok(tree) => {
                        tx.send(PortContents::Tree(tree)).unwrap();
                    }
                }
            },
            ReadbackExt(tx),
        )
    }
    pub fn make_oneshot_ext(&self) -> (Tree, Receiver<PortContents>) {
        let (tx, rx) = channel();
        let tree = self.make_oneshot_ext_from_tx(tx);
        (tree, rx)
    }
    // this is async because it will only work when ran under an async context.
    pub async fn add_redex(&self, a: Tree, b: Tree) {
        for i in self.shared.new_redex.lock().unwrap().iter_mut() {
            let _ = i.send(()).await;
        }
        self.shared.net.lock().unwrap().as_mut().unwrap().link(a, b)
    }
    pub async fn read_port(&self, tree: Tree) -> PortContents {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree).await;
        rx.await.unwrap()
    }
    pub async fn read_port_as_tree(&self, tree: Tree) -> Tree {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree).await;
        match rx.await.unwrap() {
            PortContents::Aux(sender) | PortContents::AuxLazy(sender) => {
                self.make_oneshot_ext_from_tx(sender)
            }
            PortContents::Tree(tree) => tree,
            _ => unreachable!(),
        }
    }
    pub async fn as_con(&self, tree: Tree) -> (Tree, Tree) {
        match tree {
            Tree::Con(a, b) => ((*a, *b)),
            other => {
                // eta expand
                let mut binding = self.shared.net.lock().unwrap();
                let net = binding.as_mut().unwrap();
                let (v0, v1) = net.create_wire();
                let (w0, w1) = net.create_wire();
                drop(binding);
                self.add_redex(other, Tree::c(v0, w0)).await;
                ((v1, w1))
            }
        }
    }
    pub async fn as_era(&self, tree: Tree) -> () {
        match tree {
            Tree::Era => (),
            other => todo!(),
        }
    }
    /// Eagerly read back a CON node.
    /// If we don't get a CON node, eta-expand and return the readback of that.
    pub async fn read_con(&self, tree: Tree) -> (Tree, Tree) {
        let tree = self.read_port_as_tree(tree).await;
        self.as_con(tree).await
    }
    pub async fn read_era(&self, tree: Tree) -> () {
        let tree = self.read_port_as_tree(tree).await;
        self.as_era(tree).await
    }
    pub async fn as_par(&self, tree: Tree) -> (Tree, Tree) {
        self.as_con(tree).await
    }
    pub async fn as_times(&self, tree: Tree) -> (Tree, Tree) {
        self.as_con(tree).await
    }
    pub fn flatten_multiplexed(&self, tree: Tree, len: usize) -> LocalBoxFuture<Vec<Tree>> {
        use futures::future::FutureExt;
        async move {
            if len == 0 {
                self.read_era(tree).await;
                vec![]
            } else if len == 1 {
                vec![tree]
            } else {
                let fst_len = len / 2;
                let snd_len = len - fst_len;
                let (fst, snd) = self.read_con(tree).await;
                [
                    self.flatten_multiplexed(fst, fst_len).await,
                    self.flatten_multiplexed(snd, snd_len).await,
                ]
                .concat()
            }
        }
        .boxed_local()
    }
    pub async fn as_either<Name>(&self, tree: Tree, variants: Vec<Name>) -> (Name, Tree) {
        // TODO: we might have to eta-reduce `context` here
        // because it's possible that it's link to the context variable in the variant is eta-expnded
        // A less brittle solution is to make this interact with a `choice`
        let (context, possibilities) = self.as_con(tree).await;
        let possibilities = self
            .flatten_multiplexed(possibilities, variants.len())
            .await;
        let mut name_payload = None;
        for (possible, name) in possibilities.into_iter().zip(variants) {
            if let Tree::Con(a, b) = self.read_port_as_tree(possible).await {
                name_payload = Some((name, *b));
            }
        }
        name_payload.expect("couldn't readback either")
    }
    pub async fn as_choice<Name>(
        &self,
        tree: Tree,
        variants: Vec<Name>,
    ) -> (Tree, Vec<(Name, Tree, Tree)>) {
        let (context, cases) = self.as_con(tree).await;
        let cases = self.flatten_multiplexed(cases, variants.len()).await;
        let mut res = vec![];
        for (case, name) in cases.into_iter().zip(variants) {
            let (ctx, payload) = self.read_con(case).await;
            res.push((name, ctx, payload));
        }
        (context, res)
    }
    pub async fn read_with_type<Name: Clone + Ord + std::hash::Hash + 'static + Debug>(
        &self,
        tree: TypedTree<Name>,
    ) -> ReadbackResult<Name> {
        let ty = tree.ty;
        let port = self.read_port(tree.tree).await;
        println!("Port: {port:?}");
        println!("Ty: {ty:?}");
        self.as_with_type(port, ty).await
    }
    pub fn as_with_type<Name: Clone + Ord + std::hash::Hash + 'static + Debug>(
        &self,
        port: PortContents,
        ty: Type<Loc, Name>,
    ) -> LocalBoxFuture<ReadbackResult<Name>> {
        async move {
            let tree = match port {
                PortContents::Aux(tx) => {
                    let (tx_, rx) = channel();
                    tx.send(PortContents::AuxLazy(tx_)).unwrap();
                    return self.as_with_type(rx.await.unwrap(), ty).await;
                }
                PortContents::AuxLazy(tx) => {
                    let var_id = self.shared.new_var.fetch_add(1, Ordering::AcqRel);
                    tx.send(PortContents::AuxLazyPicked(var_id)).unwrap();
                    return ReadbackResult::Variable(var_id);
                }
                PortContents::AuxLazyPicked(n) => {
                    return ReadbackResult::Variable(n);
                }
                PortContents::Tree(tree) => tree,
                PortContents::Waiting(tree) => return ReadbackResult::Waiting(tree.with_type(ty)),
            };
            match ty {
                crate::par::types::Type::Send(_, from, to) => {
                    let (a, b) = self.as_par(tree).await;
                    ReadbackResult::Send(
                        b.with_type(*from).into_readback_result_boxed(),
                        a.with_type(*to).into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Receive(_, from, to) => {
                    let (a, b) = self.as_par(tree).await;
                    ReadbackResult::Receive(
                        b.with_type((*from).dual()).into_readback_result_boxed(),
                        a.with_type(*to).into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Either(_, mut variants) => {
                    variants.sort_keys();
                    let (name, payload) = self
                        .as_either(tree, variants.keys().cloned().collect())
                        .await;
                    ReadbackResult::Either(
                        name.clone(),
                        payload
                            .with_type(variants.get(&name).unwrap().clone())
                            .into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Choice(_, mut variants) => {
                    variants.sort_keys();
                    let (ctx, cases) = self
                        .as_choice(tree, variants.keys().cloned().collect())
                        .await;
                    ReadbackResult::Choice(
                        ctx,
                        cases
                            .into_iter()
                            .map(|(name, b, c)| {
                                (
                                    name.clone(),
                                    b,
                                    c.with_type(variants.get(&name).unwrap().clone())
                                        .into_readback_result_boxed(),
                                )
                            })
                            .collect(),
                    )
                }

                crate::par::types::Type::Break(_) => {
                    self.read_era(tree).await;
                    ReadbackResult::Break
                }
                crate::par::types::Type::Continue(_) => {
                    self.read_era(tree).await;
                    ReadbackResult::Continue
                }
                ty => ReadbackResult::Unsupported(tree.with_type(ty)),
            }
        }
        .boxed_local()
    }
    pub fn create_variable_ext(&self, tree: Tree) -> Tree {
        use crate::icombs::net::Net;
        fn f(net: &mut Net, tree: Result<Tree, Box<dyn Any>>, ext: Box<dyn Any>) {
            match tree {
                Ok(Tree::Ext(f, data)) => f(net, Err(ext), data),
                Ok(a) => net.link(a, ext.downcast::<WaitingReadbackExt>().unwrap().0),
                Err(other_ext) => {
                    net.link(
                        ext.downcast::<WaitingReadbackExt>().unwrap().0,
                        other_ext.downcast::<WaitingReadbackExt>().unwrap().0,
                    );
                }
            }
        }
        Tree::ext(f, WaitingReadbackExt(tree))
    }
    pub fn spawn(&self, f: impl Future<Output = ()> + 'static) {
        use futures::task::SpawnExt;
        self.shared
            .executor
            .lock()
            .unwrap()
            .spawner()
            .spawn_local(f)
            .unwrap()
    }
    pub fn execute<T>(&self, net: &mut super::net::Net, f: impl Future<Output = T>) -> T {
        *self.shared.net.lock().unwrap() = Some(core::mem::take(net));
        let (tx, mut rx) = futures::channel::mpsc::channel(16);
        self.shared.new_redex.lock().unwrap().push(tx);
        let shared = self.shared.clone();

        let local_future = futures::future::select(
            Box::pin(f),
            Box::pin(async move {
                loop {
                    let mut binding = shared.net.lock().unwrap();
                    let mut net = binding.as_mut().unwrap();
                    while net.reduce_one() {}
                    drop(binding);
                    rx.next().await.unwrap();
                }
            }),
        );

        let local_result = self.shared.executor.lock().unwrap().run_until(local_future);
        let return_value = match local_result {
            Either::Left((a, _)) => a,
            Either::Right(((), left_fut)) => {
                self.shared.executor.lock().unwrap().run_until(left_fut)
            }
        };

        *net = self.shared.net.lock().unwrap().take().unwrap();
        return_value
    }
}

#[derive(Debug, Default)]
pub enum ReadbackResult<Name: Clone> {
    #[default]
    Break,
    Continue,
    Send(Box<ReadbackResult<Name>>, Box<ReadbackResult<Name>>),
    Receive(Box<ReadbackResult<Name>>, Box<ReadbackResult<Name>>),
    Either(Name, Box<ReadbackResult<Name>>),
    Choice(Tree, Vec<(Name, Tree, Box<ReadbackResult<Name>>)>),
    Expand(Box<ReadbackResult<Name>>),
    Halted(TypedTree<Name>),
    Unsupported(TypedTree<Name>),
    Waiting(TypedTree<Name>),
    Variable(usize),
}

impl<Name: Clone> TypedTree<Name> {
    fn into_readback_result(self) -> ReadbackResult<Name> {
        ReadbackResult::Halted(self)
    }
    fn into_readback_result_boxed(self) -> Box<ReadbackResult<Name>> {
        Box::new(self.into_readback_result())
    }
}
