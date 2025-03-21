use eframe::egui::{self, Color32, RichText, Ui};
use futures::{
    channel::oneshot::{channel, Sender},
    future::{join, BoxFuture},
    task::{Spawn, SpawnExt},
    FutureExt,
};
use indexmap::IndexSet;

use crate::{
    icombs::{net::number_to_string, Net},
    par::types::TypeDefs,
    playground::CheckedProgram,
};

pub trait NameRequiredTraits:
    Clone + Hash + Eq + core::fmt::Debug + Ord + Display + Send + Sync + 'static
{
}

impl<T: Clone + Hash + Eq + core::fmt::Debug + Ord + Display + Send + Sync + 'static>
    NameRequiredTraits for T
{
}

use crate::{
    icombs::{
        compiler::TypedTree,
        readback::{ReadbackResult, SharedState},
        Tree,
    },
    par::{
        parse::{Loc, Program},
        process::Expression,
        types::Type,
    },
};

use core::fmt::{Debug, Display};
use std::{
    collections::BTreeSet,
    hash::Hash,
    sync::{Arc, Mutex},
};

use crate::icombs::Name;

type Prog = Arc<Program<Loc, Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>>;
#[derive(Clone)]
pub struct ReadbackStateInner {
    pub shared: SharedState,
    pub spawner: Arc<dyn Spawn + Send + Sync>,
    pub type_variables: IndexSet<Name>,
}

pub struct Handle {
    refresh: Arc<dyn Fn() + Send + Sync>,
    history: Vec<Event>,
    end: Option<Request>,
    net: Arc<Mutex<Net>>,
}

impl Handle {
    fn child(&self) -> Self {
        Self {
            refresh: self.refresh.clone(),
            history: vec![],
            end: None,
            net: self.net.clone(),
        }
    }
    fn add_event(&mut self, ev: Event) {
        self.history.push(ev);
        (self.refresh)()
    }
    fn set_end(&mut self, end: Option<Request>) {
        self.end = end;
        (self.refresh)()
    }
}

#[derive(Debug)]
enum Request {
    Waiting,
    Port(TypedTree),
    Expand(TypedTree),
    Variable(usize),
    Choose(Tree, Vec<(Name, Tree, TypedTree)>),
}

#[derive(Debug)]
pub enum Event {
    Send(Arc<Mutex<Handle>>),
    Receive(Arc<Mutex<Handle>>),
    SendType(Name),
    ReceiveType(Name),
    Break,
    Continue,
    Either(Name),
    Choose(Name),
    Named(TypedTree),
}

impl Debug for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handle").finish_non_exhaustive()
    }
}

pub struct ReadbackState {
    pub root: Arc<Mutex<Handle>>,
    pub inner: ReadbackStateInner,
    // it's OK that this is unused, because we only use it to drop it when we're dropped.
    #[allow(unused)]
    pub net_tx: Sender<()>,
    pub root_impl: ReadbackImplLevel,
    pub show_net: bool,
}

impl ReadbackState {
    pub fn show_readback(&mut self, ui: &mut egui::Ui, prog: Arc<CheckedProgram>) {
        ui.vertical(|ui| {
            self.root_impl.show_message(ui);
            ui.horizontal(|ui| {
                ui.checkbox(&mut self.show_net, "Show net");
            });
            ui.horizontal(|ui| {
                self.inner.show_handle(ui, self.root.clone(), prog);
                if self.show_net {
                    ui.code(self.root.lock().unwrap().net.lock().unwrap().show());
                };
            });
        });
    }
    pub fn initialize(
        ui: &mut egui::Ui,
        net: Net,
        tree: TypedTree,
        spawner: Arc<dyn Spawn + Send + Sync>,
        program: &Arc<CheckedProgram>,
    ) -> Self {
        let root_impl = ReadbackImplLevel::from_type(&tree.ty, &program, &mut Default::default());
        let ctx = ui.ctx().clone();
        let handle = Handle {
            refresh: Arc::new(move || {
                ctx.request_repaint();
            }),
            history: vec![],
            end: Some(Request::Port(tree)),
            net: Arc::new(Mutex::new(net)),
        };
        let (net_tx, net_rx) = channel();
        let shared = SharedState::with_net(handle.net.clone(), program.type_defs.clone());
        // the net_tx channel gets dropped when the readback state is dropped.
        spawner.spawn(shared.create_net_reducer(net_rx)).unwrap();
        Self {
            root: Arc::new(Mutex::new(handle)),
            net_tx: net_tx,
            root_impl: root_impl,
            show_net: false,
            inner: ReadbackStateInner {
                shared,
                spawner: spawner,
                type_variables: IndexSet::new(),
            },
        }
    }
}
// first, we render as deep as we can. Button clicks replace nodes by a Halt node
// then, we read back, replacing Halt nodes with other nodes. This is the async part
impl ReadbackStateInner {
    pub fn show_handle(
        &mut self,
        ui: &mut egui::Ui,
        handle: Arc<Mutex<Handle>>,
        prog: Arc<CheckedProgram>,
    ) {
        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4))
            .outer_margin(egui::Margin::same(2))
            .show(ui, |ui| {
                ui.vertical(|ui| {
                    for event in handle.lock().unwrap().history.iter_mut() {
                        let prog = prog.clone();
                        match event {
                            Event::Send(handle) => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    self.show_handle(ui, handle.clone(), prog);
                                });
                            }
                            Event::Receive(handle) => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    self.show_handle(ui, handle.clone(), prog);
                                });
                            }
                            Event::SendType(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    ui.label(format!("type {}", name));
                                });
                            }
                            Event::ReceiveType(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    ui.label(format!("type {}", name));
                                });
                            }
                            Event::Either(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    ui.label(
                                        RichText::from(name.to_string())
                                            .color(Color32::WHITE)
                                            .strong(),
                                    );
                                });
                            }
                            Event::Choose(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    ui.label(
                                        RichText::from(name.to_string()).color(Color32::WHITE),
                                    );
                                });
                            }
                            Event::Named(tree) => {
                                let ty = &tree.ty;
                                let tree = &tree.tree;
                                if let Type::Name(_, name, _) = ty {
                                    ui.label(format!("name {}", name));
                                }
                            }
                            Event::Break => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    ui.label(RichText::from("!").color(Color32::WHITE).strong());
                                });
                            }
                            Event::Continue => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    ui.label(RichText::from("!").color(Color32::WHITE));
                                });
                            }
                        }
                    }
                    let mut lock = handle.lock().unwrap();
                    if let Some(end) = lock.end.as_mut() {
                        match end {
                            port @ Request::Port(..) => {
                                let Request::Port(mut port) =
                                    core::mem::replace(port, Request::Waiting)
                                else {
                                    unreachable!()
                                };

                                let shared = self.shared.clone();
                                port.ty = prepare_type_for_readback(
                                    self.shared.type_defs(),
                                    port.ty,
                                    &self.type_variables,
                                );
                                {
                                    let handle = handle.clone();
                                    let mut this = self.clone();
                                    self.spawner
                                        .spawn(async move {
                                            let port = shared.read_with_type(port).await;
                                            this.readback_result(handle, port, prog).await;
                                        })
                                        .unwrap();
                                }
                                (lock.refresh)()
                            }
                            Request::Waiting => {
                                ui.label(RichText::from("waiting").italics());
                            }
                            Request::Choose(ctx, options) => {
                                let mut chosen = None;
                                ui.vertical(|ui| {
                                    for (idx, (name, _, _)) in options.iter().enumerate() {
                                        if ui
                                            .button(
                                                RichText::new(name.to_string())
                                                    .color(Color32::WHITE),
                                            )
                                            .clicked()
                                        {
                                            chosen = Some(name.clone());
                                        }
                                    }
                                });
                                if let Some(chosen) = chosen {
                                    let Some(Request::Choose(ctx, options)) =
                                        core::mem::replace(&mut lock.end, Some(Request::Waiting))
                                    else {
                                        unreachable!()
                                    };
                                    lock.add_event(Event::Choose(chosen.clone()));
                                    let payload = self.shared.choose_choice(
                                        chosen,
                                        ctx,
                                        options,
                                        &self.spawner,
                                    );
                                    let mut this = self.clone();
                                    let handle = handle.clone();
                                    self.spawner
                                        .spawn(async move {
                                            this.readback_result(
                                                handle,
                                                ReadbackResult::Halted(payload),
                                                prog,
                                            )
                                            .await;
                                        })
                                        .unwrap();
                                }
                            }
                            Request::Expand(_) => {
                                if ui
                                    .button(
                                        RichText::from("expand").italics().color(Color32::WHITE),
                                    )
                                    .clicked()
                                {
                                    let Some(Request::Expand(package)) =
                                        core::mem::replace(&mut lock.end, Some(Request::Waiting))
                                    else {
                                        unreachable!()
                                    };
                                    let handle = handle.clone();
                                    let this = self.clone();
                                    self.spawner
                                        .spawn(async move {
                                            let tree = this.shared.expand_once(package.tree).await;
                                            let mut lock = handle.lock().unwrap();
                                            lock.end =
                                                Some(Request::Port(tree.with_type(package.ty)));
                                        })
                                        .unwrap();
                                }
                            }
                            Request::Variable(id) => {
                                ui.horizontal(|ui| {
                                    ui.label("Variable: ");
                                    ui.label(
                                        RichText::new(number_to_string(*id))
                                            .color(Color32::WHITE)
                                            .code(),
                                    );
                                });
                            }
                        }
                    }
                });
            });
    }
    pub fn readback_result(
        &mut self,
        handle: Arc<Mutex<Handle>>,
        port: ReadbackResult,
        prog: Arc<CheckedProgram>,
    ) -> BoxFuture<()> {
        async move {
            let handle_2 = handle.clone();
            match port {
                ReadbackResult::Break => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.add_event(Event::Break);
                    lock.end = None;
                }
                ReadbackResult::Continue => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.add_event(Event::Continue);
                    lock.end = None;
                }
                ReadbackResult::Send(lft, rgt) => {
                    let child = {
                        let mut lock = handle_2.lock().unwrap();
                        let child = Arc::new(Mutex::new(lock.child()));
                        lock.add_event(Event::Send(child.clone()));
                        child
                    };
                    let mut this = self.clone();
                    join(
                        self.readback_result(child, *lft, prog.clone()),
                        this.readback_result(handle, *rgt, prog),
                    )
                    .await;
                }
                ReadbackResult::Receive(lft, rgt) => {
                    let child = {
                        let mut lock = handle_2.lock().unwrap();
                        let child = Arc::new(Mutex::new(lock.child()));
                        lock.add_event(Event::Receive(child.clone()));
                        child
                    };
                    let mut this = self.clone();
                    join(
                        self.readback_result(child, *lft, prog.clone()),
                        this.readback_result(handle, *rgt, prog),
                    )
                    .await;
                }
                ReadbackResult::Either(name, payload) => {
                    {
                        let mut lock = handle_2.lock().unwrap();
                        lock.add_event(Event::Either(name));
                    }
                    self.readback_result(handle, *payload, prog).await;
                }
                ReadbackResult::Choice(ctx_in, options) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Choose(ctx_in, options)));
                }
                ReadbackResult::Expand(package) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Expand(package)));
                }
                ReadbackResult::Halted(mut tree) => {
                    let mut this = self.clone();
                    self.spawner
                        .spawn(async move {
                            tree.ty = prepare_type_for_readback(
                                &this.shared.type_defs(),
                                tree.ty,
                                &this.type_variables,
                            );
                            let res = this.shared.read_with_type(tree).await;
                            this.readback_result(handle, res, prog).await;
                        })
                        .unwrap();
                }
                ReadbackResult::Variable(id) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Variable(id)));
                }
                ReadbackResult::SendType(name, payload) => {
                    {
                        let mut lock = handle_2.lock().unwrap();
                        lock.add_event(Event::SendType(name.clone()));
                    }
                    self.type_variables.insert(name);
                    self.readback_result(handle, *payload, prog).await;
                }
                ReadbackResult::ReceiveType(name, payload) => {
                    {
                        let mut lock = handle_2.lock().unwrap();
                        lock.add_event(Event::ReceiveType(name.clone()));
                    }
                    self.type_variables.insert(name);
                    self.readback_result(handle, *payload, prog).await;
                }
                ReadbackResult::Named(tree) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.add_event(Event::Named(tree));
                    lock.set_end(None);
                }
                e => {
                    eprintln!("Don't know how to read back: {e:?}");
                }
            }
        }
        .boxed()
    }
}
pub fn prepare_type_for_readback(
    type_defs: &TypeDefs<Loc, Name>,
    mut ty: Type<Loc, Name>,
    type_variables: &IndexSet<Name>,
) -> Type<Loc, Name> {
    loop {
        ty = match ty {
            Type::Name(loc, name, args) => {
                if type_variables.contains(&name) {
                    break Type::Name(loc, name, args);
                } else {
                    type_defs.get(&loc, &name, &args).unwrap()
                }
            }
            Type::Recursive(_, asc, label, body) => {
                Type::expand_recursive(&asc, &label, &*body, &type_defs).unwrap()
            }
            Type::Iterative(_, asc, label, body) => {
                Type::expand_iterative(&asc, &label, &*body, &type_defs).unwrap()
            }
            ty => break ty,
        };
    }
}

enum ReadbackImplLevel {
    Incomplete,
    QuantifiedTypes,
    Ok,
}

impl core::ops::BitAnd<ReadbackImplLevel> for ReadbackImplLevel {
    type Output = ReadbackImplLevel;

    fn bitand(self, rhs: ReadbackImplLevel) -> Self::Output {
        use ReadbackImplLevel::*;
        match (self, rhs) {
            (Incomplete, _) | (_, Incomplete) => Incomplete,
            (QuantifiedTypes, _) | (_, QuantifiedTypes) => QuantifiedTypes,
            _ => Ok,
        }
    }
}

impl ReadbackImplLevel {
    fn from_type(
        typ: &Type<Loc, Name>,
        prog: &CheckedProgram,
        type_variables: &mut BTreeSet<Name>,
    ) -> Self {
        use core::ops::BitAnd;
        use ReadbackImplLevel::*;
        match typ {
            Type::Chan(loc, body) => ReadbackImplLevel::from_type(&body, prog, type_variables),
            Type::Name(loc, name, items) => {
                if !type_variables.contains(name) {
                    ReadbackImplLevel::from_type(
                        &prog.type_defs.get(&loc, name, &items).unwrap(),
                        prog,
                        type_variables,
                    )
                } else {
                    ReadbackImplLevel::QuantifiedTypes
                }
            }
            Type::Send(_, a, b) | Type::Receive(_, a, b) => {
                ReadbackImplLevel::from_type(a, prog, type_variables)
                    & ReadbackImplLevel::from_type(b, prog, type_variables)
            }
            Type::Either(_, index_map) | Type::Choice(_, index_map) => index_map
                .values()
                .map(|x| ReadbackImplLevel::from_type(x, prog, type_variables))
                .fold(Ok, ReadbackImplLevel::bitand),
            Type::Break(_) | Type::Continue(_) => Ok,
            Type::Recursive(_, _, _, body) => {
                ReadbackImplLevel::from_type(body, prog, type_variables)
            }
            Type::Iterative(_, _, _, body) => {
                ReadbackImplLevel::from_type(body, prog, type_variables)
            }
            Type::ReceiveType(_, name, body) | Type::SendType(_, name, body) => {
                let inserted = type_variables.insert(name.clone());
                let res =
                    ReadbackImplLevel::from_type(body, prog, type_variables) & QuantifiedTypes;
                if inserted {
                    type_variables.remove(name);
                };
                res
            }
            Type::Self_(_, _) => Ok,
            _ => unreachable!("Type not implemented: {typ:?}"),
        }
    }
    fn show_message(&self, ui: &mut Ui) {
        match self {
            ReadbackImplLevel::Incomplete => {
                ui.label(RichText::from("This type uses type constructors whose readback has not been implemented yet").color(Color32::RED));
            }
            ReadbackImplLevel::QuantifiedTypes => {
                ui.label(RichText::from("This type uses quantified types. You will be able to see most of the data structure, but you won't be able to operate on quantified-over types").color(Color32::YELLOW));
            }
            ReadbackImplLevel::Ok => {}
        }
    }
}
