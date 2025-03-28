use crate::{
    par::process::Expression,
    par::runtime::{self, Context, Message, Value},
};
use futures::{channel::oneshot, task::SpawnExt};
use std::{
    hash::Hash,
    sync::{Arc, Mutex},
};
use crate::location::Span;

pub struct Handle<Loc, Name, Typ> {
    refresh: Arc<dyn Fn() + Send + Sync>,
    events: Vec<Event<Loc, Name, Typ>>,
    interaction: Option<Result<Interaction<Loc, Name, Typ>, runtime::Error<Loc, Name>>>,
    cancelled: bool,
}

pub enum Event<Loc, Name, Typ> {
    Send(Loc, Arc<Mutex<Handle<Loc, Name, Typ>>>),
    Receive(Loc, Arc<Mutex<Handle<Loc, Name, Typ>>>),
    Choose(Loc, Name),
    Either(Loc, Name),
    Break(Loc),
    Continue(Loc),
}

struct Interaction<Loc, Name, Typ> {
    context: Context<Loc, Name, Typ>,
    value: Value<Loc, Name>,
    request: Request<Loc, Name>,
}

#[derive(Clone, Debug)]
pub enum Request<Loc, Name> {
    Dynamic(Loc),
    Either(Loc, Arc<[Name]>),
}

impl<Name, Typ> Handle<Span, Name, Typ>
where
    Name: Clone + Eq + Hash + Send + Sync + 'static,
    Typ: Send + Sync + 'static,
{
    pub fn events(&self) -> &[Event<Span, Name, Typ>] {
        &self.events
    }

    pub fn interaction(&self) -> Option<Result<Request<Span, Name>, runtime::Error<Span, Name>>> {
        match &self.interaction {
            Some(Ok(int)) => Some(Ok(int.request.clone())),
            Some(Err(error)) => Some(Err(error.clone())),
            None => None,
        }
    }

    pub fn choose(handle: Arc<Mutex<Self>>, loc: Span, chosen: Name) {
        if let Some(Ok(mut int)) = handle.lock().expect("lock failed").interaction.take() {
            int.context
                .spawner()
                .spawn({
                    let handle = Arc::clone(&handle);
                    async move {
                        match int
                            .context
                            .choose_in(Span::default(), int.value, chosen.clone())
                            .await
                        {
                            Ok(value) => {
                                handle
                                    .lock()
                                    .expect("lock failed")
                                    .add_event(Event::Either(loc, chosen));
                                Self::run(handle, int.context, value).await
                            }
                            Err(error) => {
                                let mut handle = handle.lock().expect("lock failed");
                                handle.interaction = Some(Err(error));
                                (handle.refresh)();
                            }
                        }
                    }
                })
                .expect("spawn failed");
        }
    }

    pub fn start_expression(
        refresh: Arc<dyn Fn() + Send + Sync>,
        context: Context<Span, Name, Typ>,
        expression: &Expression<Name, Typ>,
    ) -> Arc<Mutex<Self>> {
        let mut context = context;
        match context.evaluate(expression) {
            Ok(value) => Self::start(refresh, context, value),
            Err(error) => Arc::new(Mutex::new(Self {
                refresh,
                events: Vec::new(),
                interaction: Some(Err(error)),
                cancelled: false,
            })),
        }
    }

    pub fn start(
        refresh: Arc<dyn Fn() + Send + Sync>,
        context: Context<Span, Name, Typ>,
        value: Value<Span, Name>,
    ) -> Arc<Mutex<Self>> {
        let handle = Arc::new(Mutex::new(Self {
            refresh,
            events: Vec::new(),
            interaction: None,
            cancelled: false,
        }));

        context
            .spawner()
            .spawn(Self::run(Arc::clone(&handle), context, value))
            .expect("spawn failed");

        handle
    }

    async fn run(
        handle: Arc<Mutex<Self>>,
        mut context: Context<Span, Name, Typ>,
        mut value: Value<Span, Name>,
    ) {
        let mut consecutive_dynamic: usize = 0;

        loop {
            let previous_consecutive = consecutive_dynamic;
            consecutive_dynamic = 0;

            match value {
                Value::Receiver(rx) => {
                    let message = rx.await.ok().expect("sender dropped");
                    let mut handle = handle.lock().expect("lock failed");

                    match message {
                        Message::Swap(runtime::Request::Dynamic(loc), tx) => {
                            if previous_consecutive > 3 {
                                handle.request_interaction(
                                    context,
                                    Value::Sender(tx),
                                    Request::Dynamic(loc),
                                );
                                break;
                            }
                            value =
                                Value::Receiver(context.swap(runtime::Request::Dynamic(loc), tx));
                            consecutive_dynamic = previous_consecutive + 1;
                        }

                        Message::Swap(runtime::Request::Receive(loc), tx) => {
                            let (tx1, rx1) = oneshot::channel();
                            let (tx2, rx2) = oneshot::channel();
                            tx.send(Message::Send(Span::default(), Value::Receiver(rx1), rx2))
                                .ok()
                                .expect("receiver dropped");

                            let refresh = Arc::clone(&handle.refresh);
                            handle.add_event(Event::Receive(
                                loc,
                                Handle::start(refresh, context.split(), Value::Sender(tx1)),
                            ));

                            value = Value::Sender(tx2);
                        }

                        Message::Swap(runtime::Request::Match(loc, choices), tx) => {
                            handle.request_interaction(
                                context,
                                Value::Sender(tx),
                                Request::Either(loc, choices),
                            );
                            break;
                        }

                        Message::Swap(runtime::Request::Continue(loc), tx) => {
                            tx.send(Message::Break(Span::default()))
                                .ok()
                                .expect("receiver dropped");
                            handle.add_event(Event::Continue(loc));
                            break;
                        }

                        Message::Send(loc, argument, rx) => {
                            let refresh = Arc::clone(&handle.refresh);
                            handle.add_event(Event::Send(
                                loc,
                                Handle::start(refresh, context.split(), argument),
                            ));
                            value = Value::Receiver(rx);
                        }

                        Message::Choose(loc, chosen, rx) => {
                            handle.add_event(Event::Choose(loc, chosen));
                            value = Value::Receiver(rx);
                        }

                        Message::Break(loc) => {
                            handle.add_event(Event::Break(loc));
                            break;
                        }

                        Message::Error(error) => {
                            handle.interaction = Some(Err(error));
                            (handle.refresh)();
                            break;
                        }
                    }
                }

                Value::Sender(tx) => {
                    value = Value::Receiver(
                        context.swap(runtime::Request::Dynamic(Span::default()), tx),
                    );
                }
            };
        }
    }

    fn add_event(&mut self, event: Event<Span, Name, Typ>) {
        self.events.push(event);
        (self.refresh)();
    }

    fn request_interaction(
        &mut self,
        mut context: Context<Span, Name, Typ>,
        value: Value<Span, Name>,
        request: Request<Span, Name>,
    ) {
        if self.cancelled {
            context
                .spawner()
                .spawn(async move {
                    let _ = context.continue_from(Span::default(), value).await;
                })
                .expect("spawn failed");
            return;
        }
        self.interaction = Some(Ok(Interaction {
            context,
            value,
            request,
        }));
        (self.refresh)();
    }

    pub fn cancel(&mut self) {
        self.cancelled = true;
        for event in self.events.drain(..) {
            match event {
                Event::Send(_, int) => int.lock().expect("lock failed").cancel(),
                Event::Receive(_, int) => int.lock().expect("lock failed").cancel(),
                _ => continue,
            }
        }
        if let Some(Ok(mut int)) = self.interaction.take() {
            int.context
                .spawner()
                .spawn(async move {
                    let _ = int.context.continue_from(Span::default(), int.value).await;
                })
                .expect("spawn failed");
        }
    }
}
