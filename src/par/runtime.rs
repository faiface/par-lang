use futures::{
    channel::oneshot,
    task::{Spawn, SpawnExt},
};
use indexmap::IndexMap;
use std::{hash::Hash, sync::Arc};
use crate::location::Span;
use super::process::{Captures, Command, Expression, Process};

#[derive(Clone, Debug)]
pub enum Error<Loc, Name> {
    NameNotDefined(Loc, Name),
    ShadowedObligation(Loc, Name),
    UnfulfilledObligations(Loc, Vec<Name>),
    IncompatibleOperations(Operation<Loc, Name>, Operation<Loc, Name>),
    NoSuchLoopPoint(Loc, Option<Name>),
    Multiple(Box<Self>, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Operation<Loc, Name> {
    Unknown(Loc),
    Send(Loc),
    Receive(Loc),
    Choose(Loc, Name),
    Match(Loc, Arc<[Name]>),
    Break(Loc),
    Continue(Loc),
}

pub enum Message<Loc, Name> {
    Swap(Request<Loc, Name>, oneshot::Sender<Self>),
    Send(Loc, Value<Loc, Name>, oneshot::Receiver<Self>),
    Choose(Loc, Name, oneshot::Receiver<Self>),
    Break(Loc),
    Error(Error<Loc, Name>),
}

impl<Loc, Name> Message<Loc, Name> {
    pub fn into_operation_and_values(
        self,
    ) -> Result<(Operation<Loc, Name>, Vec<Value<Loc, Name>>), Error<Loc, Name>> {
        match self {
            Message::Swap(request, tx) => Ok((request.into_operation(), vec![Value::Sender(tx)])),
            Message::Send(loc, value, rx) => {
                Ok((Operation::Send(loc), vec![value, Value::Receiver(rx)]))
            }
            Message::Choose(loc, chosen, rx) => {
                Ok((Operation::Choose(loc, chosen), vec![Value::Receiver(rx)]))
            }
            Message::Break(loc) => Ok((Operation::Break(loc), vec![])),
            Message::Error(error) => Err(error),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Request<Loc, Name> {
    Receive(Loc),
    Match(Loc, Arc<[Name]>),
    Continue(Loc),
    Dynamic(Loc),
}

impl<Loc, Name> Request<Loc, Name> {
    pub fn into_operation(self) -> Operation<Loc, Name> {
        match self {
            Request::Receive(loc) => Operation::Receive(loc),
            Request::Match(loc, choices) => Operation::Match(loc, choices),
            Request::Continue(loc) => Operation::Continue(loc),
            Request::Dynamic(loc) => Operation::Unknown(loc),
        }
    }
}

impl<Loc, Name> Request<Loc, Name> {
    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Dynamic(_), _) | (_, Self::Dynamic(_)) => true,
            (Self::Receive(_), Self::Receive(_)) => true,
            (Self::Match(_, _), Self::Match(_, _)) => true,
            (Self::Continue(_), Self::Continue(_)) => true,
            (_, _) => false,
        }
    }
}

pub enum Value<Loc, Name> {
    Receiver(oneshot::Receiver<Message<Loc, Name>>),
    Sender(oneshot::Sender<Message<Loc, Name>>),
}

pub struct Context<Loc, Name, Typ> {
    spawner: Arc<dyn Spawn + Send + Sync>,
    globals: Arc<IndexMap<Name, Arc<Expression<Name, Typ>>>>,
    variables: IndexMap<Name, Value<Loc, Name>>,
    loop_points: IndexMap<Option<Name>, (Name, Arc<Process<Loc, Name, Typ>>)>,
}

impl<Name, Typ> Context<Span, Name, Typ>
where
    Name: Clone + Eq + Hash + Send + Sync + 'static,
    Typ: Send + Sync + 'static,
{
    pub fn new(
        spawner: Arc<dyn Spawn + Send + Sync>,
        globals: Arc<IndexMap<Name, Arc<Expression<Name, Typ>>>>,
    ) -> Self {
        Self {
            spawner,
            globals,
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
        }
    }

    pub fn spawner(&self) -> Arc<dyn Spawn + Send + Sync> {
        Arc::clone(&self.spawner)
    }

    pub fn split(&self) -> Self {
        Self {
            spawner: Arc::clone(&self.spawner),
            globals: Arc::clone(&self.globals),
            variables: IndexMap::new(),
            loop_points: self.loop_points.clone(),
        }
    }

    pub fn get_variable(&mut self, name: &Name) -> Option<Value<Span, Name>> {
        self.variables.shift_remove(name)
    }

    pub fn get(&mut self, loc: &Span, name: &Name) -> Result<Value<Span, Name>, Error<Span, Name>> {
        match self.get_variable(name) {
            Some(value) => Ok(value),
            None => match self.globals.get(name) {
                Some(expression) => self.evaluate(&Arc::clone(expression)),
                None => self.throw([], Error::NameNotDefined(loc.clone(), name.clone())),
            },
        }
    }

    pub fn put(
        &mut self,
        loc: &Span,
        name: Name,
        value: Value<Span, Name>,
    ) -> Result<(), Error<Span, Name>> {
        if let Some(value) = self.variables.shift_remove(&name) {
            return self.throw([value], Error::ShadowedObligation(loc.clone(), name));
        }
        self.variables.insert(name, value);
        Ok(())
    }

    pub fn capture(
        &mut self,
        cap: &Captures<Span, Name>,
        target: &mut Self,
    ) -> Result<(), Error<Span, Name>> {
        for (name, loc) in &cap.names {
            let value = match self.get_variable(name) {
                Some(value) => value,
                None => continue,
            };
            target.put(loc, name.clone(), value)?;
        }
        Ok(())
    }

    pub fn obligations(&self) -> impl Iterator<Item = &Name> {
        self.variables.iter().map(|(name, _)| name)
    }

    pub fn evaluate(
        &mut self,
        expression: &Expression<Name, Typ>,
    ) -> Result<Value<Span, Name>, Error<Span, Name>> {
        match expression {
            Expression::Reference(loc, name, _) => self.get(loc, name),

            Expression::Fork { span, captures: cap, chan_name: channel, process, .. } => {
                let mut context = self.split();
                self.capture(cap, &mut context)?;

                let (tx, rx) = oneshot::channel();
                context.put(span, channel.clone(), Value::Sender(tx))?;

                let process = Arc::clone(process);
                self.spawner
                    .spawn(async move {
                        let _ = context.run(process).await;
                    })
                    .expect("could not spawn");

                Ok(Value::Receiver(rx))
            }
        }
    }

    pub async fn run(
        &mut self,
        process: Arc<Process<Span, Name, Typ>>,
    ) -> Result<(), Error<Span, Name>> {
        let mut current_process = process;
        loop {
            match current_process.as_ref() {
                Process::Let(loc, name, _, _, expression, process) => {
                    let value = self.evaluate(expression)?;
                    self.put(loc, name.clone(), value)?;
                    current_process = Arc::clone(process);
                }

                Process::Do(loc, object_name, _, command) => {
                    let object = self.get(loc, object_name)?;

                    match command {
                        Command::Link(expression) => {
                            let value = match self.evaluate(&expression) {
                                Ok(value) => value,
                                Err(error) => return self.throw([object], error),
                            };
                            return self.link(loc.clone(), object, value).await;
                        }

                        Command::Send(argument, process) => {
                            let argument = match self.evaluate(&argument) {
                                Ok(value) => value,
                                Err(error) => return self.throw([object], error),
                            };
                            let object = self.send_to(loc.clone(), object, argument).await?;
                            self.put(loc, object_name.clone(), object)?;
                            current_process = Arc::clone(process);
                        }

                        Command::Receive(parameter, _, process) => {
                            let (argument, object) = self.receive_from(loc.clone(), object).await?;
                            self.put(loc, object_name.clone(), object)?;
                            self.put(loc, parameter.clone(), argument)?;
                            current_process = Arc::clone(process);
                        }

                        Command::Choose(chosen, process) => {
                            let object =
                                self.choose_in(loc.clone(), object, chosen.clone()).await?;
                            self.put(loc, object_name.clone(), object)?;
                            current_process = Arc::clone(process);
                        }

                        Command::Match(choices, processes) => {
                            let (loc1, chosen, object) = self
                                .either_of(loc.clone(), object, Arc::clone(choices))
                                .await?;
                            let index = match choices.iter().position(|c| c == &chosen) {
                                Some(index) => index,
                                None => {
                                    return self.throw(
                                        [object],
                                        Error::IncompatibleOperations(
                                            Operation::Choose(loc1, chosen),
                                            Operation::Match(loc.clone(), Arc::clone(choices)),
                                        ),
                                    )
                                }
                            };
                            self.put(loc, object_name.clone(), object)?;
                            current_process = Arc::clone(&processes[index]);
                        }

                        Command::Break => {
                            return self.break_to(loc.clone(), object).await;
                        }

                        Command::Continue(process) => {
                            self.continue_from(loc.clone(), object).await?;
                            current_process = Arc::clone(process);
                        }

                        Command::Begin(_, point, process) => {
                            self.loop_points
                                .insert(point.clone(), (object_name.clone(), Arc::clone(process)));
                            self.put(loc, object_name.clone(), object)?;
                            current_process = Arc::clone(process);
                        }

                        Command::Loop(point) => {
                            let Some((name, process)) = self.loop_points.get(point) else {
                                return self.throw(
                                    [object],
                                    Error::NoSuchLoopPoint(loc.clone(), point.clone()),
                                );
                            };
                            let name = name.clone();
                            let process = Arc::clone(process);
                            self.put(loc, name, object)?;
                            current_process = process;
                        }

                        Command::SendType(_, process) | Command::ReceiveType(_, process) => {
                            self.put(loc, object_name.clone(), object)?;
                            current_process = Arc::clone(process);
                        }
                    }
                }

                Process::Telltypes(_, process) => {
                    current_process = Arc::clone(process);
                }
            }
        }
    }

    pub async fn link(
        &mut self,
        loc: Span,
        left: Value<Span, Name>,
        right: Value<Span, Name>,
    ) -> Result<(), Error<Span, Name>> {
        let [left, right] = self.cannot_have_obligations(&loc, [left, right]).await?;
        match (left, right) {
            (Value::Receiver(rx1), Value::Receiver(rx2)) => {
                match (
                    rx1.await.ok().expect("sender dropped"),
                    rx2.await.ok().expect("sender dropped"),
                ) {
                    (Message::Swap(_, tx), message) | (message, Message::Swap(_, tx)) => {
                        tx.send(message).ok().expect("receiver dropped");
                    }
                    (message1, message2) => self.invalid_message_and_message(message1, message2)?,
                }
            }
            (Value::Sender(tx1), Value::Sender(tx2)) => {
                let message = self
                    .swap(Request::Dynamic(loc), tx1)
                    .await
                    .ok()
                    .expect("sender dropped");
                tx2.send(message).ok().expect("receiver dropped");
            }
            (Value::Receiver(rx), Value::Sender(tx)) | (Value::Sender(tx), Value::Receiver(rx)) => {
                let message = rx.await.ok().expect("sender dropped");
                tx.send(message).ok().expect("receiver dropped");
            }
        }
        Ok(())
    }

    pub async fn send_to(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
        argument: Value<Span, Name>,
    ) -> Result<Value<Span, Name>, Error<Span, Name>> {
        let tx = match object {
            Value::Receiver(rx) => self.expect_swap(Request::Receive(loc.clone()), rx).await?,
            Value::Sender(tx) => tx,
        };
        let (tx1, rx1) = oneshot::channel();
        tx.send(Message::Send(loc, argument, rx1))
            .ok()
            .expect("receiver dropped");
        Ok(Value::Sender(tx1))
    }

    pub async fn receive_from(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
    ) -> Result<(Value<Span, Name>, Value<Span, Name>), Error<Span, Name>> {
        let mut rx = match object {
            Value::Receiver(rx) => rx,
            Value::Sender(tx) => self.swap(Request::Receive(loc.clone()), tx),
        };
        loop {
            match rx.await.ok().expect("sender dropped") {
                Message::Swap(Request::Dynamic(_), tx) => {
                    rx = self.swap(Request::Receive(loc.clone()), tx);
                    continue;
                }
                Message::Send(_, argument, rx) => return Ok((argument, Value::Receiver(rx))),
                message => return self.invalid_message_and_request(message, Request::Receive(loc)),
            }
        }
    }

    pub async fn choose_in(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
        chosen: Name,
    ) -> Result<Value<Span, Name>, Error<Span, Name>> {
        let tx = match object {
            Value::Receiver(rx) => self.expect_swap_choose(loc.clone(), &chosen, rx).await?,
            Value::Sender(tx) => tx,
        };
        let (tx1, rx1) = oneshot::channel();
        tx.send(Message::Choose(loc, chosen, rx1))
            .ok()
            .expect("receiver dropped");
        Ok(Value::Sender(tx1))
    }

    pub async fn either_of(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
        choices: Arc<[Name]>,
    ) -> Result<(Span, Name, Value<Span, Name>), Error<Span, Name>> {
        let request = Request::Match(loc.clone(), Arc::clone(&choices));
        let mut rx = match object {
            Value::Receiver(rx) => rx,
            Value::Sender(tx) => self.swap(request.clone(), tx),
        };
        loop {
            match rx.await.ok().expect("sender dropped") {
                Message::Swap(Request::Dynamic(_), tx) => {
                    rx = self.swap(request.clone(), tx);
                    continue;
                }
                Message::Choose(loc, chosen, rx) => return Ok((loc, chosen, Value::Receiver(rx))),
                message => return self.invalid_message_and_request(message, request),
            }
        }
    }

    pub async fn break_to(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
    ) -> Result<(), Error<Span, Name>> {
        let [object] = self.cannot_have_obligations(&loc, [object]).await?;
        let tx = match object {
            Value::Receiver(rx) => self.expect_swap(Request::Continue(loc.clone()), rx).await?,
            Value::Sender(tx) => tx,
        };
        tx.send(Message::Break(loc)).ok().expect("receiver dropped");
        Ok(())
    }

    pub async fn continue_from(
        &mut self,
        loc: Span,
        object: Value<Span, Name>,
    ) -> Result<(), Error<Span, Name>> {
        let mut rx = match object {
            Value::Receiver(rx) => rx,
            Value::Sender(tx) => self.swap(Request::Continue(loc.clone()), tx),
        };
        loop {
            match rx.await.ok().expect("sender dropped") {
                Message::Swap(Request::Dynamic(_), tx) => {
                    rx = self.swap(Request::Continue(loc.clone()), tx);
                    continue;
                }
                Message::Break(_) => return Ok(()),
                message => {
                    return self.invalid_message_and_request(message, Request::Continue(loc))
                }
            }
        }
    }

    async fn cannot_have_obligations<V: IntoIterator<Item = Value<Span, Name>>>(
        &mut self,
        loc: &Span,
        values: V,
    ) -> Result<V, Error<Span, Name>> {
        if self.obligations().any(|_| true) {
            return self.throw(
                values,
                Error::UnfulfilledObligations(loc.clone(), self.obligations().cloned().collect()),
            );
        }
        Ok(values)
    }

    pub fn swap(
        &mut self,
        request: Request<Span, Name>,
        tx: oneshot::Sender<Message<Span, Name>>,
    ) -> oneshot::Receiver<Message<Span, Name>> {
        let (tx1, rx1) = oneshot::channel();
        tx.send(Message::Swap(request, tx1))
            .ok()
            .expect("receiver dropped");
        rx1
    }

    async fn expect_swap(
        &mut self,
        expected_request: Request<Span, Name>,
        rx: oneshot::Receiver<Message<Span, Name>>,
    ) -> Result<oneshot::Sender<Message<Span, Name>>, Error<Span, Name>> {
        match rx.await.ok().expect("sender dropped") {
            Message::Swap(request, tx) if request.matches(&expected_request) => Ok(tx),
            message => self.invalid_message_and_request(message, expected_request),
        }
    }

    async fn expect_swap_choose(
        &mut self,
        loc: Span,
        chosen: &Name,
        rx: oneshot::Receiver<Message<Span, Name>>,
    ) -> Result<oneshot::Sender<Message<Span, Name>>, Error<Span, Name>> {
        match rx.await.ok().expect("sender dropped") {
            Message::Swap(Request::Dynamic(_), tx) => Ok(tx),
            Message::Swap(Request::Match(_, choices), tx)
                if choices.iter().any(|c| c == chosen) =>
            {
                Ok(tx)
            }
            message => self.invalid_message_and_request(
                message,
                Request::Match(loc, Arc::new([chosen.clone()])),
            ),
        }
    }

    fn invalid_message_and_request<T>(
        &mut self,
        message: Message<Span, Name>,
        request: Request<Span, Name>,
    ) -> Result<T, Error<Span, Name>> {
        match (
            message.into_operation_and_values(),
            request.into_operation(),
        ) {
            (Err(error), _) => self.throw([], error),
            (Ok((op1, values)), op2) => self.throw(values, Error::IncompatibleOperations(op1, op2)),
        }
    }

    fn invalid_message_and_message<T>(
        &mut self,
        message1: Message<Span, Name>,
        message2: Message<Span, Name>,
    ) -> Result<T, Error<Span, Name>> {
        match (
            message1.into_operation_and_values(),
            message2.into_operation_and_values(),
        ) {
            (Err(error1), Err(error2)) => {
                self.throw([], Error::Multiple(Box::new(error1), Box::new(error2)))
            }
            (Err(error), Ok((_, values))) | (Ok((_, values)), Err(error)) => {
                self.throw(values, error)
            }
            (Ok((op1, values1)), Ok((op2, values2))) => self.throw(
                values1.into_iter().chain(values2),
                Error::IncompatibleOperations(op1, op2),
            ),
        }
    }

    fn throw<T>(
        &mut self,
        values: impl IntoIterator<Item = Value<Span, Name>>,
        error: Error<Span, Name>,
    ) -> Result<T, Error<Span, Name>> {
        let mut pending = self
            .variables
            .drain(..)
            .map(|(_, value)| value)
            .collect::<Vec<_>>();
        pending.extend(values);

        self.spawner
            .spawn({
                let mut error = error.clone();
                async move {
                    while let Some(value) = pending.pop() {
                        match value {
                            Value::Receiver(rx) => match rx.await.ok().expect("sender dropped") {
                                Message::Swap(_, tx) => pending.push(Value::Sender(tx)),
                                Message::Send(_, argument, rx) => {
                                    pending.push(argument);
                                    pending.push(Value::Receiver(rx));
                                }
                                Message::Choose(_, _, rx) => pending.push(Value::Receiver(rx)),
                                Message::Break(_) => (),
                                Message::Error(error1) => {
                                    error = Error::Multiple(Box::new(error), Box::new(error1))
                                }
                            },
                            Value::Sender(tx) => tx
                                .send(Message::Error(error.clone()))
                                .ok()
                                .expect("receiver dropped"),
                        }
                    }
                }
            })
            .expect("spawn failed");

        Err(error)
    }
}
