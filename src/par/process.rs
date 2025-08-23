use super::{
    language::{GlobalName, LocalName},
    primitive::Primitive,
    types::{Type, TypeDefs},
};
use crate::{
    icombs::readback::Handle,
    location::{Span, Spanning},
};
use indexmap::IndexMap;
use std::{
    fmt::{self, Write},
    future::Future,
    pin::Pin,
    sync::Arc,
};

#[derive(Clone, Debug)]
pub enum Process<Typ> {
    Let {
        span: Span,
        name: LocalName,
        annotation: Option<Type>,
        typ: Typ,
        value: Arc<Expression<Typ>>,
        then: Arc<Self>,
    },
    Do {
        span: Span,
        name: LocalName,
        typ: Typ,
        command: Command<Typ>,
    },
    Telltypes(Span, Arc<Self>),
}

#[derive(Clone, Debug)]
pub enum Command<Typ> {
    Link(Arc<Expression<Typ>>),
    Send(Arc<Expression<Typ>>, Arc<Process<Typ>>),
    Receive(LocalName, Option<Type>, Typ, Arc<Process<Typ>>),
    Signal(LocalName, Arc<Process<Typ>>),
    Case(Arc<[LocalName]>, Box<[Arc<Process<Typ>>]>),
    Break,
    Continue(Arc<Process<Typ>>),
    Begin {
        unfounded: bool,
        label: Option<LocalName>,
        captures: Captures,
        body: Arc<Process<Typ>>,
    },
    Loop(Option<LocalName>, LocalName, Captures),
    SendType(Type, Arc<Process<Typ>>),
    ReceiveType(LocalName, Arc<Process<Typ>>),
}

#[derive(Clone, Debug)]
pub enum Expression<Typ> {
    Global(Span, GlobalName, Typ),
    Variable(Span, LocalName, Typ),
    Box(Span, Captures, Arc<Self>, Typ),
    Chan {
        span: Span,
        captures: Captures,
        chan_name: LocalName,
        chan_annotation: Option<Type>,
        chan_type: Typ,
        expr_type: Typ,
        process: Arc<Process<Typ>>,
    },
    Primitive(Span, Primitive, Typ),
    External(
        Type,
        fn(Handle) -> Pin<Box<dyn Send + Future<Output = ()>>>,
        Typ,
    ),
}

impl<Typ> Spanning for Process<Typ> {
    fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => span.clone(),
            Self::Do { span, .. } => span.clone(),
            Self::Telltypes(span, ..) => span.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Captures {
    pub names: IndexMap<LocalName, Span>,
}

impl Default for Captures {
    fn default() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }
}

impl Captures {
    pub fn new() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }

    pub fn single(name: LocalName, span: Span) -> Self {
        let mut caps = Self::new();
        caps.add(name, span);
        caps
    }

    pub fn extend(&mut self, other: Self) {
        for (name, span) in other.names {
            self.names.insert(name, span);
        }
    }

    pub fn add(&mut self, name: LocalName, span: Span) {
        self.names.insert(name, span);
    }

    pub fn remove(&mut self, name: &LocalName) -> Option<Span> {
        self.names.shift_remove(name)
    }
}

impl Process<()> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<LocalName>, (LocalName, Captures)>,
    ) -> (Arc<Self>, Captures) {
        match self {
            Self::Let {
                span,
                name,
                annotation,
                typ,
                value: expression,
                then: process,
            } => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(name);
                let (expression, caps1) = expression.fix_captures(loop_points);
                caps.extend(caps1);
                (
                    Arc::new(Self::Let {
                        span: span.clone(),
                        name: name.clone(),
                        annotation: annotation.clone(),
                        typ: typ.clone(),
                        value: expression,
                        then: process,
                    }),
                    caps,
                )
            }
            Self::Do {
                span,
                name,
                typ,
                command,
            } => {
                let (command, mut caps) = command.fix_captures(name, loop_points);
                caps.add(name.clone(), span.clone());
                (
                    Arc::new(Self::Do {
                        span: span.clone(),
                        name: name.clone(),
                        typ: typ.clone(),
                        command,
                    }),
                    caps,
                )
            }
            Self::Telltypes(span, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Arc::new(Self::Telltypes(span.clone(), process)), caps)
            }
        }
    }

    pub fn optimize(&self) -> Arc<Self> {
        match self {
            Self::Let {
                span,
                name,
                annotation,
                typ,
                value: expression,
                then: process,
            } => Arc::new(Self::Let {
                span: span.clone(),
                name: name.clone(),
                annotation: annotation.clone(),
                typ: typ.clone(),
                value: expression.optimize(),
                then: process.optimize(),
            }),
            Self::Do {
                span,
                name,
                typ,
                command,
            } => Arc::new(Self::Do {
                span: span.clone(),
                name: name.clone(),
                typ: typ.clone(),
                command: match command {
                    Command::Link(expression) => {
                        let expression = expression.optimize();
                        match expression.optimize().as_ref() {
                            Expression::Chan {
                                chan_name: channel,
                                process,
                                ..
                            } => {
                                if name == channel {
                                    return Arc::clone(process);
                                } else {
                                    return Arc::new(Process::Let {
                                        span: span.clone(),
                                        name: channel.clone(),
                                        annotation: None,
                                        typ: (),
                                        value: Arc::new(Expression::Variable(
                                            span.clone(),
                                            name.clone(),
                                            (),
                                        )),
                                        then: Arc::clone(process),
                                    });
                                }
                            }
                            _ => Command::Link(expression),
                        }
                    }
                    Command::Send(argument, process) => {
                        Command::Send(argument.optimize(), process.optimize())
                    }
                    Command::Receive(parameter, annotation, typ, process) => Command::Receive(
                        parameter.clone(),
                        annotation.clone(),
                        typ.clone(),
                        process.optimize(),
                    ),
                    Command::Signal(chosen, process) => {
                        Command::Signal(chosen.clone(), process.optimize())
                    }
                    Command::Case(branches, processes) => {
                        let processes = processes.iter().map(|p| p.optimize()).collect();
                        Command::Case(Arc::clone(branches), processes)
                    }
                    Command::Break => Command::Break,
                    Command::Continue(process) => Command::Continue(process.optimize()),
                    Command::Begin {
                        unfounded,
                        label,
                        captures,
                        body: process,
                    } => Command::Begin {
                        unfounded: unfounded.clone(),
                        label: label.clone(),
                        captures: captures.clone(),
                        body: process.optimize(),
                    },
                    Command::Loop(label, driver, captures) => {
                        Command::Loop(label.clone(), driver.clone(), captures.clone())
                    }
                    Command::SendType(argument, process) => {
                        Command::SendType(argument.clone(), process.optimize())
                    }
                    Command::ReceiveType(parameter, process) => {
                        Command::ReceiveType(parameter.clone(), process.optimize())
                    }
                },
            }),
            Self::Telltypes(span, process) => {
                Arc::new(Self::Telltypes(span.clone(), process.optimize()))
            }
        }
    }
}

impl Process<Type> {
    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs,
        consume: &mut impl FnMut(Span, Option<String>, Type),
    ) {
        match self {
            Process::Let {
                name,
                annotation,
                typ,
                value,
                then,
                ..
            } => {
                value.types_at_spans(type_defs, consume);
                consume(name.span(), Some(format!("{}", name)), typ.clone());
                if let Some(annotation) = annotation {
                    annotation.types_at_spans(type_defs, consume);
                }
                then.types_at_spans(type_defs, consume);
            }
            Process::Do {
                span,
                name,
                typ,
                command,
                ..
            } => {
                consume(name.span(), Some(format!("{}", name)), typ.clone());
                if name == &LocalName::result() {
                    consume(*span, None, typ.clone().dual(Span::None));
                } else if name == &LocalName::object() {
                    consume(*span, None, typ.clone());
                } else {
                    consume(*span, Some(format!("{}", name)), typ.clone());
                }
                command.types_at_spans(type_defs, consume);
            }
            Process::Telltypes(_, process) => {
                process.types_at_spans(type_defs, consume);
            }
        }
    }
}

impl Command<()> {
    pub fn fix_captures(
        &self,
        subject: &LocalName,
        loop_points: &IndexMap<Option<LocalName>, (LocalName, Captures)>,
    ) -> (Self, Captures) {
        match self {
            Self::Link(expression) => {
                let (expression, caps) = expression.fix_captures(loop_points);
                (Self::Link(expression), caps)
            }
            Self::Send(argument, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                let (argument, caps1) = argument.fix_captures(loop_points);
                caps.extend(caps1);
                (Self::Send(argument, process), caps)
            }
            Self::Receive(parameter, annotation, typ, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(parameter);
                (
                    Self::Receive(parameter.clone(), annotation.clone(), typ.clone(), process),
                    caps,
                )
            }
            Self::Signal(chosen, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Signal(chosen.clone(), process), caps)
            }
            Self::Case(branches, processes) => {
                let mut fixed_processes = Vec::new();
                let mut caps = Captures::new();
                for process in processes {
                    let (process, caps1) = process.fix_captures(loop_points);
                    fixed_processes.push(process);
                    caps.extend(caps1);
                }
                (
                    Self::Case(branches.clone(), fixed_processes.into_boxed_slice()),
                    caps,
                )
            }
            Self::Break => (Self::Break, Captures::new()),
            Self::Continue(process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Continue(process), caps)
            }
            Self::Begin {
                unfounded,
                label,
                captures: _,
                body: process,
            } => {
                let (_, mut loop_caps) = process.fix_captures(loop_points);
                loop_caps.remove(subject);
                let mut loop_points = loop_points.clone();
                loop_points.insert(label.clone(), (subject.clone(), loop_caps.clone()));
                let (process, caps) = process.fix_captures(&loop_points);
                (
                    Self::Begin {
                        unfounded: unfounded.clone(),
                        label: label.clone(),
                        captures: loop_caps.clone(),
                        body: process,
                    },
                    caps,
                )
            }
            Self::Loop(label, _, _) => {
                let (driver, loop_caps) = loop_points
                    .get(label)
                    .cloned()
                    .unwrap_or((LocalName::invalid(), Captures::default()));
                (
                    Self::Loop(label.clone(), driver, loop_caps.clone()),
                    loop_caps,
                )
            }
            Self::SendType(argument, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::SendType(argument.clone(), process), caps)
            }
            Self::ReceiveType(parameter, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::ReceiveType(parameter.clone(), process), caps)
            }
        }
    }
}

impl Command<Type> {
    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs,
        consume: &mut impl FnMut(Span, Option<String>, Type),
    ) {
        match self {
            Self::Link(expression) => {
                expression.types_at_spans(type_defs, consume);
            }
            Self::Send(argument, process) => {
                argument.types_at_spans(type_defs, consume);
                process.types_at_spans(type_defs, consume);
            }
            Self::Receive(param, annotation, param_type, process) => {
                consume(param.span(), Some(format!("{}", param)), param_type.clone());
                if let Some(annotation) = annotation {
                    annotation.types_at_spans(type_defs, consume);
                }
                process.types_at_spans(type_defs, consume);
            }
            Self::Signal(_, process) => {
                process.types_at_spans(type_defs, consume);
            }
            Self::Case(_, branches) => {
                for process in branches {
                    process.types_at_spans(type_defs, consume);
                }
            }
            Self::Break => {}
            Self::Continue(process) => {
                process.types_at_spans(type_defs, consume);
            }
            Self::Begin { body, .. } => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Loop(_, _, _) => {}
            Self::SendType(_, process) => {
                process.types_at_spans(type_defs, consume);
            }
            Self::ReceiveType(_, process) => {
                process.types_at_spans(type_defs, consume);
            }
        }
    }
}

impl Expression<()> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<LocalName>, (LocalName, Captures)>,
    ) -> (Arc<Self>, Captures) {
        match self {
            Self::Global(span, name, typ) => (
                Arc::new(Self::Global(*span, name.clone(), typ.clone())),
                Captures::new(),
            ),
            Self::Variable(span, name, typ) => (
                Arc::new(Self::Variable(*span, name.clone(), typ.clone())),
                Captures::single(name.clone(), span.clone()),
            ),
            Self::Box(span, _, expression, typ) => {
                let (expression, caps) = expression.fix_captures(loop_points);
                (
                    Arc::new(Self::Box(*span, caps.clone(), expression, typ.clone())),
                    caps,
                )
            }
            Self::Chan {
                span,
                chan_name: channel,
                chan_annotation: annotation,
                chan_type,
                expr_type,
                process,
                ..
            } => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(channel);
                (
                    Arc::new(Self::Chan {
                        span: *span,
                        captures: caps.clone(),
                        chan_name: channel.clone(),
                        chan_annotation: annotation.clone(),
                        chan_type: chan_type.clone(),
                        expr_type: expr_type.clone(),
                        process,
                    }),
                    caps,
                )
            }
            Self::Primitive(span, value, typ) => (
                Arc::new(Self::Primitive(span.clone(), value.clone(), typ.clone())),
                Captures::new(),
            ),
            Self::External(claimed_type, f, typ) => (
                Arc::new(Self::External(claimed_type.clone(), *f, typ.clone())),
                Captures::new(),
            ),
        }
    }

    pub fn optimize(&self) -> Arc<Self> {
        match self {
            Self::Global(span, name, typ) => {
                Arc::new(Self::Global(*span, name.clone(), typ.clone()))
            }
            Self::Variable(span, name, typ) => {
                Arc::new(Self::Variable(*span, name.clone(), typ.clone()))
            }
            Self::Box(span, caps, expression, typ) => Arc::new(Self::Box(
                *span,
                caps.clone(),
                expression.optimize(),
                typ.clone(),
            )),
            Self::Chan {
                span,
                captures,
                chan_name,
                chan_annotation,
                chan_type,
                expr_type,
                process,
            } => Arc::new(Self::Chan {
                span: span.clone(),
                captures: captures.clone(),
                chan_name: chan_name.clone(),
                chan_annotation: chan_annotation.clone(),
                chan_type: chan_type.clone(),
                expr_type: expr_type.clone(),
                process: process.optimize(),
            }),
            Self::Primitive(span, value, typ) => {
                Arc::new(Self::Primitive(span.clone(), value.clone(), typ.clone()))
            }
            Self::External(claimed_type, f, typ) => {
                Arc::new(Self::External(claimed_type.clone(), *f, typ.clone()))
            }
        }
    }
}

impl Expression<Type> {
    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs,
        consume: &mut impl FnMut(Span, Option<String>, Type),
    ) {
        match self {
            Self::Global(_, name, typ) => {
                consume(name.span(), Some(format!("{}", name)), typ.clone());
            }
            Self::Variable(_, name, typ) => {
                consume(name.span(), Some(format!("{}", name)), typ.clone());
            }
            Self::Box(span, _, expression, typ) => {
                consume(*span, None, typ.clone());
                expression.types_at_spans(type_defs, consume);
            }
            Self::Chan {
                chan_name,
                chan_annotation,
                chan_type,
                process,
                ..
            } => {
                consume(
                    chan_name.span(),
                    Some(format!("{}", chan_name)),
                    chan_type.clone(),
                );
                if let Some(chan_annotation) = chan_annotation {
                    chan_annotation.types_at_spans(type_defs, consume);
                }
                process.types_at_spans(type_defs, consume);
            }
            Self::Primitive(_, _, _) => {}
            Self::External(_, _, _) => {}
        }
    }
}

impl<Typ: Clone> Expression<Typ> {
    pub fn get_type(&self) -> Typ {
        match self {
            Self::Global(_, _, typ) => typ.clone(),
            Self::Variable(_, _, typ) => typ.clone(),
            Self::Box(_, _, _, typ) => typ.clone(),
            Self::Chan { expr_type, .. } => expr_type.clone(),
            Self::Primitive(_, _, typ) => typ.clone(),
            Self::External(_, _, typ) => typ.clone(),
        }
    }
}

impl Process<()> {
    pub fn qualify(self: &mut Arc<Self>, module: Option<&str>) {
        match Arc::make_mut(self) {
            Self::Let {
                span: _,
                name: _,
                annotation,
                typ: (),
                value,
                then,
            } => {
                if let Some(annotation) = annotation {
                    annotation.qualify(module);
                }
                value.qualify(module);
                then.qualify(module);
            }
            Self::Do {
                span: _,
                name: _,
                typ: (),
                command,
            } => command.qualify(module),
            Self::Telltypes(_span, process) => process.qualify(module),
        }
    }
}

impl Command<()> {
    pub fn qualify(&mut self, module: Option<&str>) {
        match self {
            Self::Link(expression) => expression.qualify(module),
            Self::Send(expression, process) => {
                expression.qualify(module);
                process.qualify(module);
            }
            Self::Receive(_, annotation, (), process) => {
                if let Some(annotation) = annotation {
                    annotation.qualify(module);
                }
                process.qualify(module);
            }
            Self::Signal(_, process) => {
                process.qualify(module);
            }
            Self::Case(_, branches) => {
                for process in branches {
                    process.qualify(module);
                }
            }
            Self::Break => {}
            Self::Continue(process) => {
                process.qualify(module);
            }
            Self::Begin { body, .. } => {
                body.qualify(module);
            }
            Self::Loop(_, _, _) => {}
            Self::SendType(argument, process) => {
                argument.qualify(module);
                process.qualify(module);
            }
            Self::ReceiveType(_, process) => {
                process.qualify(module);
            }
        }
    }
}

impl Expression<()> {
    pub fn qualify(self: &mut Arc<Self>, module: Option<&str>) {
        match Arc::make_mut(self) {
            Self::Global(_span, name, ()) => name.qualify(module),
            Self::Variable(_span, _name, ()) => {}
            Self::Box(_span, _caps, expression, ()) => expression.qualify(module),
            Self::Chan {
                span: _,
                captures: _,
                chan_name: _,
                chan_annotation,
                chan_type: (),
                expr_type: (),
                process,
            } => {
                if let Some(chan_annotation) = chan_annotation {
                    chan_annotation.qualify(module);
                }
                process.qualify(module)
            }
            Self::Primitive(_span, _primitive, ()) => {}
            Self::External(claimed_type, _, _) => claimed_type.qualify(module),
        }
    }
}

impl<Typ> Process<Typ> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Let {
                span: _,
                name,
                annotation: _,
                typ: _,
                value: expression,
                then: process,
            } => {
                indentation(f, indent)?;
                write!(f, "let {} = ", name)?;
                expression.pretty(f, indent)?;
                process.pretty(f, indent)
            }

            Self::Do {
                span: _,
                name: subject,
                typ: _,
                command,
            } => {
                indentation(f, indent)?;
                write!(f, "{}", subject)?;

                match command {
                    Command::Link(expression) => {
                        write!(f, " <> ")?;
                        expression.pretty(f, indent)
                    }

                    Command::Send(argument, process) => {
                        write!(f, "(")?;
                        argument.pretty(f, indent)?;
                        write!(f, ")")?;
                        process.pretty(f, indent)
                    }

                    Command::Receive(parameter, _, _, process) => {
                        write!(f, "[{}]", parameter)?;
                        process.pretty(f, indent)
                    }

                    Command::Signal(chosen, process) => {
                        write!(f, ".{}", chosen)?;
                        process.pretty(f, indent)
                    }

                    Command::Case(choices, branches) => {
                        write!(f, ".case {{")?;
                        for (choice, process) in choices.iter().zip(branches.iter()) {
                            indentation(f, indent + 1)?;
                            write!(f, ".{} => {{", choice)?;
                            process.pretty(f, indent + 2)?;
                            indentation(f, indent + 1)?;
                            write!(f, "}}")?;
                        }
                        indentation(f, indent)?;
                        write!(f, "}}")
                    }

                    Command::Break => {
                        write!(f, "!")
                    }

                    Command::Continue(process) => {
                        write!(f, "?")?;
                        process.pretty(f, indent)
                    }

                    Command::Begin {
                        unfounded,
                        label,
                        body: process,
                        ..
                    } => {
                        if *unfounded {
                            write!(f, ".unfounded")?;
                        } else {
                            write!(f, ".begin")?;
                        }
                        if let Some(label) = label {
                            write!(f, "/{}", label)?;
                        }
                        process.pretty(f, indent)
                    }

                    Command::Loop(label, driver, caps) => {
                        write!(f, ".loop")?;
                        if let Some(label) = label {
                            write!(f, "/{} ", label)?;
                        }
                        write!(f, "{{{} |", driver)?;
                        for var in caps.names.keys() {
                            write!(f, " {}", var)?;
                        }
                        write!(f, "}}")?;
                        Ok(())
                    }

                    Command::SendType(argument, process) => {
                        write!(f, "(type ")?;
                        argument.pretty(f, indent)?;
                        write!(f, ")")?;
                        process.pretty(f, indent)
                    }

                    Command::ReceiveType(parameter, process) => {
                        write!(f, "[type {}]", parameter)?;
                        process.pretty(f, indent)
                    }
                }
            }

            Self::Telltypes(_, process) => {
                indentation(f, indent)?;
                write!(f, "telltypes")?;
                process.pretty(f, indent)
            }
        }
    }
}

impl<Typ> Expression<Typ> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Global(_, name, _) => {
                write!(f, "{}", name)
            }

            Self::Variable(_, name, _) => {
                write!(f, "{}", name)
            }

            Self::Box(_, _, expression, _) => {
                write!(f, "box ")?;
                expression.pretty(f, indent)
            }

            Self::Chan {
                chan_name: channel,
                process,
                ..
            } => {
                write!(f, "chan {} {{", channel)?;
                process.pretty(f, indent + 1)?;
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Primitive(_, value, _) => value.pretty(f, indent),

            Self::External(_, _, _) => {
                write!(f, "<external>")
            }
        }
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
