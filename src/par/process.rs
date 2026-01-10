pub use super::captures::{Captures, VariableUsage};
use super::{
    language::{GlobalName, LocalName},
    primitive::Primitive,
    types::Type,
};
use crate::{
    location::{Span, Spanning},
    par::program::CheckedModule,
    runtime::Handle,
};
use indexmap::IndexSet;
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
        usage: VariableUsage,
        typ: Typ,
        command: Command<Typ>,
    },
    Telltypes(Span, Arc<Self>),
    Block(Span, usize, Arc<Self>, Arc<Self>),
    Goto(Span, usize, Captures),
    Unreachable(Span),
}

#[derive(Clone, Debug)]
pub enum Command<Typ> {
    Link(Arc<Expression<Typ>>),
    Send(Arc<Expression<Typ>>, Arc<Process<Typ>>),
    Receive(
        LocalName,
        Option<Type>,
        Typ,
        Arc<Process<Typ>>,
        Vec<LocalName>,
    ),
    Signal(LocalName, Arc<Process<Typ>>),
    Case(
        Arc<[LocalName]>,
        Box<[Arc<Process<Typ>>]>,
        Option<Arc<Process<Typ>>>,
    ),
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
    Variable(Span, LocalName, Typ, VariableUsage),
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
            Self::Block(span, _, _, _) => span.clone(),
            Self::Goto(span, _, _) => span.clone(),
            Self::Unreachable(span) => span.clone(),
        }
    }
}

impl Process<()> {
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
                usage,
                typ,
                command,
            } => Arc::new(Self::Do {
                span: span.clone(),
                name: name.clone(),
                typ: typ.clone(),
                usage: usage.clone(),
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
                                            VariableUsage::Unknown,
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
                    Command::Receive(parameter, annotation, typ, process, vars) => {
                        Command::Receive(
                            parameter.clone(),
                            annotation.clone(),
                            typ.clone(),
                            process.optimize(),
                            vars.clone(),
                        )
                    }
                    Command::Signal(chosen, process) => {
                        Command::Signal(chosen.clone(), process.optimize())
                    }
                    Command::Case(branches, processes, else_process) => {
                        let processes = processes.iter().map(|p| p.optimize()).collect();
                        let else_process = else_process.clone().map(|p| p.optimize());
                        Command::Case(Arc::clone(branches), processes, else_process)
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
            Self::Block(span, index, body, process) => Arc::new(Self::Block(
                span.clone(),
                *index,
                body.optimize(),
                process.optimize(),
            )),
            Self::Goto(span, index, caps) => {
                Arc::new(Self::Goto(span.clone(), *index, caps.clone()))
            }
            Self::Unreachable(span) => Arc::new(Self::Unreachable(span.clone())),
        }
    }
}

impl Process<Type> {
    pub fn types_at_spans(
        &self,
        program: &CheckedModule,
        consume: &mut impl FnMut(Span, NameWithType),
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
                value.types_at_spans(program, consume);
                consume(name.span(), NameWithType::named(name, typ.clone()));
                if let Some(annotation) = annotation {
                    annotation.types_at_spans(&program.type_defs, consume);
                }
                then.types_at_spans(program, consume);
            }
            Process::Do {
                span,
                name,
                typ,
                command,
                ..
            } => {
                consume(name.span(), NameWithType::named(name, typ.clone()));
                if name == &LocalName::result() {
                    consume(
                        span.clone(),
                        NameWithType::unnamed(typ.clone().dual(Span::None)),
                    );
                } else if name == &LocalName::object() {
                    consume(span.clone(), NameWithType::unnamed(typ.clone()));
                } else {
                    consume(span.clone(), NameWithType::named(name, typ.clone()));
                }
                command.types_at_spans(program, consume);
            }
            Process::Telltypes(_, process) => {
                process.types_at_spans(program, consume);
            }
            Process::Block(_, _, body, process) => {
                body.types_at_spans(program, consume);
                process.types_at_spans(program, consume);
            }
            Process::Goto(_, _, _) => {}
            Process::Unreachable(_) => {}
        }
    }
}

impl<Typ> Command<Typ> {
    pub fn free_variables(&self) -> IndexSet<LocalName> {
        match self {
            Command::Link(expression) => expression.free_variables(),
            Command::Send(argument, process) => {
                let mut vars = argument.free_variables();
                vars.extend(process.free_variables());
                vars
            }
            Command::Receive(parameter, _annot, _typ, process, _vars) => {
                let mut vars = process.free_variables();
                vars.shift_remove(parameter);
                vars
            }
            Command::Signal(_, process) => process.free_variables(),
            Command::Case(_, processes, else_process) => {
                let mut vars: IndexSet<LocalName> =
                    processes.iter().flat_map(|p| p.free_variables()).collect();
                if let Some(p) = else_process {
                    vars.extend(p.free_variables());
                }
                vars
            }
            Command::Break => IndexSet::new(),
            Command::Continue(process) => process.free_variables(),
            Command::Begin { captures, body, .. } => {
                let mut vars: IndexSet<LocalName> = captures.names.keys().cloned().collect();
                vars.extend(body.free_variables());
                vars
            }
            Command::Loop(_, _, captures) => captures.names.keys().cloned().collect(),
            Command::SendType(_, process) => process.free_variables(),
            Command::ReceiveType(_, process) => process.free_variables(),
        }
    }
}

impl Command<Type> {
    pub fn types_at_spans(
        &self,
        program: &CheckedModule,
        consume: &mut impl FnMut(Span, NameWithType),
    ) {
        match self {
            Self::Link(expression) => {
                expression.types_at_spans(program, consume);
            }
            Self::Send(argument, process) => {
                argument.types_at_spans(program, consume);
                process.types_at_spans(program, consume);
            }
            Self::Receive(param, annotation, param_type, process, _) => {
                consume(param.span(), NameWithType::named(param, param_type.clone()));
                if let Some(annotation) = annotation {
                    annotation.types_at_spans(&program.type_defs, consume);
                }
                process.types_at_spans(program, consume);
            }
            Self::Signal(_, process) => {
                process.types_at_spans(program, consume);
            }
            Self::Case(_, branches, else_process) => {
                for process in branches {
                    process.types_at_spans(program, consume);
                }
                if let Some(process) = else_process {
                    process.types_at_spans(program, consume);
                }
            }
            Self::Break => {}
            Self::Continue(process) => {
                process.types_at_spans(program, consume);
            }
            Self::Begin { body, .. } => {
                body.types_at_spans(program, consume);
            }
            Self::Loop(_, _, _) => {}
            Self::SendType(_, process) => {
                process.types_at_spans(program, consume);
            }
            Self::ReceiveType(_, process) => {
                process.types_at_spans(program, consume);
            }
        }
    }
}

impl Expression<()> {
    pub fn optimize(&self) -> Arc<Self> {
        match self {
            Self::Global(span, name, typ) => {
                Arc::new(Self::Global(span.clone(), name.clone(), typ.clone()))
            }
            Self::Variable(span, name, typ, usage) => Arc::new(Self::Variable(
                span.clone(),
                name.clone(),
                typ.clone(),
                usage.clone(),
            )),
            Self::Box(span, caps, expression, typ) => Arc::new(Self::Box(
                span.clone(),
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

#[derive(Clone, Debug)]
pub struct NameWithType {
    pub name: Option<String>,
    pub typ: Type,
    pub def_span: Span,
    pub decl_span: Span,
}

impl NameWithType {
    pub fn new(name: Option<String>, typ: Type) -> Self {
        NameWithType {
            name,
            typ,
            def_span: Span::None,
            decl_span: Span::None,
        }
    }
    pub fn named(name: impl ToString, typ: Type) -> Self {
        Self::new(Some(name.to_string()), typ)
    }
    pub fn unnamed(typ: Type) -> Self {
        Self::new(None, typ)
    }
}

impl Expression<Type> {
    pub fn types_at_spans(
        &self,
        program: &CheckedModule,
        consume: &mut impl FnMut(Span, NameWithType),
    ) {
        match self {
            Self::Global(_, name, typ) => {
                let def_span = (program.definitions.get(name))
                    .map(|(def, _typ)| def.name.span())
                    .unwrap_or_default();
                let decl_span = (program.declarations.get(name))
                    .map(|decl| decl.name.span())
                    .unwrap_or_else(|| def_span.clone());
                consume(
                    name.span(),
                    NameWithType {
                        name: Some(name.to_string()),
                        typ: typ.clone(),
                        def_span,
                        decl_span,
                    },
                );
            }
            Self::Variable(_, name, typ, _usage) => {
                consume(name.span(), NameWithType::named(name, typ.clone()));
            }
            Self::Box(span, _, expression, typ) => {
                consume(span.clone(), NameWithType::unnamed(typ.clone()));
                expression.types_at_spans(program, consume);
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
                    NameWithType::named(chan_name, chan_type.clone()),
                );
                if let Some(chan_annotation) = chan_annotation {
                    chan_annotation.types_at_spans(&program.type_defs, consume);
                }
                process.types_at_spans(program, consume);
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
            Self::Variable(_, _, typ, _usage) => typ.clone(),
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
                usage: _,
                typ: (),
                command,
            } => command.qualify(module),
            Self::Telltypes(_span, process) => process.qualify(module),
            Self::Block(_, _, body, process) => {
                body.qualify(module);
                process.qualify(module);
            }
            Self::Goto(_, _, _) => {}
            Self::Unreachable(_) => {}
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
            Self::Receive(_, annotation, (), process, _) => {
                if let Some(annotation) = annotation {
                    annotation.qualify(module);
                }
                process.qualify(module);
            }
            Self::Signal(_, process) => {
                process.qualify(module);
            }
            Self::Case(_, branches, else_process) => {
                for process in branches {
                    process.qualify(module);
                }
                if let Some(process) = else_process {
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
            Self::Variable(_span, _name, (), _usage) => {}
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
    pub fn free_variables(&self) -> IndexSet<LocalName> {
        match self {
            Process::Let {
                name, value, then, ..
            } => {
                let mut vars = then.free_variables();
                vars.shift_remove(name);
                vars.extend(value.free_variables());
                vars
            }
            Process::Do { name, command, .. } => {
                let mut vars = command.free_variables();
                vars.insert(name.clone());
                vars
            }
            Process::Telltypes(_, process) => process.free_variables(),
            Process::Block(_, _, _body, process) => process.free_variables(),
            Process::Goto(_, _, caps) => caps.names.keys().cloned().collect(),
            Process::Unreachable(_) => IndexSet::new(),
        }
    }

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

            Self::Unreachable(_) => {
                indentation(f, indent)?;
                write!(f, "unreachable")
            }

            Self::Do {
                span: _,
                name: subject,
                usage: _,
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

                    Command::Receive(parameter, _, _, process, vars) => {
                        if !vars.is_empty() {
                            write!(f, "<")?;
                            write!(f, "{}", vars[0])?;
                            for var in &vars[1..] {
                                write!(f, ", {}", var)?;
                            }
                            write!(f, ">")?;
                        }
                        write!(f, "[{}]", parameter)?;
                        process.pretty(f, indent)
                    }

                    Command::Signal(chosen, process) => {
                        write!(f, ".{}", chosen)?;
                        process.pretty(f, indent)
                    }

                    Command::Case(choices, branches, else_process) => {
                        write!(f, ".case {{")?;
                        for (choice, process) in choices.iter().zip(branches.iter()) {
                            indentation(f, indent + 1)?;
                            write!(f, ".{} => {{", choice)?;
                            process.pretty(f, indent + 2)?;
                            indentation(f, indent + 1)?;
                            write!(f, "}}")?;
                        }
                        if let Some(process) = else_process {
                            indentation(f, indent + 1)?;
                            write!(f, "else => {{")?;
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

            Self::Block(_, index, body, process) => {
                indentation(f, indent)?;
                write!(f, "block@{} {{", index)?;
                body.pretty(f, indent + 1)?;
                indentation(f, indent)?;
                write!(f, "}}")?;
                process.pretty(f, indent)
            }

            Self::Goto(_, index, _) => {
                indentation(f, indent)?;
                write!(f, "goto@{}", index)
            }
        }
    }
}

impl<Typ> Expression<Typ> {
    pub fn free_variables(&self) -> IndexSet<LocalName> {
        match self {
            Expression::Global(_, _, _) => IndexSet::new(),
            Expression::Variable(_, name, _, _) => {
                let mut set = IndexSet::new();
                set.insert(name.clone());
                set
            }
            Expression::Box(_, _, expression, _) => expression.free_variables(),
            Expression::Chan { captures, .. } => captures.names.keys().cloned().collect(),
            Expression::Primitive(_, _, _) => IndexSet::new(),
            Expression::External(_, _, _) => IndexSet::new(),
        }
    }

    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Global(_, name, _) => {
                write!(f, "{}", name)
            }

            Self::Variable(_, name, _, _) => {
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
