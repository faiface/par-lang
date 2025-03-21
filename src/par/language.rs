use std::{fmt::Display, hash::Hash, sync::Arc};

use indexmap::IndexMap;

use super::{
    process::{self, Captures},
    types::Type,
};

#[derive(Clone, Debug)]
pub enum Pattern<Loc, Name> {
    Name(Loc, Name, Option<Type<Loc, Name>>),
    Receive(Loc, Box<Self>, Box<Self>),
    Continue(Loc),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Expression<Loc, Name> {
    Reference(Loc, Name),
    Let(Loc, Pattern<Loc, Name>, Box<Self>, Box<Self>),
    Do(Loc, Box<Process<Loc, Name>>, Box<Self>),
    Fork(Loc, Name, Option<Type<Loc, Name>>, Box<Process<Loc, Name>>),
    Construction(Loc, Construct<Loc, Name>),
    Application(Loc, Box<Self>, Apply<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Loc, Name> {
    Then(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Pattern<Loc, Name>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ConstructBranches<Loc, Name>),
    Break(Loc),
    Begin(Loc, bool, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
    SendType(Loc, Type<Loc, Name>, Box<Self>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Loc, Name>(pub IndexMap<Name, ConstructBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Loc, Name> {
    Then(Loc, Expression<Loc, Name>),
    Receive(Loc, Pattern<Loc, Name>, Box<Self>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply<Loc, Name> {
    Noop(Loc),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ApplyBranches<Loc, Name>),
    Begin(Loc, bool, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
    SendType(Loc, Type<Loc, Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches<Loc, Name>(pub IndexMap<Name, ApplyBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ApplyBranch<Loc, Name> {
    Then(Loc, Name, Expression<Loc, Name>),
    Receive(Loc, Pattern<Loc, Name>, Box<Self>),
    Continue(Loc, Expression<Loc, Name>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Process<Loc, Name> {
    Let(
        Loc,
        Pattern<Loc, Name>,
        Box<Expression<Loc, Name>>,
        Box<Self>,
    ),
    Command(Name, Command<Loc, Name>),
    Telltypes(Loc, Box<Self>),
    Noop(Loc),
}

#[derive(Clone, Debug)]
pub enum Command<Loc, Name> {
    Then(Box<Process<Loc, Name>>),
    Link(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Pattern<Loc, Name>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(
        Loc,
        CommandBranches<Loc, Name>,
        Option<Box<Process<Loc, Name>>>,
    ),
    Break(Loc),
    Continue(Loc, Box<Process<Loc, Name>>),
    Begin(Loc, bool, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
    SendType(Loc, Type<Loc, Name>, Box<Self>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches<Loc, Name>(pub IndexMap<Name, CommandBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum CommandBranch<Loc, Name> {
    Then(Process<Loc, Name>),
    Receive(Loc, Pattern<Loc, Name>, Box<Self>),
    Continue(Loc, Process<Loc, Name>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Internal<Name> {
    Original(Name),
    Result(Option<Name>),
    Object(Option<Name>),
    Match(usize),
}

impl<Name: From<String>> From<String> for Internal<Name> {
    fn from(value: String) -> Self {
        Self::Original(Name::from(value))
    }
}

impl<Name: Display> Display for Internal<Name> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Original(name) => write!(f, "{}", name),
            Self::Result(name) => {
                if let Some(name) = name {
                    write!(f, "{}", name)?;
                }
                write!(f, "#result")
            }
            Self::Object(name) => {
                if let Some(name) = name {
                    write!(f, "{}", name)?;
                }
                write!(f, "#object")
            }
            Self::Match(level) => write!(f, "#match{}", level),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileError<Loc> {
    PassNotPossible(Loc),
    MustEndProcess(Loc),
    CannotEndInDoExpression(Loc),
}

type Pass<Loc, Name> = Option<Arc<process::Process<Loc, Internal<Name>, ()>>>;

impl<Loc: Clone, Name: Clone + Hash + Eq> Pattern<Loc, Name> {
    pub fn compile_let(
        &self,
        loc: &Loc,
        expression: Arc<process::Expression<Loc, Internal<Name>, ()>>,
        process: Arc<process::Process<Loc, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Loc, Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Let(
                loc.clone(),
                Internal::Original(name.clone()),
                original(annotation),
                (),
                expression,
                process,
            ));
        }
        Arc::new(process::Process::Let(
            loc.clone(),
            Internal::Match(0),
            self.annotation(),
            (),
            expression,
            self.compile_helper(0, process),
        ))
    }

    pub fn compile_receive(
        &self,
        level: usize,
        loc: &Loc,
        subject: &Internal<Name>,
        process: Arc<process::Process<Loc, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Loc, Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Do(
                loc.clone(),
                subject.clone(),
                (),
                process::Command::Receive(
                    Internal::Original(name.clone()),
                    original(annotation),
                    process,
                ),
            ));
        }
        Arc::new(process::Process::Do(
            loc.clone(),
            subject.clone(),
            (),
            process::Command::Receive(
                Internal::Match(level),
                self.annotation(),
                self.compile_helper(level, process),
            ),
        ))
    }

    fn compile_helper(
        &self,
        level: usize,
        process: Arc<process::Process<Loc, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Loc, Internal<Name>, ()>> {
        match self {
            Self::Name(loc, name, annotation) => Arc::new(process::Process::Let(
                loc.clone(),
                Internal::Original(name.clone()),
                original(annotation),
                (),
                Arc::new(process::Expression::Reference(
                    loc.clone(),
                    Internal::Match(level),
                    (),
                )),
                process,
            )),

            Self::Receive(loc, first, rest) => first.compile_receive(
                level + 1,
                loc,
                &Internal::Match(level),
                rest.compile_helper(level, process),
            ),

            Self::Continue(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Match(level),
                (),
                process::Command::Continue(process),
            )),

            Self::ReceiveType(loc, parameter, rest) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Match(level),
                (),
                process::Command::ReceiveType(
                    Internal::Original(parameter.clone()),
                    rest.compile_helper(level, process),
                ),
            )),
        }
    }

    fn annotation(&self) -> Option<Type<Loc, Internal<Name>>> {
        match self {
            Self::Name(_, _, annotation) => original(annotation),
            Self::Receive(loc, first, rest) => {
                let first = first.annotation()?;
                let rest = rest.annotation()?;
                Some(Type::Send(loc.clone(), Box::new(first), Box::new(rest)))
            }
            Self::Continue(loc) => Some(Type::Break(loc.clone())),
            Self::ReceiveType(loc, parameter, rest) => {
                let rest = rest.annotation()?;
                Some(Type::SendType(
                    loc.clone(),
                    Internal::Original(parameter.clone()),
                    Box::new(rest),
                ))
            }
        }
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Expression<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Expression<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Reference(loc, name) => Arc::new(process::Expression::Reference(
                loc.clone(),
                Internal::Original(name.clone()),
                (),
            )),

            Self::Let(loc, pattern, expression, body) => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    pattern.compile_let(
                        loc,
                        expression,
                        Arc::new(process::Process::Do(
                            loc.clone(),
                            Internal::Result(None),
                            (),
                            process::Command::Link(body),
                        )),
                    ),
                ))
            }

            Self::Do(loc, process, expression) => {
                let expression = expression.compile()?;
                let body = process.compile(Some(Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))))?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    body,
                ))
            }

            Self::Fork(loc, channel, annotation, process) => Arc::new(process::Expression::Fork(
                loc.clone(),
                Captures::new(),
                Internal::Original(channel.clone()),
                original(annotation),
                (),
                process.compile(None)?,
            )),

            Self::Construction(loc, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    process,
                ))
            }

            Self::Application(_, expr, Apply::Noop(_)) => expr.compile()?,

            Self::Application(loc, expr, apply) => {
                let expr = expr.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    Arc::new(process::Process::Let(
                        loc.clone(),
                        Internal::Object(None),
                        None,
                        (),
                        expr,
                        process,
                    )),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Construct<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, construct) => {
                let argument = argument.compile()?;
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, pattern, construct) => {
                let process = construct.compile()?;
                pattern.compile_receive(0, loc, &Internal::Result(None), process)
            }

            Self::Choose(loc, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, ConstructBranches(construct_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(construct_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Break,
            )),

            Self::Begin(loc, unfounded, label, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Result(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Loop(Some(Internal::Result(label.clone()))),
            )),

            Self::SendType(loc, argument, construct) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::SendType(argument, process),
                ))
            }

            Self::ReceiveType(loc, parameter, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ConstructBranch<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, loc, &Internal::Result(None), process)
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Apply<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Noop(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Link(Arc::new(process::Expression::Reference(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                ))),
            )),

            Self::Send(loc, expression, apply) => {
                let expression = expression.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Send(expression, process),
                ))
            }

            Self::Choose(loc, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, ApplyBranches(expression_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(expression_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Begin(loc, unfounded, label, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Object(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Object(None),
                (),
                process::Command::Loop(Some(Internal::Object(label.clone()))),
            )),

            Self::SendType(loc, argument, apply) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::SendType(argument, process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ApplyBranch<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let(
                    loc.clone(),
                    Internal::Original(name.clone()),
                    None,
                    (),
                    Arc::new(process::Expression::Reference(
                        loc.clone(),
                        Internal::Object(None),
                        (),
                    )),
                    Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    )),
                ))
            }

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, loc, &Internal::Object(None), process)
            }

            Self::Continue(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Continue(Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    ))),
                ))
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Process<Loc, Name> {
    pub fn compile(
        &self,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Let(loc, pattern, expression, process) => {
                pattern.compile_let(loc, expression.compile()?, process.compile(pass)?)
            }

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Telltypes(loc, process) => Arc::new(process::Process::Telltypes(
                loc.clone(),
                process.compile(pass)?,
            )),

            Self::Noop(loc) => match pass {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(loc.clone()))?,
            },
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Command<Loc, Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Link(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, command) => {
                let argument = argument.compile()?;
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, pattern, command) => {
                let process = command.compile(object_name, pass)?;
                pattern.compile_receive(0, loc, &object_internal, process)
            }

            Self::Choose(loc, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, CommandBranches(process_branches), optional_process) => {
                let pass = match optional_process {
                    Some(process) => Some(process.compile(pass)?),
                    None => pass,
                };

                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, process_branch) in process_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(process_branch.compile(object_name, pass.clone())?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                object_internal,
                (),
                process::Command::Break,
            )),

            Self::Continue(loc, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::Begin(loc, unfounded, label, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Begin(
                        *unfounded,
                        label.clone().map(Internal::Original),
                        process,
                    ),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                object_internal,
                (),
                process::Command::Loop(label.clone().map(Internal::Original)),
            )),

            Self::SendType(loc, argument, command) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::SendType(argument, process),
                ))
            }

            Self::ReceiveType(loc, parameter, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> CommandBranch<Loc, Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile(object_name, pass)?;
                pattern.compile_receive(0, loc, &object_internal, process)
            }

            Self::Continue(loc, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

fn original<Loc: Clone, Name: Clone + Eq + Hash>(
    annotation: &Option<Type<Loc, Name>>,
) -> Option<Type<Loc, Internal<Name>>> {
    annotation
        .clone()
        .map(|t| t.map_names(&mut Internal::Original))
}
