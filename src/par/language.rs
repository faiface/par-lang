// why not rename this file to ast.rs?

use std::{fmt::Display, hash::Hash, sync::Arc};

use indexmap::IndexMap;
use crate::location::{Point, Span, Spanning};
use super::{
    process::{self, Captures},
    types::Type,
};

#[derive(Clone, Debug)]
pub struct Name {
    pub span: Span,
    pub string: String,
}

#[derive(Clone, Debug)]
pub struct Program<Name, Expr> {
    pub type_defs: Vec<TypeDef<Name>>,
    pub declarations: Vec<Declaration<Name>>,
    pub definitions: Vec<Definition<Name, Expr>>,
}

#[derive(Clone, Debug)]
pub struct TypeDef<Name> {
    pub span: Span,
    pub name: Name,
    pub params: Vec<Name>,
    pub typ: Type<Name>,
}

#[derive(Clone, Debug)]
pub struct Declaration<Name> {
    pub span: Span,
    pub name: Name,
    pub typ: Type<Name>,
}

#[derive(Clone, Debug)]
pub struct Definition<Name, Expr> {
    pub span: Span,
    pub name: Name,
    pub expression: Expr,
}

#[derive(Clone, Debug)]
pub enum Pattern<Name> {
    Name(Span, Name, Option<Type<Name>>),
    Receive(Span, Vec<Self>, Box<Self>),
    Continue(Span),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Expression<Name> {
    Reference(Span, Name),
    Grouped(Span, Box<Self>),
    Let {
        span: Span,
        pattern: Pattern<Name>,
        expression: Box<Self>,
        then: Box<Self>,
    },
    Do {
        span: Span,
        process: Box<Process<Name>>,
        then: Box<Self>,
    },
    Fork {
        span: Span,
        channel: Name,
        annotation: Option<Type<Name>>,
        process: Box<Process<Name>>,
    },
    Construction(Construct<Name>),
    Application(Span, Box<Self>, Apply<Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Name> {
    /// wraps an expression
    Then(Box<Expression<Name>>),
    Send(Span, Vec<Expression<Name>>, Box<Self>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    /// constructs an either type
    Choose(Span, Name, Box<Self>),
    /// constructs a choice type
    Either(Span, ConstructBranches<Name>),
    /// ! (unit)
    Break(Span),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendTypes(Span, Vec<Type<Name>>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Name>(pub IndexMap<Name, ConstructBranch<Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Name> {
    Then(Span, Expression<Name>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply<Name> {
    Noop(Point),
    Send(Span, Vec<Expression<Name>>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(Span, ApplyBranches<Name>),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendTypes(Span, Vec<Type<Name>>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches<Name>(pub IndexMap<Name, ApplyBranch<Name>>);

#[derive(Clone, Debug)]
pub enum ApplyBranch<Name> {
    Then(Span, Name, Expression<Name>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    Continue(Span, Expression<Name>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

// span doesn't include the "then" process
#[derive(Clone, Debug)]
pub enum Process<Name> {
    Let { span: Span, pattern: Pattern<Name>, value: Box<Expression<Name>>, then: Box<Self> },
    Command(Name, Command<Name>),
    Telltypes(Span, Box<Self>),
    Noop(Point),
}

#[derive(Clone, Debug)]
pub enum Command<Name> {
    Then(Box<Process<Name>>),
    Link(Span, Box<Expression<Name>>),
    Send(Span, Vec<Expression<Name>>, Box<Self>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(
        Span,
        CommandBranches<Name>,
        Option<Box<Process<Name>>>,
    ),
    Break(Span),
    Continue(Span, Box<Process<Name>>),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendTypes(Span, Vec<Type<Name>>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches<Name>(pub IndexMap<Name, CommandBranch<Name>>);

#[derive(Clone, Debug)]
pub enum CommandBranch<Name> {
    Then(Span, Process<Name>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    Continue(Span, Process<Name>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Internal<Name> {
    Original(Name),
    Result(Option<Name>),
    Object(Option<Name>),
    Match(usize),
}

impl Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for Name {}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl Internal<Name> {
    pub fn span(&self) -> Option<Span> {
        match self {
            | Self::Original(name)
            | Self::Result(Some(name))
            | Self::Object(Some(name))
            => Some(name.span.clone()),

            _ => None,
        }
    }
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

impl<Name, Expr> Default for Program<Name, Expr> {
    fn default() -> Self {
        Self {
            type_defs: Vec::new(),
            declarations: Vec::new(),
            definitions: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileError {
    MustEndProcess(Span),
}

impl Spanning for CompileError {
    fn span(&self) -> Span {
        match self {
            CompileError::MustEndProcess(span) => span.clone(),
        }
    }
}

impl CompileError {
    pub fn message(&self) -> &str {
        match self {
            CompileError::MustEndProcess(_) => "process must end",
        }
    }
}

type Pass<Name> = Option<Arc<process::Process<Span, Internal<Name>, ()>>>;

impl<Name: Clone + Hash + Eq> Pattern<Name> {
    pub fn compile_let(
        &self,
        loc: &Span,
        expression: Arc<process::Expression<Internal<Name>, ()>>,
        process: Arc<process::Process<Span, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Span, Internal<Name>, ()>> {
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
        loc: &Span,
        subject: &Internal<Name>,
        process: Arc<process::Process<Span, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Span, Internal<Name>, ()>> {
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
        process: Arc<process::Process<Span, Internal<Name>, ()>>,
    ) -> Arc<process::Process<Span, Internal<Name>, ()>> {
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

            Self::Receive(loc, patterns, rest) => {
                patterns.into_iter().rfold(
                    rest.compile_helper(level, process),
                    |rest, pattern| {
                        pattern.compile_receive(level + 1, loc, &Internal::Match(level), rest)
                    },
                )
            },

            Self::Continue(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Match(level),
                (),
                process::Command::Continue(process),
            )),

            Self::ReceiveTypes(loc, params, rest) => {
                params.into_iter().rfold(
                    rest.compile_helper(level, process),
                    |process, param| Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Match(level),
                        (),
                        process::Command::ReceiveType(
                            Internal::Original(param.clone()),
                            rest.compile_helper(level, process),
                        ),
                    ))
                )
            },
        }
    }

    fn annotation(&self) -> Option<Type<Internal<Name>>> {
        match self {
            Self::Name(_, _, annotation) => original(annotation),
            Self::Receive(loc, patterns, rest) => {
                let send = patterns.into_iter()
                    .map(|pattern| pattern.annotation())
                    .collect::<Option<Vec<_>>>()?;
                let rest = rest.annotation()?;
                Some(Type::Send(loc.clone(), send, Box::new(rest)))
            }
            Self::Continue(loc) => Some(Type::Break(loc.clone())),
            Self::ReceiveTypes(loc, params, rest) => {
                let rest = rest.annotation()?;
                Some(Type::SendTypes(
                    loc.clone(),
                    params.into_iter().map(Name::clone).map(Internal::Original).collect(),
                    Box::new(rest),
                ))
            }
        }
    }
}

impl<Name> Spanning for Pattern<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Name(span, _, _)
            | Self::Continue(span)
            | Self::Receive(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Expression<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Expression<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Reference(loc, name) => Arc::new(process::Expression::Reference(
                loc.clone(),
                Internal::Original(name.clone()),
                (),
            )),

            Self::Grouped(_, expression) => expression.compile()?,

            Self::Let { span, pattern, expression, then: body} => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: pattern.compile_let(
                        span,
                        expression,
                        Arc::new(process::Process::Do(
                            span.clone(),
                            Internal::Result(None),
                            (),
                            process::Command::Link(body),
                        )),
                    ),
                })
            }

            Self::Do { span, process, then: expression } => {
                let expression = expression.compile()?;
                let body = process.compile(Some(Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))))?;
                Arc::new(process::Expression::Fork {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: body,
                })
            }

            Self::Fork { span, channel, annotation, process} => Arc::new(process::Expression::Fork {
                span: span.clone(),
                captures: Captures::new(),
                chan_name: Internal::Original(channel.clone()),
                chan_annotation: original(annotation),
                chan_type: (),
                expr_type: (),
                process: process.compile(None)?,
            }),

            Self::Construction(construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork {
                    span: construct.span().clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process,
                })
            }

            Self::Application(_, expr, Apply::Noop(_)) => expr.compile()?,

            Self::Application(loc, expr, apply) => {
                let expr = expr.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork {
                    span: loc.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: Arc::new(process::Process::Let(
                        loc.clone(),
                        Internal::Object(None),
                        None,
                        (),
                        expr,
                        process,
                    )),
                })
            }
        })
    }
}

impl<Name> Spanning for Expression<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Reference(span, _)
            | Self::Grouped(span, _)
            | Self::Let { span, .. }
            | Self::Do { span, .. }
            | Self::Fork { span, .. }
            | Self::Application(span, _, _)
            => span.clone(),

            Self::Construction(construction) => construction.span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Construct<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(expression) => {
                let span = expression.span().clone();
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span,
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(span, args, construct) => {
                let mut process = construct.compile()?;
                for arg in args.into_iter().rev() {
                    let arg = arg.compile()?;
                    process = Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Send(arg, process),
                    ));
                }
                process
            }

            Self::Receive(span, patterns, construct) => {
                patterns.into_iter().rfold(
                    construct.compile()?,
                    |process, pattern| {
                        pattern.compile_receive(0, span, &Internal::Result(None), process)
                    }
                )
            }

            Self::Choose(span, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, ConstructBranches(construct_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(construct_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    span.clone(),
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

            Self::Begin { span, unfounded, label, then: construct } => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Result(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Result(None),
                (),
                process::Command::Loop(Some(Internal::Result(label.clone()))),
            )),

            Self::SendTypes(span, args, construct) => {
                args.into_iter().rfold(
                    construct.compile()?,
                    |process, arg| Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::SendType(
                            arg.clone().map_names(&mut Internal::Original),
                            process
                        ),
                    ))
                )
            }

            Self::ReceiveTypes(span, params, construct) => {
                params.into_iter().rfold(
                    construct.compile()?,
                    |process, param| Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::ReceiveType(Internal::Original(param.clone()), process),
                    ))
                )
            }
        })
    }
}

impl<Name> Spanning for Construct<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Break(span)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendTypes(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),

            Self::Then(expression) => expression.span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> ConstructBranch<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
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

            Self::Receive(loc, patterns, branch) => {
                patterns.into_iter().rfold(
                    branch.compile()?,
                    |process, pattern| {
                        pattern.compile_receive(0, loc, &Internal::Result(None), process)
                    }
                )
            }

            Self::ReceiveTypes(loc, params, branch) => {
                params.into_iter().rfold(
                    branch.compile()?,
                    |process, param| Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::ReceiveType(Internal::Original(param.clone()), process),
                    ))
                )
            }
        })
    }
}

impl<Name> Spanning for ConstructBranch<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Then(span, _)
            | Self::Receive(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Apply<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Noop(point) => {
                let span = point.point_span();
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(Arc::new(process::Expression::Reference(
                        span.clone(),
                        Internal::Object(None),
                        (),
                    ))),
                ))
            },

            Self::Send(span, expressions, apply) => {
                let mut process = apply.compile()?;
                for expression in expressions.into_iter().rev() {
                    let expression = expression.compile()?;
                    process = Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Object(None),
                        (),
                        process::Command::Send(expression, process),
                    ));
                }
                process
            }

            Self::Choose(span, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, ApplyBranches(expression_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(expression_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Begin { span, unfounded, label, then: apply } => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Object(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Object(None),
                (),
                process::Command::Loop(Some(Internal::Object(label.clone()))),
            )),

            Self::SendTypes(span, args, apply) => {
                args.into_iter().rfold(
                    apply.compile()?,
                    |process, arg| Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Object(None),
                        (),
                        process::Command::SendType(
                            arg.clone().map_names(&mut Internal::Original),
                            process
                        ),
                    ))
                )
            }
        })
    }
}

impl Spanning for Apply<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Send(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendTypes(span, _, _)
            => span.clone(),

            Self::Noop(point) => point.point_span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> ApplyBranch<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(span, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let(
                    span.clone(),
                    Internal::Original(name.clone()),
                    None,
                    (),
                    Arc::new(process::Expression::Reference(
                        span.clone(),
                        Internal::Object(None),
                        (),
                    )),
                    Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    )),
                ))
            }

            Self::Receive(span, patterns, branch) => {
                patterns.into_iter().rfold(
                    branch.compile()?,
                    |process, pattern| {
                        pattern.compile_receive(0, span, &Internal::Object(None), process)
                    }
                )
            }

            Self::Continue(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Continue(Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    ))),
                ))
            }

            Self::ReceiveTypes(span, params, branch) => {
                params.into_iter().rfold(
                    branch.compile()?,
                    |process, param| Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Object(None),
                        (),
                        process::Command::ReceiveType(Internal::Original(param.clone()), process),
                    ))
                )
            }
        })
    }
}

impl<Name> Spanning for ApplyBranch<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Then(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Process<Name> {
    pub fn compile(
        &self,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Let { span, pattern, value, then } => {
                pattern.compile_let(span, value.compile()?, then.compile(pass)?)
            }

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Telltypes(loc, process) => Arc::new(process::Process::Telltypes(
                loc.clone(),
                process.compile(pass)?,
            )),

            Self::Noop(point) => match pass {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(point.point_span()))?,
            },
        })
    }
}

impl<Name> Spanning for Process<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Let { span, .. }
            | Self::Telltypes(span, _)
            => span.clone(),

            Self::Command(_, command) => command.span(),
            Self::Noop(point) => point.point_span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Command<Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Link(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(span, args, command) => {
                let mut process = command.compile(object_name, pass)?;
                for arg in args.into_iter().rev() {
                    let arg = arg.compile()?;
                    process = Arc::new(process::Process::Do(
                        span.clone(),
                        object_internal.clone(),
                        (),
                        process::Command::Send(arg, process),
                    ));
                }
                process
            }

            Self::Receive(span, patterns, command) => {
                patterns.into_iter().rfold(
                    command.compile(object_name, pass)?,
                    |process, pattern| {
                        pattern.compile_receive(0, span, &object_internal, process)
                    }
                )
            }

            Self::Choose(span, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, CommandBranches(process_branches), optional_process) => {
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
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(span) => Arc::new(process::Process::Do(
                span.clone(),
                object_internal,
                (),
                process::Command::Break,
            )),

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::Begin { span, unfounded, label, then: command} => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Begin(
                        *unfounded,
                        label.clone().map(Internal::Original),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                object_internal,
                (),
                process::Command::Loop(label.clone().map(Internal::Original)),
            )),

            Self::SendTypes(span, args, command) => {
                args.into_iter().rfold(
                    command.compile(object_name, pass)?,
                    |process, arg| Arc::new(process::Process::Do(
                        span.clone(),
                        object_internal.clone(),
                        (),
                        process::Command::SendType(
                            arg.clone().map_names(&mut Internal::Original),
                            process,
                        ),
                    ))
                )
            }

            Self::ReceiveTypes(span, params, command) => {
                params.into_iter().rfold(
                    command.compile(object_name, pass)?,
                    |process, param| Arc::new(process::Process::Do(
                        span.clone(),
                        object_internal.clone(),
                        (),
                        process::Command::ReceiveType(Internal::Original(param.clone()), process),
                    ))
                )
            }
        })
    }
}

impl<Name> Spanning for Command<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Link(span, _)
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _, _)
            | Self::Break(span)
            | Self::Continue(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendTypes(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),

            Self::Then(process) => process.span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> CommandBranch<Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Span, Internal<Name>, ()>>, CompileError> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(_, process) => process.compile(pass)?,

            Self::Receive(span, patterns, branch) => {
                patterns.into_iter().rfold(
                    branch.compile(object_name, pass)?,
                    |process, pattern| {
                        pattern.compile_receive(0, span, &object_internal, process)
                    }
                )
            }

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::ReceiveTypes(span, params, branch) => {
                params.into_iter().rfold(
                    branch.compile(object_name, pass)?,
                    |process, param| Arc::new(process::Process::Do(
                        span.clone(),
                        object_internal.clone(),
                        (),
                        process::Command::ReceiveType(Internal::Original(param.clone()), process),
                    ))
                )
            }
        })
    }
}

impl<Name> Spanning for CommandBranch<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Then(span, _)
            | Self::Receive(span, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
}

fn original<Name: Clone + Eq + Hash>(
    annotation: &Option<Type<Name>>,
) -> Option<Type<Internal<Name>>> {
    annotation
        .clone()
        .map(|t| t.map_names(&mut Internal::Original))
}
