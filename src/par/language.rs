// why not rename this file to ast.rs?

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    hash::Hash,
    sync::Arc,
};

use arcstr::{literal, ArcStr};

use super::{
    primitive::Primitive,
    process::{self, Captures},
    types::Type,
};
use crate::par::process::VariableUsage;
use crate::{
    location::{Span, Spanning},
    par::types::error::labels_from_span,
};

#[derive(Clone, Debug)]
pub struct LocalName {
    pub span: Span,
    pub string: ArcStr,
}

#[derive(Clone, Debug)]
pub struct GlobalName {
    pub span: Span,
    pub module: Option<String>,
    pub primary: String,
}

impl GlobalName {
    pub fn external(module: Option<&'static str>, primary: &'static str) -> Self {
        GlobalName {
            span: Default::default(),
            module: module.map(String::from),
            primary: String::from(primary),
        }
    }
}

impl From<ArcStr> for LocalName {
    fn from(value: ArcStr) -> Self {
        LocalName {
            span: Span::None,
            string: value,
        }
    }
}

impl Spanning for LocalName {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanning for GlobalName {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl LocalName {
    pub fn result() -> Self {
        Self {
            span: Span::None,
            string: literal!("#result"),
        }
    }

    pub fn object() -> Self {
        Self {
            span: Span::None,
            string: literal!("#object"),
        }
    }

    pub fn error() -> Self {
        Self {
            span: Span::None,
            string: literal!("#error"),
        }
    }

    pub fn match_(level: usize) -> Self {
        Self {
            span: Span::None,
            string: arcstr::format!("#match{}", level),
        }
    }

    pub fn temp() -> Self {
        Self {
            span: Span::None,
            string: literal!("#temp"),
        }
    }

    pub fn invalid() -> Self {
        Self {
            span: Span::None,
            string: literal!("#invalid"),
        }
    }

    /// Check if this is an internal pattern matching variable.
    pub fn is_match(&self) -> bool {
        self.string.starts_with("#match")
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Name(Span, LocalName, Option<Type>),
    Receive(Span, Box<Self>, Box<Self>, Vec<LocalName>),
    Continue(Span),
    ReceiveType(Span, LocalName, Box<Self>),
    Try(Span, Option<LocalName>, Box<Self>),
    Default(Span, Box<Expression>, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Primitive(Span, Primitive),
    List(Span, Vec<Self>),
    Global(Span, GlobalName),
    Variable(Span, LocalName),
    Grouped(Span, Box<Self>),
    Let {
        span: Span,
        pattern: Pattern,
        expression: Box<Self>,
        then: Box<Self>,
    },
    Catch {
        span: Span,
        label: Option<LocalName>,
        pattern: Pattern,
        block: Box<Self>,
        then: Box<Self>,
    },
    Throw(Span, Option<LocalName>, Box<Self>),
    Do {
        span: Span,
        process: Box<Process>,
        then: Box<Self>,
    },
    Box(Span, Box<Self>),
    Chan {
        span: Span,
        pattern: Pattern,
        process: Box<Process>,
    },
    Construction(Construct),
    Application(Span, Box<Self>, Apply),
}

#[derive(Clone, Debug)]
pub enum Construct {
    /// wraps an expression
    Then(Box<Expression>),
    Send(Span, Box<Expression>, Box<Self>),
    Receive(Span, Pattern, Box<Self>, Vec<LocalName>),
    /// constructs an either type
    Signal(Span, LocalName, Box<Self>),
    /// constructs a choice type
    Case(Span, ConstructBranches, Option<Box<ConstructBranch>>),
    /// ! (unit)
    Break(Span),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<LocalName>,
        then: Box<Self>,
    },
    Loop(Span, Option<LocalName>),
    SendType(Span, Type, Box<Self>),
    ReceiveType(Span, LocalName, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches(pub BTreeMap<LocalName, ConstructBranch>);

#[derive(Clone, Debug)]
pub enum ConstructBranch {
    Then(Span, Expression),
    Receive(Span, Pattern, Box<Self>, Vec<LocalName>),
    ReceiveType(Span, LocalName, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply {
    Noop(Span),
    Send(Span, Box<Expression>, Box<Self>),
    Signal(Span, LocalName, Box<Self>),
    Case(Span, ApplyBranches, Option<Box<ApplyBranch>>),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<LocalName>,
        then: Box<Self>,
    },
    Loop(Span, Option<LocalName>),
    SendType(Span, Type, Box<Self>),
    Try(Span, Option<LocalName>, Box<Self>),
    Default(Span, Box<Expression>, Box<Self>),
    Pipe(Span, Box<Expression>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches(pub BTreeMap<LocalName, ApplyBranch>);

#[derive(Clone, Debug)]
pub enum ApplyBranch {
    Then(Span, LocalName, Expression),
    Receive(Span, Pattern, Box<Self>, Vec<LocalName>),
    Continue(Span, Expression),
    ReceiveType(Span, LocalName, Box<Self>),
    Try(Span, Option<LocalName>, Box<Self>),
    Default(Span, Box<Expression>, Box<Self>),
}

// span doesn't include the "then" process
#[derive(Clone, Debug)]
pub enum Process {
    Let {
        span: Span,
        pattern: Pattern,
        value: Box<Expression>,
        then: Box<Self>,
    },
    Catch {
        span: Span,
        label: Option<LocalName>,
        pattern: Pattern,
        block: Box<Self>,
        then: Box<Self>,
    },
    Throw(Span, Option<LocalName>, Box<Expression>),
    GlobalCommand(GlobalName, Command),
    Command(LocalName, Command),
    Telltypes(Span, Box<Self>),
    Noop(Span),
}

#[derive(Clone, Debug)]
pub enum Command {
    Then(Box<Process>),
    Link(Span, Box<Expression>),
    Send(Span, Expression, Box<Self>),
    Receive(Span, Pattern, Box<Self>, Vec<LocalName>),
    Signal(Span, LocalName, Box<Self>),
    Case(
        Span,
        CommandBranches,
        Option<Box<CommandBranch>>,
        Option<Box<Process>>,
    ),
    Break(Span),
    Continue(Span, Box<Process>),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<LocalName>,
        then: Box<Self>,
    },
    Loop(Span, Option<LocalName>),
    SendType(Span, Type, Box<Self>),
    ReceiveType(Span, LocalName, Box<Self>),
    Try(Span, Option<LocalName>, Box<Self>),
    Default(Span, Box<Expression>, Box<Self>),
    Pipe(Span, Box<Expression>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches(pub BTreeMap<LocalName, CommandBranch>);

#[derive(Clone, Debug)]
pub enum CommandBranch {
    Then(Span, Process),
    BindThen(Span, LocalName, Process),
    Receive(Span, Pattern, Box<Self>, Vec<LocalName>),
    Continue(Span, Process),
    ReceiveType(Span, LocalName, Box<Self>),
    Try(Span, Option<LocalName>, Box<Self>),
    Default(Span, Box<Expression>, Box<Self>),
}

impl Hash for LocalName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}
impl PartialEq for LocalName {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}
impl Eq for LocalName {}
impl PartialOrd for LocalName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.string.partial_cmp(&other.string)
    }
}
impl Ord for LocalName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.string.cmp(&other.string)
    }
}
impl Display for LocalName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl GlobalName {
    pub fn qualify(&mut self, module: Option<&str>) {
        self.module = match self.module.take() {
            Some(old) => Some(old),
            None => module.map(String::from),
        };
    }

    fn no_module_or_same_as_primary(&self) -> bool {
        if let Some(module) = &self.module {
            module == &self.primary
        } else {
            true
        }
    }
}

impl Hash for GlobalName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if !self.no_module_or_same_as_primary() {
            self.module.hash(state);
        }
        self.primary.hash(state);
    }
}
impl PartialEq for GlobalName {
    fn eq(&self, other: &Self) -> bool {
        if self.no_module_or_same_as_primary() && other.no_module_or_same_as_primary() {
            self.primary == other.primary
        } else {
            (&self.module, &self.primary) == (&other.module, &other.primary)
        }
    }
}
impl Eq for GlobalName {}
impl PartialOrd for GlobalName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.no_module_or_same_as_primary() && other.no_module_or_same_as_primary() {
            self.primary.partial_cmp(&other.primary)
        } else {
            (&self.module, &self.primary).partial_cmp(&(&other.module, &other.primary))
        }
    }
}
impl Ord for GlobalName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.no_module_or_same_as_primary() && other.no_module_or_same_as_primary() {
            self.primary.cmp(&other.primary)
        } else {
            (&self.module, &self.primary).cmp(&(&other.module, &other.primary))
        }
    }
}
impl Display for GlobalName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.no_module_or_same_as_primary() {
            if let Some(module) = &self.module {
                write!(f, "{}.", module)?;
            }
        }
        write!(f, "{}", self.primary)
    }
}

#[derive(Clone, Debug)]
pub enum CompileError {
    MustEndProcess(Span),
    UnreachableCode(Span),
    NoMatchingCatch(Span),
    MatchingCatchDisabled(Span, CatchDisabledReason),
}

#[derive(Clone, Debug)]
pub enum CatchDisabledReason {
    DifferentProcess,
    ValuePartiallyConstructed,
}

impl Spanning for CompileError {
    fn span(&self) -> Span {
        match self {
            Self::MustEndProcess(span) => span.clone(),
            Self::UnreachableCode(span) => span.clone(),
            Self::NoMatchingCatch(span) => span.clone(),
            Self::MatchingCatchDisabled(span, _) => span.clone(),
        }
    }
}

impl CompileError {
    pub fn to_report(&self, source_code: Arc<str>) -> miette::Report {
        let mk_report = |span: &Span, msg: &'static str| {
            let labels = labels_from_span(&source_code, span);
            let code: Arc<str> = if labels.is_empty() {
                "<UI>".into()
            } else {
                Arc::clone(&source_code)
            };
            miette::miette! { labels = labels, "{}", msg }.with_source_code(code)
        };

        match self {
            Self::MustEndProcess(span) => mk_report(span, "This process must end."),
            Self::UnreachableCode(span) => mk_report(span, "Unreachable code."),
            Self::NoMatchingCatch(span) => mk_report(span, "No matching `catch` block defined."),
            Self::MatchingCatchDisabled(span, CatchDisabledReason::DifferentProcess) => {
                mk_report(span, "Matching `catch` is in a different process.")
            }
            Self::MatchingCatchDisabled(
                span,
                CatchDisabledReason::ValuePartiallyConstructed,
            ) => mk_report(
                span,
                "The expression the matching `catch` would return from has its result already partially constructed.",
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Passes {
    fallthrough: Option<Pass>,
    fallthrough_stash: Vec<Option<Pass>>,
    catch: HashMap<Option<LocalName>, Pass>,
    catch_stash: HashMap<Option<LocalName>, Vec<Option<Pass>>>,
}

#[derive(Clone, Debug)]
pub struct Pass {
    process: Arc<process::Process<()>>,
    used: bool,
    // Stack of reasons that currently disable this catch (LIFO).
    disabled_reasons: Vec<CatchDisabledReason>,
}

impl Passes {
    pub fn new() -> Self {
        Passes {
            fallthrough: None,
            fallthrough_stash: Vec::new(),
            catch: HashMap::new(),
            catch_stash: HashMap::new(),
        }
    }

    fn set_fallthrough(
        &mut self,
        p: Option<Arc<process::Process<()>>>,
    ) -> Result<&mut Self, CompileError> {
        self.fallthrough_stash.push(self.fallthrough.take());
        self.fallthrough = p.map(Pass::new);
        Ok(self)
    }

    fn use_fallthrough(&mut self) -> Option<Arc<process::Process<()>>> {
        match self.fallthrough.as_mut() {
            Some(pass) => {
                pass.used = true;
                Some(Arc::clone(&pass.process))
            }
            None => None,
        }
    }

    fn unset_fallthrough(&mut self) -> Result<&mut Self, CompileError> {
        if let Some(previous) = self.fallthrough.take() {
            if !previous.used {
                return Err(CompileError::UnreachableCode(previous.process.span()));
            }
        }
        self.fallthrough = self
            .fallthrough_stash
            .pop()
            .expect("fallthrough was supposed be stashed");
        Ok(self)
    }

    fn set_catch(
        &mut self,
        label: Option<LocalName>,
        p: Option<Arc<process::Process<()>>>,
    ) -> Result<&mut Self, CompileError> {
        self.catch_stash
            .entry(label.clone())
            .or_default()
            .push(self.catch.remove(&label));
        if let Some(p) = p {
            self.catch.insert(label, Pass::new(p));
        }
        Ok(self)
    }

    fn use_catch(
        &mut self,
        span: &Span,
        label: &Option<LocalName>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        match self.catch.get_mut(label) {
            Some(pass) => {
                if let Some(reason) = pass.disabled_reasons.last().cloned() {
                    return Err(CompileError::MatchingCatchDisabled(span.clone(), reason));
                }
                pass.used = true;
                Ok(Arc::clone(&pass.process))
            }
            None => Err(CompileError::NoMatchingCatch(span.clone())),
        }
    }

    fn unset_catch(&mut self, label: Option<LocalName>) -> Result<&mut Self, CompileError> {
        if let Some(previous) = self.catch.remove(&label) {
            if !previous.used {
                return Err(CompileError::UnreachableCode(previous.process.span()));
            }
        }
        let stashed = self
            .catch_stash
            .entry(label.clone())
            .or_default()
            .pop()
            .expect("catch was supposed be stashed");
        if let Some(stashed) = stashed {
            self.catch.insert(label, stashed);
        }
        Ok(self)
    }

    fn disable_catches(&mut self, reason: CatchDisabledReason) -> &mut Self {
        for pass in self.catch.values_mut() {
            pass.disabled_reasons.push(reason.clone());
        }
        self
    }

    fn enable_catches(&mut self) -> &mut Self {
        for pass in self.catch.values_mut() {
            if !pass.disabled_reasons.is_empty() {
                pass.disabled_reasons
                    .pop()
                    .expect("disabled reason must exist (well-parenthesized)");
            }
        }
        self
    }
}

impl Pass {
    fn new(p: Arc<process::Process<()>>) -> Self {
        Pass {
            process: p,
            used: false,
            disabled_reasons: Vec::new(),
        }
    }
}

impl Pattern {
    pub fn compile_let(
        &self,
        span: &Span,
        expression: Arc<process::Expression<()>>,
        process: Arc<process::Process<()>>,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Self::Name(_, name, annotation) = self {
            return Ok(Arc::new(process::Process::Let {
                span: span.clone(),
                name: name.clone(),
                annotation: annotation.clone(),
                typ: (),
                value: expression,
                then: process,
            }));
        }
        Ok(Arc::new(process::Process::Let {
            span: span.clone(),
            name: LocalName::match_(0),
            annotation: self.annotation(),
            typ: (),
            value: expression,
            then: self.compile_helper(0, process, pass)?,
        }))
    }

    pub fn compile_chan(
        &self,
        span: &Span,
        process: Arc<process::Process<()>>,
        pass: &mut Passes,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        if let Self::Name(_, name, annotation) = self {
            return Ok(Arc::new(process::Expression::Chan {
                span: span.clone(),
                captures: Captures::new(),
                chan_name: name.clone(),
                chan_annotation: annotation.clone(),
                chan_type: (),
                expr_type: (),
                process,
            }));
        }
        Ok(Arc::new(process::Expression::Chan {
            span: span.clone(),
            captures: Captures::new(),
            chan_name: LocalName::match_(0),
            chan_annotation: None,
            chan_type: (),
            expr_type: (),
            process: self.compile_helper(0, process, pass)?,
        }))
    }

    pub fn compile_catch_block(
        &self,
        span: &Span,
        block: Arc<process::Process<()>>,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Self::Name(_, name, annotation) = self {
            return Ok(Arc::new(process::Process::Let {
                span: span.clone(),
                name: name.clone(),
                annotation: annotation.clone(),
                typ: (),
                value: Arc::new(process::Expression::Variable(
                    span.clone(),
                    LocalName::error(),
                    (),
                    VariableUsage::Unknown,
                )),
                then: block,
            }));
        }
        Ok(Arc::new(process::Process::Let {
            span: span.clone(),
            name: LocalName::match_(0),
            annotation: None,
            typ: (),
            value: Arc::new(process::Expression::Variable(
                span.clone(),
                LocalName::error(),
                (),
                VariableUsage::Unknown,
            )),
            then: self.compile_helper(0, block, pass)?,
        }))
    }

    pub fn compile_receive(
        &self,
        level: usize,
        span: &Span,
        subject: &LocalName,
        process: Arc<process::Process<()>>,
        vars: Vec<LocalName>,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Self::Name(_, name, annotation) = self {
            return Ok(Arc::new(process::Process::Do {
                span: span.clone(),
                name: subject.clone(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Receive(
                    name.clone(),
                    annotation.clone(),
                    (),
                    process,
                    vars,
                ),
            }));
        }
        Ok(Arc::new(process::Process::Do {
            span: span.clone(),
            name: subject.clone(),
            usage: VariableUsage::Unknown,
            typ: (),
            command: process::Command::Receive(
                LocalName::match_(level),
                self.annotation(),
                (),
                self.compile_helper(level, process, pass)?,
                vars,
            ),
        }))
    }

    fn compile_helper(
        &self,
        level: usize,
        process: Arc<process::Process<()>>,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        match self {
            Self::Name(span, name, annotation) => Ok(Arc::new(process::Process::Let {
                span: span.clone(),
                name: name.clone(),
                annotation: annotation.clone(),
                typ: (),
                value: Arc::new(process::Expression::Variable(
                    span.clone(),
                    LocalName::match_(level),
                    (),
                    VariableUsage::Unknown,
                )),
                then: process,
            })),

            Self::Receive(span, first, rest, vars) => Ok(first.compile_receive(
                level + 1,
                span,
                &LocalName::match_(level),
                rest.compile_helper(level, process, pass)?,
                vars.clone(),
                pass,
            )?),

            Self::Continue(span) => Ok(Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::match_(level),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Continue(process),
            })),

            Self::ReceiveType(span, parameter, rest) => Ok(Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::match_(level),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::ReceiveType(
                    parameter.clone(),
                    rest.compile_helper(level, process, pass)?,
                ),
            })),

            Self::Try(span, label, rest) => {
                let catch_block = pass.use_catch(span, label)?;
                Ok(compile_try(
                    span,
                    LocalName::match_(level),
                    catch_block,
                    rest.compile_helper(level, process, pass)?,
                ))
            }

            Self::Default(span, expr, rest) => {
                let default_expr = expr.compile(pass)?;
                Ok(compile_default(
                    span,
                    LocalName::match_(level),
                    default_expr,
                    rest.compile_helper(level, process, pass)?,
                ))
            }
        }
    }

    fn annotation(&self) -> Option<Type> {
        match self {
            Self::Name(_, _, annotation) => annotation.clone(),
            Self::Receive(span, first, rest, vars) => {
                let first = first.annotation()?;
                let rest = rest.annotation()?;
                Some(Type::Pair(
                    span.clone(),
                    Box::new(first),
                    Box::new(rest),
                    vars.clone(),
                ))
            }
            Self::Continue(span) => Some(Type::Break(span.clone())),
            Self::ReceiveType(span, parameter, rest) => {
                let rest = rest.annotation()?;
                Some(Type::Exists(
                    span.clone(),
                    parameter.clone(),
                    Box::new(rest),
                ))
            }
            Self::Try(_, _, _) => None,
            Self::Default(_, _, rest) => rest.annotation(),
        }
    }
}

impl Spanning for Pattern {
    fn span(&self) -> Span {
        match self {
            Self::Name(span, _, _)
            | Self::Continue(span)
            | Self::Receive(span, _, _, _)
            | Self::ReceiveType(span, _, _)
            | Self::Try(span, _, _)
            | Self::Default(span, _, _) => span.clone(),
        }
    }
}

impl Expression {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Expression<()>>, CompileError> {
        Ok(match self {
            Self::Primitive(span, value) => Arc::new(process::Expression::Primitive(
                span.clone(),
                value.clone(),
                (),
            )),

            Self::List(span, items) => {
                let mut process = Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(
                        LocalName {
                            span: span.clone(),
                            string: literal!("end"),
                        },
                        Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: LocalName::result(),
                            usage: VariableUsage::Unknown,
                            typ: (),
                            command: process::Command::Break,
                        }),
                    ),
                });
                for item in items.iter().rev() {
                    let span = item.span();
                    process = Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: LocalName::result(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Signal(
                            LocalName {
                                span: span.clone(),
                                string: literal!("item"),
                            },
                            Arc::new(process::Process::Do {
                                span,
                                name: LocalName::result(),
                                usage: VariableUsage::Unknown,
                                typ: (),
                                command: process::Command::Send(item.compile(pass)?, process),
                            }),
                        ),
                    });
                }
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process,
                })
            }

            Self::Global(span, name) => {
                Arc::new(process::Expression::Global(span.clone(), name.clone(), ()))
            }

            Self::Variable(span, name) => Arc::new(process::Expression::Variable(
                span.clone(),
                name.clone(),
                (),
                VariableUsage::Unknown,
            )),

            Self::Grouped(_, expression) => expression.compile(pass)?,

            Self::Box(span, expression) => {
                let expression = expression.compile(pass)?;
                Arc::new(process::Expression::Box(
                    span.clone(),
                    Captures::new(),
                    expression,
                    (),
                ))
            }

            Self::Let {
                span,
                pattern,
                expression,
                then: body,
            } => {
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let expression = expression.compile(pass)?;
                pass.enable_catches();
                let body = body.compile(pass)?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: pattern.compile_let(
                        span,
                        expression,
                        Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: LocalName::result(),
                            usage: VariableUsage::Unknown,
                            typ: (),
                            command: process::Command::Link(body),
                        }),
                        pass,
                    )?,
                })
            }

            Self::Catch {
                span,
                label,
                pattern,
                block,
                then,
            } => {
                let block = Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(block.compile(pass)?),
                });
                let block = pattern.compile_catch_block(span, block, pass)?;
                pass.set_catch(label.clone(), Some(block))?;
                let expression = then.compile(pass)?;
                pass.unset_catch(label.clone())?;
                expression
            }

            Self::Throw(span, label, expression) => {
                let catch_block = pass.use_catch(span, label)?;
                pass.set_fallthrough(None)?;
                let expression = expression.compile(pass)?;
                pass.unset_fallthrough()?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: Arc::new(process::Process::Let {
                        span: span.clone(),
                        name: LocalName::error(),
                        annotation: None,
                        typ: (),
                        value: expression,
                        then: catch_block,
                    }),
                })
            }

            Self::Do {
                span,
                process,
                then: expression,
            } => {
                let expression = expression.compile(pass)?;
                pass.set_fallthrough(Some(Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })))?;
                let body = process.compile(pass)?;
                pass.unset_fallthrough()?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: body,
                })
            }

            Self::Chan {
                span,
                pattern,
                process,
            } => {
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let proc = process.compile(pass)?;
                pass.enable_catches();
                pattern.compile_chan(span, proc, pass)?
            }

            Self::Construction(construct) => {
                pass.disable_catches(CatchDisabledReason::ValuePartiallyConstructed);
                let process = construct.compile(pass)?;
                pass.enable_catches();
                Arc::new(process::Expression::Chan {
                    span: construct.span().clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process,
                })
            }

            Self::Application(_, expr, Apply::Noop(_)) => expr.compile(pass)?,

            Self::Application(span, expr, apply) => {
                let expr = expr.compile(pass)?;
                let process = apply.compile(pass)?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: Arc::new(process::Process::Let {
                        span: span.clone(),
                        name: LocalName::object(),
                        annotation: None,
                        typ: (),
                        value: expr,
                        then: process,
                    }),
                })
            }
        })
    }
}

impl Spanning for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(span, _)
            | Self::List(span, _)
            | Self::Global(span, _)
            | Self::Variable(span, _)
            | Self::Grouped(span, _)
            | Self::Let { span, .. }
            | Self::Catch { span, .. }
            | Self::Throw(span, _, _)
            | Self::Do { span, .. }
            | Self::Box(span, _)
            | Self::Chan { span, .. }
            | Self::Application(span, _, _) => span.clone(),

            Self::Construction(construction) => construction.span(),
        }
    }
}

impl Construct {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Then(expression) => {
                let span = expression.span().clone();
                let expression = expression.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span,
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Send(span, argument, construct) => {
                let argument = argument.compile(pass)?;
                let process = construct.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Self::Receive(span, pattern, construct, vars) => {
                let process = construct.compile(pass)?;
                pattern.compile_receive(
                    0,
                    span,
                    &LocalName::result(),
                    process,
                    vars.clone(),
                    pass,
                )?
            }

            Self::Signal(span, chosen, construct) => {
                let process = construct.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Self::Case(span, ConstructBranches(construct_branches), else_branch) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(branch_name.clone());
                    processes.push(construct_branch.compile(pass)?);
                }
                let else_process = match else_branch {
                    Some(branch) => Some(branch.compile(pass)?),
                    None => None,
                };
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Case(branches, processes, else_process),
                })
            }

            Self::Break(span) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::result(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Break,
            }),

            Self::Begin {
                span,
                unfounded,
                label,
                then: construct,
            } => {
                let process = construct.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone(),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::result(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Loop(
                    label.clone(),
                    LocalName::invalid(),
                    Captures::new(),
                ),
            }),

            Self::SendType(span, argument, construct) => {
                let process = construct.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Self::ReceiveType(span, parameter, construct) => {
                let process = construct.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }
        })
    }
}

impl Spanning for Construct {
    fn span(&self) -> Span {
        match self {
            Self::Send(span, _, _)
            | Self::Receive(span, _, _, _)
            | Self::Signal(span, _, _)
            | Self::Case(span, _, _)
            | Self::Break(span)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            | Self::ReceiveType(span, _, _) => span.clone(),

            Self::Then(expression) => expression.span(),
        }
    }
}

impl ConstructBranch {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Then(span, expression) => {
                let expression = expression.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Receive(span, pattern, branch, vars) => {
                let process = branch.compile(pass)?;
                pattern.compile_receive(
                    0,
                    span,
                    &LocalName::result(),
                    process,
                    vars.clone(),
                    pass,
                )?
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }
        })
    }
}

impl Spanning for ConstructBranch {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _) | Self::Receive(span, _, _, _) | Self::ReceiveType(span, _, _) => {
                span.clone()
            }
        }
    }
}

impl Apply {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Noop(span) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::result(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Link(Arc::new(process::Expression::Variable(
                    span.clone(),
                    LocalName::object(),
                    (),
                    VariableUsage::Unknown,
                ))),
            }),

            Self::Send(span, expression, apply) => {
                let expression = expression.compile(pass)?;
                let process = apply.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(expression, process),
                })
            }

            Self::Signal(span, chosen, apply) => {
                let process = apply.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Self::Case(span, ApplyBranches(expression_branches), else_branch) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(branch_name.clone());
                    processes.push(expression_branch.compile(pass)?);
                }
                let else_process = match else_branch {
                    Some(branch) => Some(branch.compile(pass)?),
                    None => None,
                };
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Case(branches, processes, else_process),
                })
            }

            Self::Begin {
                span,
                unfounded,
                label,
                then: apply,
            } => {
                let process = apply.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone(),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::object(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Loop(
                    label.clone(),
                    LocalName::invalid(),
                    Captures::new(),
                ),
            }),

            Self::SendType(span, argument, apply) => {
                let process = apply.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Self::Default(span, expr, apply) => {
                let default_expr = expr.compile(pass)?;
                let ok_process = apply.compile(pass)?;
                compile_default(span, LocalName::object(), default_expr, ok_process)
            }

            Self::Try(span, label, apply) => {
                let catch_block = pass.use_catch(span, label)?;
                compile_try(span, LocalName::object(), catch_block, apply.compile(pass)?)
            }

            Self::Pipe(span, function, apply) => {
                let function = function.compile(pass)?;
                compile_pipe(span, LocalName::object(), function, apply.compile(pass)?)
            }
        })
    }
}

impl Spanning for Apply {
    fn span(&self) -> Span {
        match self {
            Self::Send(span, _, _)
            | Self::Signal(span, _, _)
            | Self::Case(span, _, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            | Self::Noop(span)
            | Self::Try(span, _, _)
            | Self::Default(span, _, _)
            | Self::Pipe(span, _, _) => span.clone(),
        }
    }
}

impl ApplyBranch {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Then(span, name, expression) => {
                let expression = expression.compile(pass)?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: name.clone(),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Variable(
                        span.clone(),
                        LocalName::object(),
                        (),
                        VariableUsage::Unknown,
                    )),
                    then: Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: LocalName::result(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Link(expression),
                    }),
                })
            }

            Self::Receive(span, pattern, branch, vars) => {
                let process = branch.compile(pass)?;
                pattern.compile_receive(
                    0,
                    span,
                    &LocalName::object(),
                    process,
                    vars.clone(),
                    pass,
                )?
            }

            Self::Continue(span, expression) => {
                let expression = expression.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Continue(Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: LocalName::result(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Link(expression),
                    })),
                })
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            Self::Try(span, label, branch) => {
                let catch_block = pass.use_catch(span, label)?;
                let process = branch.compile(pass)?;
                compile_try(span, LocalName::object(), catch_block, process)
            }

            Self::Default(span, expr, branch) => {
                let default_expr = expr.compile(pass)?;
                let ok_process = branch.compile(pass)?;
                compile_default(span, LocalName::object(), default_expr, ok_process)
            }
        })
    }
}

impl Spanning for ApplyBranch {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _, _)
            | Self::Receive(span, _, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveType(span, _, _)
            | Self::Try(span, _, _)
            | Self::Default(span, _, _) => span.clone(),
        }
    }
}

impl Process {
    pub fn compile(&self, pass: &mut Passes) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Let {
                span,
                pattern,
                value,
                then,
            } => {
                pass.set_fallthrough(None)?;
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let value = value.compile(pass)?;
                pass.enable_catches();
                pass.unset_fallthrough()?;
                pattern.compile_let(span, value, then.compile(pass)?, pass)?
            }

            Self::Catch {
                span,
                label,
                pattern,
                block,
                then,
            } => {
                pass.set_fallthrough(None)?;
                let block = pattern.compile_catch_block(span, block.compile(pass)?, pass)?;
                pass.unset_fallthrough()?;
                pass.set_catch(label.clone(), Some(block))?;
                let process = then.compile(pass)?;
                pass.unset_catch(label.clone())?;
                process
            }

            Self::Throw(span, label, expression) => {
                let catch_block = pass.use_catch(span, label)?;
                pass.set_fallthrough(None)?;
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let expression = expression.compile(pass)?;
                pass.enable_catches();
                pass.unset_fallthrough()?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: LocalName::error(),
                    annotation: None,
                    typ: (),
                    value: expression,
                    then: catch_block,
                })
            }

            Self::GlobalCommand(global_name, command) => {
                let span = global_name.span.clone();
                let local_name = LocalName {
                    span: span.clone(),
                    string: arcstr::format!("{}", global_name),
                };
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: local_name.clone(),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Global(span, global_name.clone(), ())),
                    then: command.compile(&local_name, pass)?,
                })
            }

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Telltypes(span, process) => Arc::new(process::Process::Telltypes(
                span.clone(),
                process.compile(pass)?,
            )),

            Self::Noop(span) => match pass.use_fallthrough() {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(span.clone()))?,
            },
        })
    }
}

impl Spanning for Process {
    fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => span.clone(),
            Self::Catch { span, .. } => span.clone(),
            Self::Throw(span, _, _) => span.clone(),
            Self::GlobalCommand(_, command) => command.span(),
            Self::Command(_, command) => command.span(),
            Self::Telltypes(span, _) => span.clone(),
            Self::Noop(span) => span.clone(),
        }
    }
}

impl Command {
    pub fn compile(
        &self,
        object_name: &LocalName,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Link(span, expression) => {
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let expression = expression.compile(pass)?;
                pass.enable_catches();
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Send(span, argument, command) => {
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let argument = argument.compile(pass)?;
                pass.enable_catches();
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Self::Receive(span, pattern, command, vars) => {
                let process = command.compile(object_name, pass)?;
                pattern.compile_receive(0, span, object_name, process, vars.clone(), pass)?
            }

            Self::Signal(span, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Self::Case(span, CommandBranches(process_branches), else_branch, optional_process) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();

                if let Some(process) = optional_process {
                    let process = process.compile(pass)?;
                    pass.set_fallthrough(Some(process))?;
                }
                for (branch_name, process_branch) in process_branches {
                    branches.push(branch_name.clone());
                    processes.push(process_branch.compile(object_name, pass)?);
                }
                let else_process = match else_branch {
                    Some(branch) => Some(branch.compile(object_name, pass)?),
                    None => None,
                };
                if optional_process.is_some() {
                    pass.unset_fallthrough()?;
                }

                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Case(branches, processes, else_process),
                })
            }

            Self::Break(span) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: object_name.clone(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Break,
            }),

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            Self::Begin {
                span,
                unfounded,
                label,
                then: command,
            } => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone(),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: object_name.clone(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Loop(
                    label.clone(),
                    LocalName::invalid(),
                    Captures::new(),
                ),
            }),

            Self::SendType(span, argument, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Self::ReceiveType(span, parameter, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            Self::Try(span, label, command) => {
                let catch_block = pass.use_catch(span, label)?;
                compile_try(
                    span,
                    object_name.clone(),
                    catch_block,
                    command.compile(object_name, pass)?,
                )
            }

            Self::Default(span, expr, command) => {
                let default_expr = expr.compile(pass)?;
                let ok_process = command.compile(object_name, pass)?;
                compile_default(span, object_name.clone(), default_expr, ok_process)
            }

            Self::Pipe(span, function, command) => {
                pass.disable_catches(CatchDisabledReason::DifferentProcess);
                let function = function.compile(pass)?;
                pass.enable_catches();
                let process = command.compile(object_name, pass)?;
                compile_pipe(span, object_name.clone(), function, process)
            }
        })
    }
}

fn compile_try(
    span: &Span,
    variable: LocalName,
    catch_block: Arc<process::Process<()>>,
    ok_process: Arc<process::Process<()>>,
) -> Arc<process::Process<()>> {
    Arc::new(process::Process::Do {
        span: span.clone(),
        name: variable.clone(),
        usage: VariableUsage::Unknown,
        typ: (),
        command: process::Command::Case(
            Arc::from([
                LocalName::from(literal!("err")),
                LocalName::from(literal!("ok")),
            ]),
            Box::from([
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: LocalName::error(),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Variable(
                        span.clone(),
                        variable,
                        (),
                        VariableUsage::Unknown,
                    )),
                    then: catch_block,
                }),
                ok_process,
            ]),
            None,
        ),
    })
}

fn compile_default(
    span: &Span,
    variable: LocalName,
    default_expr: Arc<process::Expression<()>>,
    ok_process: Arc<process::Process<()>>,
) -> Arc<process::Process<()>> {
    Arc::new(process::Process::Do {
        span: span.clone(),
        name: variable.clone(),
        usage: VariableUsage::Unknown,
        typ: (),
        command: process::Command::Case(
            Arc::from([
                LocalName::from(literal!("err")),
                LocalName::from(literal!("ok")),
            ]),
            Box::from([
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: variable.clone(),
                    annotation: None,
                    typ: (),
                    value: default_expr,
                    then: ok_process.clone(),
                }),
                ok_process,
            ]),
            None,
        ),
    })
}

fn compile_pipe(
    span: &Span,
    variable: LocalName,
    function: Arc<process::Expression<()>>,
    then: Arc<process::Process<()>>,
) -> Arc<process::Process<()>> {
    Arc::new(process::Process::Let {
        span: span.clone(),
        name: LocalName::temp(),
        annotation: None,
        typ: (),
        value: function,
        then: Arc::new(process::Process::Do {
            span: span.clone(),
            name: LocalName::temp(),
            usage: VariableUsage::Unknown,
            typ: (),
            command: process::Command::Send(
                Arc::new(process::Expression::Variable(
                    span.clone(),
                    variable.clone(),
                    (),
                    VariableUsage::Unknown,
                )),
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: variable,
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Variable(
                        span.clone(),
                        LocalName::temp(),
                        (),
                        VariableUsage::Unknown,
                    )),
                    then,
                }),
            ),
        }),
    })
}

impl Spanning for Command {
    fn span(&self) -> Span {
        match self {
            Self::Link(span, _)
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _, _)
            | Self::Signal(span, _, _)
            | Self::Case(span, _, _, _)
            | Self::Break(span)
            | Self::Continue(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            | Self::ReceiveType(span, _, _)
            | Self::Try(span, _, _)
            | Self::Default(span, _, _)
            | Self::Pipe(span, _, _) => span.clone(),

            Self::Then(process) => process.span(),
        }
    }
}

impl CommandBranch {
    pub fn compile(
        &self,
        object_name: &LocalName,
        pass: &mut Passes,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match self {
            Self::Then(_, process) => process.compile(pass)?,

            Self::BindThen(span, name, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: name.clone(),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Variable(
                        span.clone(),
                        object_name.clone(),
                        (),
                        VariableUsage::Unknown,
                    )),
                    then: process,
                })
            }

            Self::Receive(span, pattern, branch, vars) => {
                let process = branch.compile(object_name, pass)?;
                pattern.compile_receive(0, span, object_name, process, vars.clone(), pass)?
            }

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            Self::Try(span, label, branch) => {
                let catch_block = pass.use_catch(span, label)?;
                let process = branch.compile(object_name, pass)?;
                compile_try(span, object_name.clone(), catch_block, process)
            }
            Self::Default(span, expr, branch) => {
                let default_expr = expr.compile(pass)?;
                let ok_process = branch.compile(object_name, pass)?;
                compile_default(span, object_name.clone(), default_expr, ok_process)
            }
        })
    }
}

impl Spanning for CommandBranch {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _)
            | Self::BindThen(span, _, _)
            | Self::Receive(span, _, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveType(span, _, _)
            | Self::Try(span, _, _)
            | Self::Default(span, _, _) => span.clone(),
        }
    }
}
