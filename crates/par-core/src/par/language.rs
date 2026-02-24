// why not rename this file to ast.rs?

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    hash::Hash,
    sync::Arc,
};

use arcstr::{ArcStr, literal};

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

    pub fn subject() -> Self {
        Self {
            span: Span::None,
            string: literal!("#subject"),
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
pub enum Condition {
    Bool(Span, Box<Expression>),
    Is {
        span: Span,
        value: Expression,
        variant: LocalName,
        pattern: Pattern,
    },
    And(Span, Box<Self>, Box<Self>),
    Or(Span, Box<Self>, Box<Self>),
    Not(Span, Box<Self>),
}

impl Condition {
    pub fn span(&self) -> Span {
        match self {
            Self::Bool(span, _)
            | Self::Is { span, .. }
            | Self::And(span, _, _)
            | Self::Or(span, _, _)
            | Self::Not(span, _) => span.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Primitive(Span, Primitive),
    List(Span, Vec<Self>),
    Global(Span, GlobalName),
    Variable(Span, LocalName),
    Poll {
        span: Span,
        label: Option<LocalName>,
        clients: Vec<Expression>,
        name: LocalName,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
    Repoll {
        span: Span,
        label: Option<LocalName>,
        clients: Vec<Expression>,
        name: LocalName,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
    Submit {
        span: Span,
        label: Option<LocalName>,
        values: Vec<Expression>,
    },
    Condition(Span, Box<Condition>),
    Grouped(Span, Box<Self>),
    TypeIn {
        span: Span,
        typ: Type,
        expr: Box<Self>,
    },
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
    If {
        span: Span,
        branches: Vec<(Condition, Expression)>,
        else_: Option<Box<Self>>,
    },
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
    Poll {
        span: Span,
        label: Option<LocalName>,
        clients: Vec<Expression>,
        name: LocalName,
        then: Box<Process>,
        else_: Box<Process>,
    },
    Repoll {
        span: Span,
        label: Option<LocalName>,
        clients: Vec<Expression>,
        name: LocalName,
        then: Box<Process>,
        else_: Box<Process>,
    },
    Submit {
        span: Span,
        label: Option<LocalName>,
        values: Vec<Expression>,
    },
    Catch {
        span: Span,
        label: Option<LocalName>,
        pattern: Pattern,
        block: Box<Self>,
        then: Box<Self>,
    },
    Throw(Span, Option<LocalName>, Box<Expression>),
    If {
        span: Span,
        branches: Vec<(Condition, Process)>,
        else_: Option<Box<Process>>,
        then: Option<Box<Process>>,
    },
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
    NoSuchPollPoint(Span, Option<LocalName>),
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
            Self::NoSuchPollPoint(span, _) => span.clone(),
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
        let mk_report_owned = |span: &Span, msg: String| {
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
            Self::MatchingCatchDisabled(span, CatchDisabledReason::ValuePartiallyConstructed) => {
                mk_report(
                    span,
                    "The expression the matching `catch` would return from has its result already partially constructed.",
                )
            }
            Self::NoSuchPollPoint(span, None) => {
                mk_report(span, "No unlabeled `poll`/`repoll` point is in scope here.")
            }
            Self::NoSuchPollPoint(span, Some(label)) => mk_report_owned(
                span,
                format!("No such `poll@...`/`repoll@...` label `@{label}` is in scope here."),
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Context {
    passes: Passes,
    original_object_name: Option<LocalName>,
}

#[derive(Clone, Debug)]
pub(crate) struct Passes {
    next_block_index: usize,
    next_poll_index: usize,
    fallthrough: Option<Pass>,
    fallthrough_stash: Vec<Option<Pass>>,
    catch: HashMap<Option<LocalName>, Pass>,
    catch_stash: HashMap<Option<LocalName>, Vec<Option<Pass>>>,
    poll: Option<PollScope>,
    poll_stash: Vec<Option<PollScope>>,
}

#[derive(Clone, Debug)]
struct PollPoint {
    label: Option<ArcStr>,
    point: LocalName,
}

#[derive(Clone, Debug)]
struct PollScope {
    driver: LocalName,
    points: Vec<PollPoint>,
}

#[derive(Clone, Debug)]
struct Pass {
    block_index: usize,
    used: bool,
    // Stack of reasons that currently disable this catch (LIFO).
    disabled_reasons: Vec<CatchDisabledReason>,
}

impl Context {
    pub(crate) fn new() -> Self {
        Self {
            passes: Passes::new(),
            original_object_name: None,
        }
    }

    pub(crate) fn restore_object_name(
        &mut self,
        name: Option<LocalName>,
        process: Arc<process::Process<()>>,
    ) -> Arc<process::Process<()>> {
        match name {
            None => process,
            Some(original) => Arc::new(process::Process::Let {
                span: original.span.clone(),
                name: original.clone(),
                annotation: None,
                typ: (),
                value: Arc::new(process::Expression::Variable(
                    original.span.clone(),
                    LocalName::subject(),
                    (),
                    VariableUsage::Unknown,
                )),
                then: process,
            }),
        }
    }

    fn get_block_index(&mut self) -> usize {
        self.passes.get_block_index()
    }

    fn get_poll_index(&mut self) -> usize {
        self.passes.get_poll_index()
    }

    fn without_fallthrough(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        self.passes
            .fallthrough_stash
            .push(self.passes.fallthrough.take());
        let result = f(self);
        self.passes.fallthrough = self.passes.fallthrough_stash.pop().unwrap();
        result
    }

    fn with_fallthrough(
        &mut self,
        body: Arc<process::Process<()>>,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        self.passes
            .fallthrough_stash
            .push(self.passes.fallthrough.take());

        let block_index = self.get_block_index();
        self.passes.fallthrough = Some(Pass::new(block_index));
        let process = f(self)?;
        if !self.passes.fallthrough.take().unwrap().used {
            return Err(CompileError::UnreachableCode(body.span()));
        }
        let result = Arc::new(process::Process::Block(
            body.span(),
            block_index,
            body,
            process,
        ));

        self.passes.fallthrough = self.passes.fallthrough_stash.pop().unwrap();

        Ok(result)
    }

    fn expr_without_fallthrough(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Expression<()>>, CompileError>,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        self.passes
            .fallthrough_stash
            .push(self.passes.fallthrough.take());
        let result = f(self);
        self.passes.fallthrough = self.passes.fallthrough_stash.pop().unwrap();
        result
    }

    fn expr_with_fallthrough(
        &mut self,
        span: &Span,
        body: Arc<process::Process<()>>,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Expression<()>>, CompileError>,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        Ok(Arc::new(process::Expression::Chan {
            span: span.clone(),
            captures: Captures::new(),
            chan_name: LocalName::result(),
            chan_annotation: None,
            chan_type: (),
            expr_type: (),
            process: self.with_fallthrough(body, |pass| {
                Ok(Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(f(pass)?),
                }))
            })?,
        }))
    }

    fn use_fallthrough(&mut self, span: &Span) -> Option<Arc<process::Process<()>>> {
        self.passes
            .fallthrough
            .as_mut()
            .map(|pass| pass.use_at(span))
    }

    fn with_poll<T>(
        &mut self,
        driver: LocalName,
        point: LocalName,
        label: &Option<LocalName>,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.passes.poll_stash.push(self.passes.poll.take());
        self.passes.poll = Some(PollScope {
            driver,
            points: vec![PollPoint {
                label: label.as_ref().map(|l| l.string.clone()),
                point,
            }],
        });
        let result = f(self);
        self.passes.poll = self.passes.poll_stash.pop().unwrap();
        result
    }

    fn with_repoll<T>(
        &mut self,
        point: LocalName,
        label: &Option<LocalName>,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        let Some(mut scope) = self.passes.poll.clone() else {
            return f(self);
        };
        scope.points.push(PollPoint {
            label: label.as_ref().map(|l| l.string.clone()),
            point,
        });
        self.passes.poll_stash.push(self.passes.poll.take());
        self.passes.poll = Some(scope);
        let result = f(self);
        self.passes.poll = self.passes.poll_stash.pop().unwrap();
        result
    }

    fn without_poll<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        let outer = self.passes.poll_stash.last().cloned().unwrap_or(None);
        self.passes.poll_stash.push(self.passes.poll.take());
        self.passes.poll = outer;
        let result = f(self);
        self.passes.poll = self.passes.poll_stash.pop().unwrap();
        result
    }

    fn current_poll_driver(&self) -> Option<&LocalName> {
        self.passes.poll.as_ref().map(|p| &p.driver)
    }

    fn resolve_poll_point_by_label(&self, label: &LocalName) -> Option<&LocalName> {
        let label_str = &label.string;
        self.passes.poll.as_ref().and_then(|p| {
            p.points
                .iter()
                .rev()
                .find(|pp| pp.label.as_ref() == Some(label_str))
                .map(|pp| &pp.point)
        })
    }

    fn resolve_poll_point(&self, label: &Option<LocalName>) -> Option<&LocalName> {
        match label.as_ref() {
            Some(label) => self.resolve_poll_point_by_label(label),
            None => self.passes.poll.as_ref().and_then(|p| {
                p.points
                    .iter()
                    .rev()
                    .find(|pp| pp.label.is_none())
                    .map(|pp| &pp.point)
            }),
        }
    }

    fn make_poll_process(
        &mut self,
        span: &Span,
        kind: process::PollKind,
        label: &Option<LocalName>,
        clients: Vec<Arc<process::Expression<()>>>,
        name: LocalName,
        then: impl FnOnce(&mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
        else_: impl FnOnce(&mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        let id = self.get_poll_index();
        let point = LocalName {
            span: span.clone(),
            string: ArcStr::from(format!("@{id}")),
        };

        let driver = match kind {
            process::PollKind::Poll => LocalName {
                span: span.clone(),
                string: ArcStr::from(format!("#pool{id}")),
            },
            process::PollKind::Repoll => self
                .current_poll_driver()
                .cloned()
                .unwrap_or_else(LocalName::invalid),
        };

        let build = |pass: &mut Self| {
            let then = then(pass)?;
            let else_ = pass.without_poll(|pass| else_(pass))?;
            Ok(Arc::new(process::Process::Poll {
                span: span.clone(),
                kind: kind.clone(),
                driver: driver.clone(),
                point: point.clone(),
                clients: clients,
                name: name,
                name_typ: (),
                captures: Captures::new(),
                then,
                else_,
            }))
        };

        match kind {
            process::PollKind::Poll => {
                self.with_poll(driver.clone(), point.clone(), label, |pass| build(pass))
            }
            process::PollKind::Repoll => self.with_repoll(point.clone(), label, |pass| build(pass)),
        }
    }

    fn make_submit_process(
        &mut self,
        span: &Span,
        label: &Option<LocalName>,
        values: Vec<Arc<process::Expression<()>>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        let driver = self
            .current_poll_driver()
            .cloned()
            .unwrap_or_else(LocalName::invalid);
        let point = match self.passes.poll.as_ref() {
            None => LocalName::invalid(),
            Some(_) => self.resolve_poll_point(label).cloned().ok_or_else(|| {
                let err_span = label
                    .as_ref()
                    .map(|l| l.span())
                    .unwrap_or_else(|| span.clone());
                CompileError::NoSuchPollPoint(err_span, label.clone())
            })?,
        };

        Ok(Arc::new(process::Process::Submit {
            span: span.clone(),
            driver,
            point,
            values,
            captures: Captures::new(),
        }))
    }

    fn with_catch(
        &mut self,
        label: Option<LocalName>,
        body: Arc<process::Process<()>>,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        self.passes
            .catch_stash
            .entry(label.clone())
            .or_default()
            .push(self.passes.catch.remove(&label));
        let block_index = self.get_block_index();
        self.passes
            .catch
            .insert(label.clone(), Pass::new(block_index));

        let result = f(self);

        let current = self.passes.catch.remove(&label);
        let stashed = self
            .passes
            .catch_stash
            .entry(label.clone())
            .or_default()
            .pop()
            .unwrap();
        if let Some(stashed) = stashed {
            self.passes.catch.insert(label, stashed);
        }
        let process = result?;
        if !current.unwrap().used {
            return Err(CompileError::UnreachableCode(body.span()));
        }
        Ok(Arc::new(process::Process::Block(
            body.span(),
            block_index,
            body,
            process,
        )))
    }

    fn expr_with_catch(
        &mut self,
        span: &Span,
        label: Option<LocalName>,
        body: Arc<process::Process<()>>,
        f: impl FnOnce(&mut Self) -> Result<Arc<process::Expression<()>>, CompileError>,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        Ok(Arc::new(process::Expression::Chan {
            span: span.clone(),
            captures: Captures::new(),
            chan_name: LocalName::result(),
            chan_annotation: None,
            chan_type: (),
            expr_type: (),
            process: self.with_catch(label, body, |pass| {
                Ok(Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(f(pass)?),
                }))
            })?,
        }))
    }

    fn use_catch(
        &mut self,
        span: &Span,
        label: &Option<LocalName>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        match self.passes.catch.get_mut(label) {
            Some(pass) => {
                if let Some(reason) = pass.disabled_reasons.last().cloned() {
                    return Err(CompileError::MatchingCatchDisabled(span.clone(), reason));
                }
                Ok(pass.use_at(span))
            }
            None => Err(CompileError::NoMatchingCatch(span.clone())),
        }
    }

    fn disable_catches(&mut self, reason: CatchDisabledReason) -> &mut Self {
        for pass in self.passes.catch.values_mut() {
            pass.disabled_reasons.push(reason.clone());
        }
        self
    }

    fn enable_catches(&mut self) -> &mut Self {
        for pass in self.passes.catch.values_mut() {
            if !pass.disabled_reasons.is_empty() {
                pass.disabled_reasons.pop().unwrap();
            }
        }
        self
    }

    pub(crate) fn compile_pattern_let(
        &mut self,
        pattern: &Pattern,
        span: &Span,
        expression: Arc<process::Expression<()>>,
        process: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Pattern::Name(_, name, annotation) = pattern {
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
            annotation: pattern.annotation(),
            typ: (),
            value: expression,
            then: self.compile_pattern_helper(pattern, 0, process)?,
        }))
    }

    pub(crate) fn compile_pattern_chan(
        &mut self,
        pattern: &Pattern,
        span: &Span,
        process: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        if let Pattern::Name(_, name, annotation) = pattern {
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
            process: self.compile_pattern_helper(pattern, 0, process)?,
        }))
    }

    pub(crate) fn compile_pattern_catch_block(
        &mut self,
        pattern: &Pattern,
        span: &Span,
        block: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Pattern::Name(_, name, annotation) = pattern {
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
            then: self.compile_pattern_helper(pattern, 0, block)?,
        }))
    }

    pub(crate) fn compile_pattern_receive(
        &mut self,
        pattern: &Pattern,
        level: usize,
        span: &Span,
        subject: &LocalName,
        process: Arc<process::Process<()>>,
        vars: Vec<LocalName>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if let Pattern::Name(_, name, annotation) = pattern {
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
                pattern.annotation(),
                (),
                self.compile_pattern_helper(pattern, level, process)?,
                vars,
            ),
        }))
    }

    fn compile_pattern_helper(
        &mut self,
        pattern: &Pattern,
        level: usize,
        process: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        match pattern {
            Pattern::Name(span, name, annotation) => Ok(Arc::new(process::Process::Let {
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

            Pattern::Receive(span, first, rest, vars) => {
                let then_process = self.compile_pattern_helper(rest, level, process)?;
                self.compile_pattern_receive(
                    first,
                    level + 1,
                    span,
                    &LocalName::match_(level),
                    then_process,
                    vars.clone(),
                )
            }

            Pattern::Continue(span) => Ok(Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::match_(level),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Continue(process),
            })),

            Pattern::ReceiveType(span, parameter, rest) => Ok(Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::match_(level),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::ReceiveType(
                    parameter.clone(),
                    self.compile_pattern_helper(rest, level, process)?,
                ),
            })),

            Pattern::Try(span, label, rest) => {
                let catch_block = self.use_catch(span, label)?;
                let catch_block = if let Some(original) = &self.original_object_name {
                    Arc::new(process::Process::Let {
                        span: original.span.clone(),
                        name: original.clone(),
                        annotation: None,
                        typ: (),
                        value: Arc::new(process::Expression::Variable(
                            original.span.clone(),
                            LocalName::subject(),
                            (),
                            VariableUsage::Unknown,
                        )),
                        then: catch_block,
                    })
                } else {
                    catch_block
                };
                let then_process = self.compile_pattern_helper(rest, level, process)?;
                Ok(self.compile_try(span, LocalName::match_(level), catch_block, then_process))
            }

            Pattern::Default(span, expr, rest) => {
                let default_expr = self.compile_expression(expr)?;
                let ok_process = self.compile_pattern_helper(rest, level, process)?;
                Ok(self.compile_default(span, LocalName::match_(level), default_expr, ok_process))
            }
        }
    }

    pub(crate) fn compile_expression(
        &mut self,
        expr: &Expression,
    ) -> Result<Arc<process::Expression<()>>, CompileError> {
        let original_name = std::mem::take(&mut self.original_object_name);
        let res = Ok(match expr {
            Expression::Primitive(span, value) => Arc::new(process::Expression::Primitive(
                span.clone(),
                value.clone(),
                (),
            )),

            Expression::List(span, items) => {
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
                                command: process::Command::Send(
                                    self.compile_expression(item)?,
                                    process,
                                ),
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

            Expression::Global(span, name) => {
                Arc::new(process::Expression::Global(span.clone(), name.clone(), ()))
            }

            Expression::Variable(span, name) => Arc::new(process::Expression::Variable(
                span.clone(),
                name.clone(),
                (),
                VariableUsage::Unknown,
            )),

            Expression::Poll {
                span,
                label,
                clients,
                name,
                then,
                else_,
            } => {
                let clients: Vec<_> = clients
                    .iter()
                    .map(|e| self.compile_expression(e))
                    .collect::<Result<_, _>>()?;
                let process = self.make_poll_process(
                    span,
                    process::PollKind::Poll,
                    label,
                    clients,
                    name.clone(),
                    |pass| pass.compile_process(&link_process_from_expr(then)),
                    |pass| pass.compile_process(&link_process_from_expr(else_)),
                )?;

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

            Expression::Repoll {
                span,
                label,
                clients,
                name,
                then,
                else_,
            } => {
                let clients: Result<Vec<_>, _> =
                    clients.iter().map(|e| self.compile_expression(e)).collect();
                let clients = clients?;
                let process = self.make_poll_process(
                    span,
                    process::PollKind::Repoll,
                    label,
                    clients,
                    name.clone(),
                    |pass| pass.compile_process(&link_process_from_expr(then)),
                    |pass| pass.compile_process(&link_process_from_expr(else_)),
                )?;

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

            Expression::Submit {
                span,
                label,
                values,
            } => {
                let values: Result<Vec<_>, _> =
                    values.iter().map(|e| self.compile_expression(e)).collect();
                let values = values?;
                let process = self.make_submit_process(span, label, values)?;
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

            Expression::Condition(span, condition) => {
                let true_process = Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(
                        LocalName {
                            span: span.clone(),
                            string: literal!("true"),
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
                let false_process = Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(
                        LocalName {
                            span: span.clone(),
                            string: literal!("false"),
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
                let process = self.compile_condition_process(
                    condition.as_ref(),
                    true_process,
                    false_process,
                )?;
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

            Expression::Grouped(_, expression) => self.compile_expression(expression)?,

            Expression::TypeIn { span, typ, expr } => {
                let expression = self.compile_expression(expr)?;
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
                        annotation: Some(typ.clone()),
                        typ: (),
                        value: expression,
                        then: Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: LocalName::result(),
                            usage: VariableUsage::Unknown,
                            typ: (),
                            command: process::Command::Link(Arc::new(
                                process::Expression::Variable(
                                    span.clone(),
                                    LocalName::object(),
                                    (),
                                    VariableUsage::Unknown,
                                ),
                            )),
                        }),
                    }),
                })
            }

            Expression::Box(span, expression) => {
                let expression = self.compile_expression(expression)?;
                Arc::new(process::Expression::Box(
                    span.clone(),
                    Captures::new(),
                    expression,
                    (),
                ))
            }

            Expression::Let {
                span,
                pattern,
                expression,
                then: body,
            } => {
                self.disable_catches(CatchDisabledReason::DifferentProcess);
                let expression = self.compile_expression(expression)?;
                self.enable_catches();
                let body = self.compile_expression(body)?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: self.compile_pattern_let(
                        pattern,
                        span,
                        expression,
                        Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: LocalName::result(),
                            usage: VariableUsage::Unknown,
                            typ: (),
                            command: process::Command::Link(body),
                        }),
                    )?,
                })
            }

            Expression::Catch {
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
                    command: process::Command::Link(self.compile_expression(block)?),
                });
                let block = self.compile_pattern_catch_block(pattern, span, block)?;
                self.expr_with_catch(span, label.clone(), block, |pass| {
                    pass.compile_expression(then)
                })?
            }

            Expression::Throw(span, label, expression) => {
                let catch_block = self.use_catch(span, label)?;
                self.expr_without_fallthrough(|pass| {
                    Ok(Arc::new(process::Expression::Chan {
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
                            value: pass.compile_expression(expression)?,
                            then: catch_block,
                        }),
                    }))
                })?
            }

            Expression::If {
                span,
                branches,
                else_,
            } => {
                let else_proc = match else_ {
                    Some(expr) => self.compile_process(&link_process_from_expr(expr))?,
                    None => Arc::new(process::Process::Unreachable(span.clone())),
                };
                let compiled = self.compile_if_branches(branches, else_proc, |body, pass| {
                    pass.compile_process(&link_process_from_expr(body))
                })?;
                Arc::new(process::Expression::Chan {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: LocalName::result(),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: compiled,
                })
            }

            Expression::Do {
                span,
                process,
                then: expression,
            } => {
                let expression = self.compile_expression(expression)?;
                self.expr_with_fallthrough(
                    span,
                    Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: LocalName::result(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Link(expression),
                    }),
                    |pass| {
                        Ok(Arc::new(process::Expression::Chan {
                            span: span.clone(),
                            captures: Captures::new(),
                            chan_name: LocalName::result(),
                            chan_annotation: None,
                            chan_type: (),
                            expr_type: (),
                            process: pass.compile_process(process)?,
                        }))
                    },
                )?
            }

            Expression::Chan {
                span,
                pattern,
                process,
            } => {
                self.disable_catches(CatchDisabledReason::DifferentProcess);
                let proc = self.compile_process(process)?;
                self.enable_catches();
                self.compile_pattern_chan(pattern, span, proc)?
            }

            Expression::Construction(construct) => {
                self.disable_catches(CatchDisabledReason::ValuePartiallyConstructed);
                let process = self.compile_construct(construct)?;
                self.enable_catches();
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

            Expression::Application(_, expr, Apply::Noop(_)) => self.compile_expression(expr)?,

            Expression::Application(span, expr, apply) => {
                let expr = self.compile_expression(expr)?;
                let process = self.compile_apply(apply)?;
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
        });
        let None = self.original_object_name else {
            unreachable!("original_object_name should be none after expression")
        };
        self.original_object_name = original_name;

        res
    }

    pub(crate) fn compile_construct(
        &mut self,
        construct: &Construct,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match construct {
            Construct::Then(expression) => {
                let span = expression.span().clone();
                let expression = self.compile_expression(expression)?;
                Arc::new(process::Process::Do {
                    span: span,
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Construct::Send(span, argument, construct) => {
                let argument = self.compile_expression(argument)?;
                let process = self.compile_construct(construct)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Construct::Receive(span, pattern, construct, vars) => {
                let process = self.compile_construct(construct)?;
                self.compile_pattern_receive(
                    pattern,
                    0,
                    span,
                    &LocalName::result(),
                    process,
                    vars.clone(),
                )?
            }

            Construct::Signal(span, chosen, construct) => {
                let process = self.compile_construct(construct)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Construct::Case(span, ConstructBranches(construct_branches), else_branch) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(branch_name.clone());
                    processes.push(self.compile_construct_branch(construct_branch)?);
                }
                let else_process = match else_branch {
                    Some(branch) => Some(self.compile_construct_branch(branch)?),
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

            Construct::Break(span) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: LocalName::result(),
                usage: VariableUsage::Unknown,
                typ: (),
                command: process::Command::Break,
            }),

            Construct::Begin {
                span,
                unfounded,
                label,
                then: construct,
            } => {
                let process = self.compile_construct(construct)?;
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

            Construct::Loop(span, label) => Arc::new(process::Process::Do {
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

            Construct::SendType(span, argument, construct) => {
                let process = self.compile_construct(construct)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Construct::ReceiveType(span, parameter, construct) => {
                let process = self.compile_construct(construct)?;
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

    pub(crate) fn compile_construct_branch(
        &mut self,
        branch: &ConstructBranch,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match branch {
            ConstructBranch::Then(span, expression) => {
                let expression = self.compile_expression(expression)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::result(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            ConstructBranch::Receive(span, pattern, branch, vars) => {
                let process = self.compile_construct_branch(branch)?;
                self.compile_pattern_receive(
                    pattern,
                    0,
                    span,
                    &LocalName::result(),
                    process,
                    vars.clone(),
                )?
            }

            ConstructBranch::ReceiveType(span, parameter, branch) => {
                let process = self.compile_construct_branch(branch)?;
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

    pub(crate) fn compile_apply(
        &mut self,
        apply: &Apply,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match apply {
            Apply::Noop(span) => Arc::new(process::Process::Do {
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

            Apply::Send(span, expression, apply) => {
                let expression = self.compile_expression(expression)?;
                let process = self.compile_apply(apply)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(expression, process),
                })
            }

            Apply::Signal(span, chosen, apply) => {
                let process = self.compile_apply(apply)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Apply::Case(span, ApplyBranches(expression_branches), else_branch) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(branch_name.clone());
                    processes.push(self.compile_apply_branch(expression_branch)?);
                }
                let else_process = match else_branch {
                    Some(branch) => Some(self.compile_apply_branch(branch)?),
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

            Apply::Begin {
                span,
                unfounded,
                label,
                then: apply,
            } => {
                let process = self.compile_apply(apply)?;
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

            Apply::Loop(span, label) => Arc::new(process::Process::Do {
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

            Apply::SendType(span, argument, apply) => {
                let process = self.compile_apply(apply)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Apply::Default(span, expr, apply) => {
                let default_expr = self.compile_expression(expr)?;
                let ok_process = self.compile_apply(apply)?;
                self.compile_default(span, LocalName::object(), default_expr, ok_process)
            }

            Apply::Try(span, label, apply) => {
                let catch_block = self.use_catch(span, label)?;
                let ok_process = self.compile_apply(apply)?;
                self.compile_try(span, LocalName::object(), catch_block, ok_process)
            }

            Apply::Pipe(span, function, apply) => {
                let function = self.compile_expression(function)?;
                let then = self.compile_apply(apply)?;
                self.compile_pipe(span, LocalName::object(), function, then)
            }
        })
    }

    pub(crate) fn compile_apply_branch(
        &mut self,
        branch: &ApplyBranch,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match branch {
            ApplyBranch::Then(span, name, expression) => {
                let expression = self.compile_expression(expression)?;
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

            ApplyBranch::Receive(span, pattern, branch, vars) => {
                let process = self.compile_apply_branch(branch)?;
                self.compile_pattern_receive(
                    pattern,
                    0,
                    span,
                    &LocalName::object(),
                    process,
                    vars.clone(),
                )?
            }

            ApplyBranch::Continue(span, expression) => {
                let expression = self.compile_expression(expression)?;
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

            ApplyBranch::ReceiveType(span, parameter, branch) => {
                let process = self.compile_apply_branch(branch)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: LocalName::object(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            ApplyBranch::Try(span, label, branch) => {
                let catch_block = self.use_catch(span, label)?;
                let process = self.compile_apply_branch(branch)?;
                self.compile_try(span, LocalName::object(), catch_block, process)
            }

            ApplyBranch::Default(span, expr, branch) => {
                let default_expr = self.compile_expression(expr)?;
                let ok_process = self.compile_apply_branch(branch)?;
                self.compile_default(span, LocalName::object(), default_expr, ok_process)
            }
        })
    }

    pub(crate) fn compile_process(
        &mut self,
        process: &Process,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match process {
            Process::If {
                span,
                branches,
                else_,
                then,
            } => {
                if let Some(tail) = then {
                    let tail = self.compile_process(tail)?;
                    self.with_fallthrough(tail, |pass| {
                        let else_proc = match else_ {
                            Some(proc) => pass.compile_process(proc)?,
                            None => Arc::new(process::Process::Unreachable(span.clone())),
                        };
                        pass.compile_if_branches(branches, else_proc, |body, pass| {
                            pass.compile_process(body)
                        })
                    })?
                } else {
                    let else_proc = match else_ {
                        Some(proc) => self.compile_process(proc)?,
                        None => Arc::new(process::Process::Unreachable(span.clone())),
                    };
                    self.compile_if_branches(branches, else_proc, |body, pass| {
                        pass.compile_process(body)
                    })?
                }
            }

            Process::Poll {
                span,
                label,
                clients,
                name,
                then,
                else_,
            } => {
                let clients: Result<Vec<_>, _> =
                    clients.iter().map(|e| self.compile_expression(e)).collect();
                let clients = clients?;
                self.make_poll_process(
                    span,
                    process::PollKind::Poll,
                    label,
                    clients,
                    name.clone(),
                    |pass| pass.compile_process(then),
                    |pass| pass.compile_process(else_),
                )?
            }

            Process::Repoll {
                span,
                label,
                clients,
                name,
                then,
                else_,
            } => {
                let clients: Result<Vec<_>, _> =
                    clients.iter().map(|e| self.compile_expression(e)).collect();
                let clients = clients?;
                self.make_poll_process(
                    span,
                    process::PollKind::Repoll,
                    label,
                    clients,
                    name.clone(),
                    |pass| pass.compile_process(then),
                    |pass| pass.compile_process(else_),
                )?
            }

            Process::Submit {
                span,
                label,
                values,
            } => {
                let values: Result<Vec<_>, _> =
                    values.iter().map(|e| self.compile_expression(e)).collect();
                let values = values?;
                self.make_submit_process(span, label, values)?
            }

            Process::Let {
                span,
                pattern,
                value,
                then,
            } => {
                let value = self.expr_without_fallthrough(|pass| {
                    pass.disable_catches(CatchDisabledReason::DifferentProcess);
                    let value = pass.compile_expression(value)?;
                    pass.enable_catches();
                    Ok(value)
                })?;
                let then_process = self.compile_process(then)?;
                self.compile_pattern_let(pattern, span, value, then_process)?
            }

            Process::Catch {
                span,
                label,
                pattern,
                block,
                then,
            } => {
                let block = self.without_fallthrough(|pass| {
                    let block = pass.compile_process(block)?;
                    pass.compile_pattern_catch_block(pattern, span, block)
                })?;
                let process =
                    self.with_catch(label.clone(), block, |pass| pass.compile_process(then))?;
                process
            }

            Process::Throw(span, label, expression) => {
                let catch_block = self.use_catch(span, label)?;
                let expression = self.expr_without_fallthrough(|pass| {
                    pass.disable_catches(CatchDisabledReason::DifferentProcess);
                    let expression = pass.compile_expression(expression);
                    pass.enable_catches();
                    expression
                })?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: LocalName::error(),
                    annotation: None,
                    typ: (),
                    value: expression,
                    then: catch_block,
                })
            }

            Process::GlobalCommand(global_name, command) => {
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
                    then: self.compile_command(command, &local_name)?,
                })
            }

            Process::Command(name, command) => {
                let None = self.original_object_name else {
                    // this should never happen. If it did it means we forgot to exit the alias-mode.
                    unreachable!(
                        "Can't be in more than one command chain at once. currently set to: {}",
                        self.original_object_name.clone().unwrap().string
                    )
                };
                self.original_object_name = Some(name.clone());
                let then_process = self.compile_command(command, &LocalName::subject())?;
                let None = self.original_object_name else {
                    // this should never happen. If it did it means we forgot to exit the alias-mode.
                    unreachable!(
                        "Can't be in more than one command chain at once. {:?} was: {}",
                        command,
                        self.original_object_name.clone().unwrap().string
                    )
                };
                Arc::new(process::Process::Let {
                    span: name.span.clone(),
                    name: LocalName::subject(),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Variable(
                        name.span.clone(),
                        name.clone(),
                        (),
                        VariableUsage::Unknown,
                    )),
                    then: then_process,
                })
            }

            Process::Telltypes(span, process) => Arc::new(process::Process::Telltypes(
                span.clone(),
                self.compile_process(process)?,
            )),

            Process::Noop(span) => match self.use_fallthrough(span) {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(span.clone()))?,
            },
        })
    }

    pub(crate) fn compile_command(
        &mut self,
        command: &Command,
        object_name: &LocalName,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match command {
            Command::Then(process) => {
                let original = std::mem::take(&mut self.original_object_name);
                let process = self.compile_process(process)?;
                self.restore_object_name(original, process)
            }

            Command::Link(span, expression) => {
                self.disable_catches(CatchDisabledReason::DifferentProcess);
                let expression = self.compile_expression(expression)?;
                self.enable_catches();
                self.original_object_name = None;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Command::Send(span, argument, command) => {
                self.disable_catches(CatchDisabledReason::DifferentProcess);
                let argument = self.compile_expression(argument)?;
                self.enable_catches();
                let process = self.compile_command(command, object_name)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Command::Receive(span, pattern, command, vars) => {
                let original = self.original_object_name.clone();
                let process = self.compile_command(command, object_name)?;
                let None = self.original_object_name else {
                    unreachable!("original_object_name should be none after command")
                };
                self.original_object_name = original;
                let process = self.compile_pattern_receive(
                    pattern,
                    0,
                    span,
                    object_name,
                    process,
                    vars.clone(),
                )?;
                self.original_object_name = None;
                process
            }

            Command::Signal(span, chosen, command) => {
                let process = self.compile_command(command, object_name)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Signal(chosen.clone(), process),
                })
            }

            Command::Case(
                span,
                CommandBranches(process_branches),
                else_branch,
                optional_process,
            ) => {
                let original = std::mem::take(&mut self.original_object_name);
                let object_name = match &original {
                    None => object_name,
                    Some(original) => original,
                };

                let mut branches = Vec::new();
                let mut processes = Vec::new();

                let process = if let Some(process) = optional_process {
                    let process = self.compile_process(process)?;
                    self.with_fallthrough(process, |pass| {
                        for (branch_name, process_branch) in process_branches {
                            branches.push(branch_name.clone());
                            processes
                                .push(pass.compile_command_branch(process_branch, object_name)?);
                        }
                        let else_process = match else_branch {
                            Some(branch) => Some(pass.compile_command_branch(branch, object_name)?),
                            None => None,
                        };
                        let branches = Arc::from(branches);
                        let processes = Box::from(processes);
                        Ok(Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: object_name.clone(),
                            usage: VariableUsage::Unknown,
                            typ: (),
                            command: process::Command::Case(branches, processes, else_process),
                        }))
                    })?
                } else {
                    for (branch_name, process_branch) in process_branches {
                        branches.push(branch_name.clone());
                        processes.push(self.compile_command_branch(process_branch, object_name)?);
                    }
                    let else_process = match else_branch {
                        Some(branch) => Some(self.compile_command_branch(branch, object_name)?),
                        None => None,
                    };
                    let branches = Arc::from(branches);
                    let processes = Box::from(processes);
                    Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: object_name.clone(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Case(branches, processes, else_process),
                    })
                };
                self.restore_object_name(original, process)
            }

            Command::Break(span) => {
                self.original_object_name = None;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Break,
                })
            }

            Command::Continue(span, process) => {
                self.original_object_name = None;
                let process = self.compile_process(process)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            Command::Begin {
                span,
                unfounded,
                label,
                then: command,
            } => {
                let process = self.compile_command(command, object_name)?;
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

            Command::Loop(span, label) => {
                self.original_object_name = None;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Loop(
                        label.clone(),
                        LocalName::invalid(),
                        Captures::new(),
                    ),
                })
            }

            Command::SendType(span, argument, command) => {
                let process = self.compile_command(command, object_name)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::SendType(argument.clone(), process),
                })
            }

            Command::ReceiveType(span, parameter, command) => {
                let process = self.compile_command(command, object_name)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            Command::Try(span, label, command) => {
                let catch_block = self.use_catch(span, label)?;
                let ok_process = self.compile_command(command, object_name)?;
                self.compile_try(span, object_name.clone(), catch_block, ok_process)
            }

            Command::Default(span, expr, command) => {
                let default_expr = self.compile_expression(expr)?;
                let ok_process = self.compile_command(command, object_name)?;
                self.compile_default(span, object_name.clone(), default_expr, ok_process)
            }

            Command::Pipe(span, function, command) => {
                self.disable_catches(CatchDisabledReason::DifferentProcess);
                let function = self.compile_expression(function)?;
                self.enable_catches();
                let process = self.compile_command(command, object_name)?;
                self.compile_pipe(span, object_name.clone(), function, process)
            }
        })
    }

    pub(crate) fn compile_command_branch(
        &mut self,
        branch: &CommandBranch,
        object_name: &LocalName,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match branch {
            CommandBranch::Then(_, process) => self.compile_process(process)?,

            CommandBranch::BindThen(span, name, process) => {
                let process = self.compile_process(process)?;
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

            CommandBranch::Receive(span, pattern, branch, vars) => {
                let process = self.compile_command_branch(branch, object_name)?;
                self.compile_pattern_receive(pattern, 0, span, object_name, process, vars.clone())?
            }

            CommandBranch::Continue(span, process) => {
                let process = self.compile_process(process)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            CommandBranch::ReceiveType(span, parameter, branch) => {
                let process = self.compile_command_branch(branch, object_name)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::ReceiveType(parameter.clone(), process),
                })
            }

            CommandBranch::Try(span, label, branch) => {
                let catch_block = self.use_catch(span, label)?;
                let process = self.compile_command_branch(branch, object_name)?;
                self.compile_try(span, object_name.clone(), catch_block, process)
            }
            CommandBranch::Default(span, expr, branch) => {
                let default_expr = self.compile_expression(expr)?;
                let ok_process = self.compile_command_branch(branch, object_name)?;
                self.compile_default(span, object_name.clone(), default_expr, ok_process)
            }
        })
    }

    fn compile_try(
        &self,
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
        &mut self,
        span: &Span,
        variable: LocalName,
        default_expr: Arc<process::Expression<()>>,
        ok_process: Arc<process::Process<()>>,
    ) -> Arc<process::Process<()>> {
        self.with_fallthrough(ok_process, |pass| {
            Ok(Arc::new(process::Process::Do {
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
                            then: pass.use_fallthrough(span).unwrap(),
                        }),
                        pass.use_fallthrough(span).unwrap(),
                    ]),
                    None,
                ),
            }))
        })
        .unwrap()
    }

    fn compile_pipe(
        &self,
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

    fn attach_pattern_to_process_compiled(
        &mut self,
        pattern: &Pattern,
        body: Arc<process::Process<()>>,
        subject: &LocalName,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        if matches!(pattern, Pattern::Continue(_)) {
            return Ok(body);
        }
        self.compile_pattern_let(
            pattern,
            &pattern.span().join(body.span()),
            Arc::new(process::Expression::Variable(
                pattern.span(),
                subject.clone(),
                (),
                VariableUsage::Unknown,
            )),
            body,
        )
    }

    fn compile_restorations(
        &mut self,
        restores: &[Restoration],
        tail: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        restores.iter().rev().try_fold(tail, |acc, restore| {
            let value = self.compile_expression(&restore.value)?;
            Ok(Arc::new(process::Process::Let {
                span: restore.span.clone(),
                name: restore.name.clone(),
                annotation: None,
                typ: (),
                value,
                then: acc,
            }))
        })
    }

    fn condition_process_core(
        &mut self,
        condition: &Condition,
        success: Arc<process::Process<()>>,
        failure: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        Ok(match condition {
            Condition::Bool(span, expr) => {
                let temp = LocalName::temp();
                let expr = self.compile_expression(expr)?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: temp.clone(),
                    annotation: None,
                    typ: (),
                    value: expr,
                    then: Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: temp.clone(),
                        usage: VariableUsage::Unknown,
                        typ: (),
                        command: process::Command::Case(
                            Arc::from([
                                LocalName::from(literal!("false")),
                                LocalName::from(literal!("true")),
                            ]),
                            Box::from([failure, success]),
                            None,
                        ),
                    }),
                })
            }
            Condition::Is {
                span,
                value,
                variant,
                pattern,
            } => {
                let (subject_name, binding_value) = match value {
                    Expression::Variable(_, name) => (name.clone(), None),
                    _ => (LocalName::temp(), Some(self.compile_expression(value)?)),
                };

                let success_process =
                    self.attach_pattern_to_process_compiled(pattern, success, &subject_name)?;

                let command_process = Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: subject_name.clone(),
                    usage: VariableUsage::Unknown,
                    typ: (),
                    command: process::Command::Case(
                        Arc::from([variant.clone()]),
                        Box::from([success_process]),
                        Some(failure),
                    ),
                });

                match binding_value {
                    Some(value) => Arc::new(process::Process::Let {
                        span: span.clone(),
                        name: subject_name.clone(),
                        annotation: None,
                        typ: (),
                        value,
                        then: command_process,
                    }),
                    None => command_process,
                }
            }
            Condition::And(span, left, right) => self.with_fallthrough(failure, |pass| {
                let left_fallthrough = pass.use_fallthrough(span).unwrap();
                let right_fallthrough = pass.use_fallthrough(span).unwrap();
                let restored_failure =
                    pass.compile_restorations(&collect_restorations(left), right_fallthrough)?;
                let right_process =
                    pass.condition_process_core(right, success, restored_failure)?;
                pass.condition_process_core(left, right_process, left_fallthrough)
            })?,
            Condition::Or(span, left, right) => self.with_fallthrough(success, |pass| {
                let left_fallthrough = pass.use_fallthrough(span).unwrap();
                let right_fallthrough = pass.use_fallthrough(span).unwrap();
                let right_process =
                    pass.condition_process_core(right, right_fallthrough, failure)?;
                pass.condition_process_core(left, left_fallthrough, right_process)
            })?,
            Condition::Not(_, inner) => self.condition_process_core(inner, failure, success)?,
        })
    }

    fn compile_condition_process(
        &mut self,
        condition: &Condition,
        success_body: Arc<process::Process<()>>,
        failure_body: Arc<process::Process<()>>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        let span = condition.span();
        self.with_fallthrough(failure_body, |pass| {
            let goto_failure = pass.use_fallthrough(&span).unwrap();
            pass.with_fallthrough(success_body, |pass| {
                let goto_success = pass.use_fallthrough(&span).unwrap();
                pass.condition_process_core(condition, goto_success, goto_failure)
            })
        })
    }

    fn compile_if_branches<B>(
        &mut self,
        branches: &[(Condition, B)],
        else_proc: Arc<process::Process<()>>,
        mut compile_body: impl FnMut(&B, &mut Self) -> Result<Arc<process::Process<()>>, CompileError>,
    ) -> Result<Arc<process::Process<()>>, CompileError> {
        branches
            .iter()
            .rev()
            .try_fold(else_proc, |acc, (condition, body)| {
                let success = compile_body(body, self)?;
                self.compile_condition_process(condition, success, acc)
            })
    }
}

impl Passes {
    pub(crate) fn new() -> Self {
        Passes {
            next_block_index: 1,
            next_poll_index: 1,
            fallthrough: None,
            fallthrough_stash: Vec::new(),
            catch: HashMap::new(),
            catch_stash: HashMap::new(),
            poll: None,
            poll_stash: Vec::new(),
        }
    }

    fn get_block_index(&mut self) -> usize {
        let index = self.next_block_index;
        self.next_block_index += 1;
        index
    }

    fn get_poll_index(&mut self) -> usize {
        let index = self.next_poll_index;
        self.next_poll_index += 1;
        index
    }
}

impl Pass {
    fn new(block_index: usize) -> Self {
        Pass {
            block_index,
            used: false,
            disabled_reasons: Vec::new(),
        }
    }

    fn use_at(&mut self, span: &Span) -> Arc<process::Process<()>> {
        self.used = true;
        Arc::new(process::Process::Goto(
            span.clone(),
            self.block_index,
            Captures::new(),
        ))
    }
}

impl Pattern {
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

impl Spanning for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(span, _)
            | Self::List(span, _)
            | Self::Global(span, _)
            | Self::Variable(span, _)
            | Self::Poll { span, .. }
            | Self::Repoll { span, .. }
            | Self::Submit { span, .. }
            | Self::Condition(span, _)
            | Self::Grouped(span, _)
            | Self::TypeIn { span, .. }
            | Self::Let { span, .. }
            | Self::Catch { span, .. }
            | Self::Throw(span, _, _)
            | Self::If { span, .. }
            | Self::Do { span, .. }
            | Self::Box(span, _)
            | Self::Chan { span, .. }
            | Self::Application(span, _, _) => span.clone(),

            Self::Construction(construction) => construction.span(),
        }
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

impl Spanning for ConstructBranch {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _) | Self::Receive(span, _, _, _) | Self::ReceiveType(span, _, _) => {
                span.clone()
            }
        }
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

impl Spanning for Process {
    fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => span.clone(),
            Self::Poll { span, .. } => span.clone(),
            Self::Repoll { span, .. } => span.clone(),
            Self::Submit { span, .. } => span.clone(),
            Self::Catch { span, .. } => span.clone(),
            Self::Throw(span, _, _) => span.clone(),
            Self::If { span, .. } => span.clone(),
            Self::GlobalCommand(_, command) => command.span(),
            Self::Command(_, command) => command.span(),
            Self::Telltypes(span, _) => span.clone(),
            Self::Noop(span) => span.clone(),
        }
    }
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

#[derive(Clone)]
struct Restoration {
    span: Span,
    name: LocalName,
    value: Expression,
}

fn pattern_to_expression(pattern: &Pattern) -> Option<Expression> {
    match pattern {
        Pattern::Name(span, name, _) => Some(Expression::Variable(span.clone(), name.clone())),
        Pattern::Receive(span, first, rest, _vars) => {
            let first_expr = pattern_to_expression(first)?;
            let then = construct_from_pattern(rest)?;
            Some(Expression::Construction(Construct::Send(
                span.clone(),
                Box::new(first_expr),
                Box::new(then),
            )))
        }
        Pattern::Continue(span) => Some(Expression::Construction(Construct::Break(span.clone()))),
        Pattern::ReceiveType(_, _, _) | Pattern::Try(_, _, _) | Pattern::Default(_, _, _) => None,
    }
}

fn construct_from_pattern(pattern: &Pattern) -> Option<Construct> {
    match pattern {
        Pattern::Name(span, name, _) => Some(Construct::Then(Box::new(Expression::Variable(
            span.clone(),
            name.clone(),
        )))),
        Pattern::Receive(span, first, rest, _vars) => {
            let expression = pattern_to_expression(first)?;
            let then = construct_from_pattern(rest)?;
            Some(Construct::Send(
                span.clone(),
                Box::new(expression),
                Box::new(then),
            ))
        }
        Pattern::Continue(span) => Some(Construct::Break(span.clone())),
        Pattern::ReceiveType(_, _, _) | Pattern::Try(_, _, _) | Pattern::Default(_, _, _) => None,
    }
}

fn reconstruction_for_is(
    span: &Span,
    value: &Expression,
    variant: &LocalName,
    pattern: &Pattern,
) -> Option<Restoration> {
    let Expression::Variable(_, name) = value else {
        return None;
    };
    let payload = construct_from_pattern(pattern)?;
    let reconstruction = Expression::Construction(Construct::Signal(
        span.clone(),
        variant.clone(),
        Box::new(payload),
    ));
    Some(Restoration {
        span: span.clone(),
        name: name.clone(),
        value: reconstruction,
    })
}

fn collect_restorations(condition: &Condition) -> Vec<Restoration> {
    match condition {
        Condition::Bool(_, _) => Vec::new(),
        Condition::Is {
            span,
            value,
            variant,
            pattern,
        } => reconstruction_for_is(span, value, variant, pattern)
            .into_iter()
            .collect(),
        Condition::And(_, left, right) => {
            let mut restores = collect_restorations(left);
            restores.extend(collect_restorations(right));
            restores
        }
        Condition::Or(_, _, _) => Vec::new(),
        Condition::Not(_, _) => Vec::new(),
    }
}

fn link_process_from_expr(expr: &Expression) -> Process {
    Process::Command(
        LocalName::result(),
        Command::Link(expr.span(), Box::new(expr.clone())),
    )
}
