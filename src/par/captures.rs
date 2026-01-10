use super::{
    language::LocalName,
    process::{Command, Expression, Process},
};
use crate::location::Span;
use indexmap::IndexMap;
use std::{collections::VecDeque, sync::Arc};

#[derive(Clone, Debug)]
pub enum VariableUsage {
    Unknown,
    Copy,
    Move,
}

#[derive(Clone, Debug)]
pub struct Captures {
    pub names: IndexMap<LocalName, (Span, VariableUsage)>,
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

    pub fn single(name: LocalName, span: Span, usage: VariableUsage) -> Self {
        let mut caps = Self::new();
        caps.add(name, span, usage);
        caps
    }

    pub fn extend(&mut self, other: Self) {
        for (name, span) in other.names {
            self.names.insert(name, span);
        }
    }

    pub fn add(&mut self, name: LocalName, span: Span, usage: VariableUsage) {
        self.names.insert(name, (span, usage));
    }

    pub fn remove(&mut self, name: &LocalName) -> Option<(Span, VariableUsage)> {
        self.names.shift_remove(name)
    }

    pub fn contains(&self, name: &LocalName) -> bool {
        self.names.contains_key(name)
    }

    fn merge_missing(&mut self, other: &Captures) -> bool {
        let mut changed = false;
        for (name, (span, usage)) in &other.names {
            if !self.names.contains_key(name) {
                self.names
                    .insert(name.clone(), (span.clone(), usage.clone()));
                changed = true;
            }
        }
        changed
    }
}

type BeginId = Span;

#[derive(Clone, Debug, Default)]
struct LoopEnv {
    labels: IndexMap<Option<LocalName>, BeginId>,
}

impl LoopEnv {
    fn with_begin(&self, label: &Option<LocalName>, id: BeginId) -> Self {
        let mut env = self.clone();
        env.labels.insert(label.clone(), id);
        env
    }

    fn resolve(&self, label: &Option<LocalName>) -> Option<BeginId> {
        self.labels.get(label).cloned()
    }

    fn intersect_in_place(&mut self, other: &LoopEnv) -> bool {
        let mut changed = false;
        self.labels.retain(|label, id| {
            let keep = other.labels.get(label) == Some(id);
            if !keep {
                changed = true;
            }
            keep
        });
        changed
    }
}

#[derive(Clone, Debug)]
struct CaptureAnalysis {
    begin_drivers: IndexMap<BeginId, LocalName>,
    block_envs: IndexMap<usize, LoopEnv>,
    begin_caps: IndexMap<BeginId, Captures>,
    block_caps: IndexMap<usize, Captures>,
}

impl CaptureAnalysis {
    fn from_process(process: &Process<()>) -> Self {
        let (block_envs, begin_drivers) = BlockEnvAnalyzer::analyze_process(process);
        let (begin_caps, block_caps) = compute_captures_from_process(process, &block_envs);
        CaptureAnalysis {
            begin_drivers,
            block_envs,
            begin_caps,
            block_caps,
        }
    }

    fn from_expression(expression: &Expression<()>) -> Self {
        let (block_envs, begin_drivers) = BlockEnvAnalyzer::analyze_expression(expression);
        let (begin_caps, block_caps) = compute_captures_from_expression(expression, &block_envs);
        CaptureAnalysis {
            begin_drivers,
            block_envs,
            begin_caps,
            block_caps,
        }
    }

    fn fix_process(&self, process: &Process<()>, env: &LoopEnv) -> (Arc<Process<()>>, Captures) {
        match process {
            Process::Let {
                span,
                name,
                annotation,
                typ,
                value,
                then,
            } => {
                let (then, mut caps) = self.fix_process(then, env);
                caps.remove(name);
                let (value, caps1) = self.fix_expression(value, env, &caps);
                caps.extend(caps1);
                (
                    Arc::new(Process::Let {
                        span: span.clone(),
                        name: name.clone(),
                        annotation: annotation.clone(),
                        typ: typ.clone(),
                        value,
                        then,
                    }),
                    caps,
                )
            }
            Process::Do {
                span,
                name,
                usage: _usage,
                typ,
                command,
            } => {
                let (command, mut caps) = self.fix_command(command, span, env);
                let usage = if caps.contains(name) {
                    VariableUsage::Copy
                } else {
                    VariableUsage::Move
                };
                caps.add(name.clone(), span.clone(), VariableUsage::Unknown);
                (
                    Arc::new(Process::Do {
                        span: span.clone(),
                        name: name.clone(),
                        usage,
                        typ: typ.clone(),
                        command,
                    }),
                    caps,
                )
            }
            Process::Telltypes(span, process) => {
                let (process, caps) = self.fix_process(process, env);
                (Arc::new(Process::Telltypes(span.clone(), process)), caps)
            }
            Process::Block(span, index, body, process) => {
                let (process, caps) = self.fix_process(process, env);
                let body_env = self.block_envs.get(index).cloned().unwrap_or_default();
                let (body, _body_caps) = self.fix_process(body, &body_env);
                (
                    Arc::new(Process::Block(span.clone(), *index, body, process)),
                    caps,
                )
            }
            Process::Goto(span, index, _) => {
                let caps = self.block_caps.get(index).cloned().unwrap_or_default();
                (
                    Arc::new(Process::Goto(span.clone(), *index, caps.clone())),
                    caps,
                )
            }
            Process::Unreachable(span) => (
                Arc::new(Process::Unreachable(span.clone())),
                Captures::new(),
            ),
        }
    }

    fn fix_command(
        &self,
        command: &Command<()>,
        span: &Span,
        env: &LoopEnv,
    ) -> (Command<()>, Captures) {
        match command {
            Command::Link(expression) => {
                let (expression, caps) = self.fix_expression(expression, env, &Captures::new());
                (Command::Link(expression), caps)
            }
            Command::Send(argument, process) => {
                let (process, mut caps) = self.fix_process(process, env);
                let (argument, caps1) = self.fix_expression(argument, env, &caps);
                caps.extend(caps1);
                (Command::Send(argument, process), caps)
            }
            Command::Receive(parameter, annotation, typ, process, vars) => {
                let (process, mut caps) = self.fix_process(process, env);
                caps.remove(parameter);
                (
                    Command::Receive(
                        parameter.clone(),
                        annotation.clone(),
                        typ.clone(),
                        process,
                        vars.clone(),
                    ),
                    caps,
                )
            }
            Command::Signal(chosen, process) => {
                let (process, caps) = self.fix_process(process, env);
                (Command::Signal(chosen.clone(), process), caps)
            }
            Command::Case(branches, processes, else_process) => {
                let mut fixed_processes = Vec::new();
                let mut caps = Captures::new();
                for process in processes {
                    let (process, caps1) = self.fix_process(process, env);
                    fixed_processes.push(process);
                    caps.extend(caps1);
                }
                let fixed_else = else_process.clone().map(|process| {
                    let (process, caps1) = self.fix_process(&process, env);
                    caps.extend(caps1);
                    process
                });
                (
                    Command::Case(
                        branches.clone(),
                        fixed_processes.into_boxed_slice(),
                        fixed_else,
                    ),
                    caps,
                )
            }
            Command::Break => (Command::Break, Captures::new()),
            Command::Continue(process) => {
                let (process, caps) = self.fix_process(process, env);
                (Command::Continue(process), caps)
            }
            Command::Begin {
                unfounded,
                label,
                captures: _,
                body,
            } => {
                let begin_id = span.clone();
                let env = env.with_begin(label, begin_id.clone());
                let (process, caps) = self.fix_process(body, &env);
                let loop_caps = self.begin_caps.get(&begin_id).cloned().unwrap_or_default();
                (
                    Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone(),
                        captures: loop_caps,
                        body: process,
                    },
                    caps,
                )
            }
            Command::Loop(label, _, _) => {
                if let Some(begin_id) = env.resolve(label) {
                    let driver = self
                        .begin_drivers
                        .get(&begin_id)
                        .cloned()
                        .unwrap_or_else(LocalName::invalid);
                    let loop_caps = self.begin_caps.get(&begin_id).cloned().unwrap_or_default();
                    (
                        Command::Loop(label.clone(), driver, loop_caps.clone()),
                        loop_caps,
                    )
                } else {
                    (
                        Command::Loop(label.clone(), LocalName::invalid(), Captures::new()),
                        Captures::new(),
                    )
                }
            }
            Command::SendType(argument, process) => {
                let (process, caps) = self.fix_process(process, env);
                (Command::SendType(argument.clone(), process), caps)
            }
            Command::ReceiveType(parameter, process) => {
                let (process, caps) = self.fix_process(process, env);
                (Command::ReceiveType(parameter.clone(), process), caps)
            }
        }
    }

    fn fix_expression(
        &self,
        expression: &Expression<()>,
        env: &LoopEnv,
        later_captures: &Captures,
    ) -> (Arc<Expression<()>>, Captures) {
        match expression {
            Expression::Global(span, name, typ) => (
                Arc::new(Expression::Global(span.clone(), name.clone(), typ.clone())),
                Captures::new(),
            ),
            Expression::Variable(span, name, typ, _usage) => {
                let usage = if later_captures.contains(name) {
                    VariableUsage::Copy
                } else {
                    VariableUsage::Move
                };
                (
                    Arc::new(Expression::Variable(
                        span.clone(),
                        name.clone(),
                        typ.clone(),
                        usage,
                    )),
                    Captures::single(name.clone(), span.clone(), VariableUsage::Unknown),
                )
            }
            Expression::Box(span, _caps, expression, typ) => {
                let (expression, mut caps) = self.fix_expression(expression, env, later_captures);
                for (name, (_span, usage)) in caps.names.iter_mut() {
                    if later_captures.contains(name) {
                        *usage = VariableUsage::Copy;
                    } else {
                        *usage = VariableUsage::Move;
                    }
                }
                (
                    Arc::new(Expression::Box(
                        span.clone(),
                        caps.clone(),
                        expression,
                        typ.clone(),
                    )),
                    caps,
                )
            }
            Expression::Chan {
                span,
                chan_name,
                chan_annotation,
                chan_type,
                expr_type,
                process,
                ..
            } => {
                let (process, mut caps) = self.fix_process(process, env);
                caps.remove(chan_name);
                for (name, (_span, usage)) in caps.names.iter_mut() {
                    if later_captures.contains(name) {
                        *usage = VariableUsage::Copy;
                    } else {
                        *usage = VariableUsage::Move;
                    }
                }
                (
                    Arc::new(Expression::Chan {
                        span: span.clone(),
                        captures: caps.clone(),
                        chan_name: chan_name.clone(),
                        chan_annotation: chan_annotation.clone(),
                        chan_type: chan_type.clone(),
                        expr_type: expr_type.clone(),
                        process,
                    }),
                    caps,
                )
            }
            Expression::Primitive(span, value, typ) => (
                Arc::new(Expression::Primitive(
                    span.clone(),
                    value.clone(),
                    typ.clone(),
                )),
                Captures::new(),
            ),
            Expression::External(claimed_type, f, typ) => (
                Arc::new(Expression::External(claimed_type.clone(), *f, typ.clone())),
                Captures::new(),
            ),
        }
    }
}

struct BlockEnvAnalyzer {
    blocks: IndexMap<usize, Arc<Process<()>>>,
    begin_drivers: IndexMap<BeginId, LocalName>,
    block_envs: IndexMap<usize, LoopEnv>,
    queue: VecDeque<(Arc<Process<()>>, LoopEnv)>,
}

impl BlockEnvAnalyzer {
    fn new() -> Self {
        Self {
            blocks: IndexMap::new(),
            begin_drivers: IndexMap::new(),
            block_envs: IndexMap::new(),
            queue: VecDeque::new(),
        }
    }

    fn analyze_process(
        process: &Process<()>,
    ) -> (IndexMap<usize, LoopEnv>, IndexMap<BeginId, LocalName>) {
        let mut analyzer = Self::new();
        analyzer.visit_process(process, &LoopEnv::default());
        analyzer.run();
        (analyzer.block_envs, analyzer.begin_drivers)
    }

    fn analyze_expression(
        expression: &Expression<()>,
    ) -> (IndexMap<usize, LoopEnv>, IndexMap<BeginId, LocalName>) {
        let mut analyzer = Self::new();
        analyzer.visit_expression(expression, &LoopEnv::default());
        analyzer.run();
        (analyzer.block_envs, analyzer.begin_drivers)
    }

    fn run(&mut self) {
        while let Some((process, env)) = self.queue.pop_front() {
            self.visit_process(&process, &env);
        }
    }

    fn visit_expression(&mut self, expression: &Expression<()>, env: &LoopEnv) {
        match expression {
            Expression::Box(_, _, expr, _) => self.visit_expression(expr, env),
            Expression::Chan { process, .. } => self.visit_process(process, env),
            Expression::Global(_, _, _)
            | Expression::Variable(_, _, _, _)
            | Expression::Primitive(_, _, _)
            | Expression::External(_, _, _) => {}
        }
    }

    fn visit_process(&mut self, process: &Process<()>, env: &LoopEnv) {
        match process {
            Process::Let { value, then, .. } => {
                self.visit_expression(value, env);
                self.visit_process(then, env);
            }
            Process::Do {
                span,
                name,
                command,
                ..
            } => {
                self.visit_command(command, span, name, env);
            }
            Process::Telltypes(_, process) => {
                self.visit_process(process, env);
            }
            Process::Block(_, index, body, process) => {
                self.blocks
                    .entry(*index)
                    .or_insert_with(|| Arc::clone(body));
                self.visit_process(process, env);
            }
            Process::Goto(_, index, _) => {
                self.schedule_block(*index, env);
            }
            Process::Unreachable(_) => {}
        }
    }

    fn visit_command(
        &mut self,
        command: &Command<()>,
        span: &Span,
        subject: &LocalName,
        env: &LoopEnv,
    ) {
        match command {
            Command::Link(expression) => self.visit_expression(expression, env),
            Command::Send(argument, process) => {
                self.visit_expression(argument, env);
                self.visit_process(process, env);
            }
            Command::Receive(_, _annotation, _typ, process, _vars) => {
                self.visit_process(process, env);
            }
            Command::Signal(_, process) => {
                self.visit_process(process, env);
            }
            Command::Case(_, processes, else_process) => {
                for process in processes {
                    self.visit_process(process, env);
                }
                if let Some(process) = else_process {
                    self.visit_process(process, env);
                }
            }
            Command::Break => {}
            Command::Continue(process) => {
                self.visit_process(process, env);
            }
            Command::Begin { label, body, .. } => {
                let begin_id = span.clone();
                self.begin_drivers
                    .entry(begin_id.clone())
                    .or_insert_with(|| subject.clone());
                let env = env.with_begin(label, begin_id);
                self.visit_process(body, &env);
            }
            Command::Loop(_, _, _) => {}
            Command::SendType(_, process) => {
                self.visit_process(process, env);
            }
            Command::ReceiveType(_, process) => {
                self.visit_process(process, env);
            }
        }
    }

    fn schedule_block(&mut self, index: usize, env: &LoopEnv) {
        let changed = match self.block_envs.get_mut(&index) {
            None => {
                self.block_envs.insert(index, env.clone());
                true
            }
            Some(existing) => existing.intersect_in_place(env),
        };
        if changed {
            if let Some(body) = self.blocks.get(&index) {
                let env = self.block_envs.get(&index).cloned().unwrap_or_default();
                self.queue.push_back((Arc::clone(body), env));
            }
        }
    }
}

fn compute_captures_from_process(
    process: &Process<()>,
    block_envs: &IndexMap<usize, LoopEnv>,
) -> (IndexMap<BeginId, Captures>, IndexMap<usize, Captures>) {
    compute_captures(
        |collector| {
            collector.process_captures(process, &LoopEnv::default());
        },
        block_envs,
    )
}

fn compute_captures_from_expression(
    expression: &Expression<()>,
    block_envs: &IndexMap<usize, LoopEnv>,
) -> (IndexMap<BeginId, Captures>, IndexMap<usize, Captures>) {
    compute_captures(
        |collector| {
            collector.expression_captures(expression, &LoopEnv::default());
        },
        block_envs,
    )
}

fn compute_captures(
    f: impl Fn(&mut CaptureCollector<'_>),
    block_envs: &IndexMap<usize, LoopEnv>,
) -> (IndexMap<BeginId, Captures>, IndexMap<usize, Captures>) {
    let mut begin_caps = IndexMap::new();
    let mut block_caps = IndexMap::new();
    loop {
        let mut next_begin_caps = begin_caps.clone();
        let mut next_block_caps = block_caps.clone();
        let mut changed = false;
        {
            let mut collector = CaptureCollector {
                block_envs,
                old_begin_caps: &begin_caps,
                old_block_caps: &block_caps,
                next_begin_caps: &mut next_begin_caps,
                next_block_caps: &mut next_block_caps,
                changed: &mut changed,
            };
            f(&mut collector);
        }
        if !changed {
            return (next_begin_caps, next_block_caps);
        }
        begin_caps = next_begin_caps;
        block_caps = next_block_caps;
    }
}

struct CaptureCollector<'a> {
    block_envs: &'a IndexMap<usize, LoopEnv>,
    old_begin_caps: &'a IndexMap<BeginId, Captures>,
    old_block_caps: &'a IndexMap<usize, Captures>,
    next_begin_caps: &'a mut IndexMap<BeginId, Captures>,
    next_block_caps: &'a mut IndexMap<usize, Captures>,
    changed: &'a mut bool,
}

impl<'a> CaptureCollector<'a> {
    fn expression_captures(&mut self, expression: &Expression<()>, env: &LoopEnv) -> Captures {
        match expression {
            Expression::Global(_, _, _) => Captures::new(),
            Expression::Variable(span, name, _, _) => {
                Captures::single(name.clone(), span.clone(), VariableUsage::Unknown)
            }
            Expression::Box(_, _, expression, _) => self.expression_captures(expression, env),
            Expression::Chan {
                chan_name, process, ..
            } => {
                let mut caps = self.process_captures(process, env);
                caps.remove(chan_name);
                caps
            }
            Expression::Primitive(_, _, _) => Captures::new(),
            Expression::External(_, _, _) => Captures::new(),
        }
    }

    fn process_captures(&mut self, process: &Process<()>, env: &LoopEnv) -> Captures {
        match process {
            Process::Let {
                name, value, then, ..
            } => {
                let mut caps = self.process_captures(then, env);
                caps.remove(name);
                let expr_caps = self.expression_captures(value, env);
                caps.merge_missing(&expr_caps);
                caps
            }
            Process::Do {
                span,
                name,
                command,
                ..
            } => {
                let mut caps = self.command_captures(command, span, name, env);
                caps.add(name.clone(), span.clone(), VariableUsage::Unknown);
                caps
            }
            Process::Telltypes(_, process) => self.process_captures(process, env),
            Process::Block(_span, index, body, process) => {
                let body_env = self.block_envs.get(index).cloned().unwrap_or_default();
                let body_caps = self.process_captures(body, &body_env);
                self.update_block_caps(*index, &body_caps);
                self.process_captures(process, env)
            }
            Process::Goto(_, index, _) => {
                self.old_block_caps.get(index).cloned().unwrap_or_default()
            }
            Process::Unreachable(_) => Captures::new(),
        }
    }

    fn command_captures(
        &mut self,
        command: &Command<()>,
        span: &Span,
        subject: &LocalName,
        env: &LoopEnv,
    ) -> Captures {
        match command {
            Command::Link(expression) => self.expression_captures(expression, env),
            Command::Send(argument, process) => {
                let mut caps = self.process_captures(process, env);
                let arg_caps = self.expression_captures(argument, env);
                caps.merge_missing(&arg_caps);
                caps
            }
            Command::Receive(parameter, _annotation, _typ, process, _vars) => {
                let mut caps = self.process_captures(process, env);
                caps.remove(parameter);
                caps
            }
            Command::Signal(_, process) => self.process_captures(process, env),
            Command::Case(_, processes, else_process) => {
                let mut caps = Captures::new();
                for process in processes {
                    let branch_caps = self.process_captures(process, env);
                    caps.merge_missing(&branch_caps);
                }
                if let Some(process) = else_process {
                    let else_caps = self.process_captures(process, env);
                    caps.merge_missing(&else_caps);
                }
                caps
            }
            Command::Break => Captures::new(),
            Command::Continue(process) => self.process_captures(process, env),
            Command::Begin { label, body, .. } => {
                let begin_id = span.clone();
                let env = env.with_begin(label, begin_id.clone());
                let body_caps = self.process_captures(body, &env);
                let mut loop_caps = body_caps.clone();
                loop_caps.remove(subject);
                self.update_begin_caps(begin_id, &loop_caps);
                body_caps
            }
            Command::Loop(label, _, _) => env
                .resolve(label)
                .and_then(|id| self.old_begin_caps.get(&id).cloned())
                .unwrap_or_default(),
            Command::SendType(_, process) => self.process_captures(process, env),
            Command::ReceiveType(_, process) => self.process_captures(process, env),
        }
    }

    fn update_begin_caps(&mut self, id: BeginId, caps: &Captures) {
        let entry = self.next_begin_caps.entry(id).or_insert_with(Captures::new);
        if entry.merge_missing(caps) {
            *self.changed = true;
        }
    }

    fn update_block_caps(&mut self, index: usize, caps: &Captures) {
        let entry = self
            .next_block_caps
            .entry(index)
            .or_insert_with(Captures::new);
        if entry.merge_missing(caps) {
            *self.changed = true;
        }
    }
}

impl Process<()> {
    pub fn fix_captures(&self) -> (Arc<Self>, Captures) {
        let analysis = CaptureAnalysis::from_process(self);
        analysis.fix_process(self, &LoopEnv::default())
    }
}

impl Expression<()> {
    pub fn fix_captures(&self) -> (Arc<Self>, Captures) {
        let analysis = CaptureAnalysis::from_expression(self);
        analysis.fix_expression(self, &LoopEnv::default(), &Captures::new())
    }
}
