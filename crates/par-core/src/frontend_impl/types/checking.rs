use super::super::language::LocalName;
use super::super::process::{Captures, Command, Expression, PollKind, Process, VariableUsage};
use super::context::{PollPointScope, PollScope};
use super::core::{LoopId, Operation, Type, get_primitive_type};
use super::error::TypeError;
use super::lattice::union_types;
use super::{Context, TypeDefs};
use crate::frontend_impl::types::implicit::infer_holes;
use crate::frontend_impl::types::lattice::intersect_types;
use crate::location::Span;
use indexmap::{IndexMap, IndexSet};
use std::collections::BTreeMap;
use std::sync::Arc;

enum ProcessAnalyzerMode {
    Check,
    Infer(LocalName),
}
impl<S: Clone + Eq + std::hash::Hash> Context<S> {
    fn analyze_process(
        &mut self,
        process: &Process<(), S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Arc<Process<Type<S>, S>>, Option<Type<S>>), TypeError<S>> {
        match mode {
            ProcessAnalyzerMode::Check => {
                let process = self.check_process(process)?;
                Ok((process, None))
            }
            ProcessAnalyzerMode::Infer(inference_subject) => {
                let (process, typ) = self.infer_process(process, &inference_subject)?;
                Ok((process, Some(typ)))
            }
        }
    }
    pub(crate) fn check_process(
        &mut self,
        process: &Process<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        match process {
            Process::Let {
                span,
                name,
                annotation,
                typ: (),
                value: expression,
                then: process,
            } => match annotation {
                Some(annotated_type) => self.check_process_let_annotated(
                    span,
                    name,
                    annotation,
                    annotated_type,
                    expression,
                    process,
                ),
                None => {
                    self.check_process_let_inferred(span, name, annotation, expression, process)
                }
            },

            Process::Do {
                span,
                name: object,
                usage,
                typ: (),
                command,
            } => self.check_process_do(span, object, usage, command),

            Process::Poll {
                span,
                kind,
                driver,
                point,
                clients,
                name,
                name_typ: (),
                captures,
                then,
                else_,
            } => self.check_process_poll(
                span, kind, driver, point, clients, name, captures, then, else_,
            ),

            Process::Submit {
                span,
                driver,
                point,
                values,
                captures,
            } => self.check_process_submit(span, driver, point, values, captures),

            Process::Telltypes(span, _) => self.check_process_telltypes(span),

            Process::Unreachable(span) => self.check_process_unreachable(span),

            Process::Block(span, index, body, then) => {
                self.check_process_block(span, *index, body, then)
            }

            Process::Goto(span, index, caps) => self.check_process_goto(span, *index, caps),
        }
    }

    fn check_process_let_annotated(
        &mut self,
        span: &Span,
        name: &LocalName,
        annotation: &Option<Type<S>>,
        annotated_type: &Type<S>,
        expression: &Expression<(), S>,
        process: &Process<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        self.type_defs.validate_type(annotated_type)?;
        let expression = self.check_expression(None, expression, annotated_type)?;
        let typ = annotated_type.clone();
        self.put(span, name.clone(), typ.clone())?;
        let process = self.check_process(process)?;
        Ok(Arc::new(Process::Let {
            span: span.clone(),
            name: name.clone(),
            annotation: annotation.clone(),
            typ,
            value: expression,
            then: process,
        }))
    }

    fn check_process_let_inferred(
        &mut self,
        span: &Span,
        name: &LocalName,
        annotation: &Option<Type<S>>,
        expression: &Expression<(), S>,
        process: &Process<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let (expression, typ) = self.infer_expression(None, expression)?;
        self.put(span, name.clone(), typ.clone())?;
        let process = self.check_process(process)?;
        Ok(Arc::new(Process::Let {
            span: span.clone(),
            name: name.clone(),
            annotation: annotation.clone(),
            typ,
            value: expression,
            then: process,
        }))
    }

    fn check_process_do(
        &mut self,
        span: &Span,
        object: &LocalName,
        usage: &VariableUsage,
        command: &Command<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let typ = self.get_variable_or_error(span, object)?;
        let (command, _) = self.check_command(
            None,
            span,
            object,
            &typ,
            command,
            &ProcessAnalyzerMode::Check,
        )?;

        Ok(Arc::new(Process::Do {
            span: span.clone(),
            name: object.clone(),
            usage: usage.clone(),
            typ,
            command,
        }))
    }

    fn check_process_poll(
        &mut self,
        span: &Span,
        kind: &PollKind,
        driver: &LocalName,
        point: &LocalName,
        clients: &[Arc<Expression<(), S>>],
        name: &LocalName,
        captures: &Captures,
        then: &Process<(), S>,
        else_: &Process<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let is_repoll = matches!(kind, PollKind::Repoll);

        let preserved_vars: IndexMap<_, _> = self
            .variables
            .iter()
            .filter(|&(n, _)| captures.names.contains_key(n))
            .map(|(n, t)| (n.clone(), t.clone()))
            .collect();

        let mut typed_clients = Vec::with_capacity(clients.len());

        let mut base;
        let mut then_ctx;
        let name_typ;

        if is_repoll {
            let (poll_driver, poll_pool_type, poll_points, poll_current_point) =
                match self.poll.as_ref() {
                    Some(poll) => (
                        poll.driver.clone(),
                        poll.pool_type.clone(),
                        poll.points.clone(),
                        poll.current_point.clone(),
                    ),
                    None => return Err(TypeError::RepollOutsidePoll(span.clone())),
                };
            if poll_driver != *driver {
                return Err(TypeError::RepollOutsidePoll(span.clone()));
            }
            if self.get_variable(driver).is_none() {
                return Err(TypeError::RepollOutsidePoll(span.clone()));
            }

            let mut point_client_type = poll_points
                .get(&poll_current_point)
                .expect("current poll-point missing from poll scope")
                .client_type
                .clone();

            for client in clients {
                let (typed, typ) = self.infer_expression(None, client)?;
                typed_clients.push(typed);
                let mut typ = typ;
                loop {
                    let next = typ.expand_definition(&self.type_defs)?;
                    if next == typ {
                        break;
                    }
                    typ = next;
                }
                let Type::Recursive { .. } = typ else {
                    return Err(TypeError::PollClientMustBeRecursive(span.clone(), typ));
                };
                if !typ.is_assignable_to(&poll_pool_type, &self.type_defs)? {
                    return Err(TypeError::SubmittedClientNotAssignableToPoll(
                        span.clone(),
                        typ.clone(),
                        poll_pool_type.clone(),
                    ));
                }
                point_client_type = union_types(&self.type_defs, span, &point_client_type, &typ)?;
            }

            base = self.clone();

            let Type::Recursive {
                asc: point_asc,
                label: point_label,
                body: point_body,
                display_hint,
                ..
            } = point_client_type.clone()
            else {
                panic!("poll point client type must be recursive");
            };
            name_typ = Type::expand_recursive(
                &point_asc,
                &point_label,
                &point_body,
                display_hint.0.as_ref(),
            )?;

            let Some(base_poll) = base.poll.as_mut() else {
                panic!("repoll without a poll scope after validation");
            };
            if base_poll.driver != *driver {
                panic!("repoll driver does not match poll scope");
            }
            if base_poll
                .points
                .insert(
                    point.clone(),
                    PollPointScope {
                        client_type: point_client_type,
                        preserved: Arc::new(preserved_vars),
                    },
                )
                .is_some()
            {
                panic!("poll-point {} already registered", point);
            }
            base_poll.current_point = point.clone();

            then_ctx = base.clone();
        } else {
            if clients.is_empty() {
                return Err(TypeError::PollMustHaveAtLeastOneClient(span.clone()));
            }

            let mut client_type = None;
            for client in clients {
                let (typed, typ) = self.infer_expression(None, client)?;
                typed_clients.push(typed);
                client_type = Some(match client_type {
                    None => typ,
                    Some(prev) => union_types(&self.type_defs, span, &prev, &typ)?,
                });
            }

            let mut client_type = client_type.expect("clients is not empty");
            loop {
                let next = client_type.expand_definition(&self.type_defs)?;
                if next == client_type {
                    break;
                }
                client_type = next;
            }

            base = self.clone();

            let Type::Recursive {
                span: typ_span,
                asc,
                label,
                body,
                display_hint,
            } = client_type.clone()
            else {
                return Err(TypeError::PollClientMustBeRecursive(
                    span.clone(),
                    client_type,
                ));
            };

            let pool_type = client_type.clone();

            let mut asc = asc.clone();
            let loop_id = LoopId::new();
            asc.insert(loop_id);
            let point_client_type = Type::Recursive {
                span: typ_span.clone(),
                asc: asc.clone(),
                label: label.clone(),
                body: body.clone(),
                display_hint: display_hint.clone(),
            };

            name_typ = Type::expand_recursive(&asc, &label, &body, display_hint.0.as_ref())?;

            then_ctx = base.clone();
            let prev_poll = then_ctx.poll.take();
            if let Some(prev_poll) = &prev_poll {
                then_ctx.variables.shift_remove(&prev_poll.driver);
            }
            then_ctx.poll_stash.push(prev_poll);
            then_ctx.poll = Some(PollScope {
                driver: driver.clone(),
                pool_type,
                points: IndexMap::from([(
                    point.clone(),
                    PollPointScope {
                        client_type: point_client_type,
                        preserved: Arc::new(preserved_vars),
                    },
                )]),
                current_point: point.clone(),
                token_span: span.clone(),
            });
        }

        then_ctx.put(span, driver.clone(), Type::Continue(span.clone()))?;
        then_ctx.put(span, name.clone(), name_typ.clone())?;
        let typed_then = then_ctx.check_process(then)?;

        base.blocks = then_ctx.blocks.clone();

        let mut else_ctx = base;
        if is_repoll {
            let current = else_ctx
                .poll
                .take()
                .expect("repoll else branch must have a poll scope");
            if current.driver != *driver {
                panic!("repoll else branch driver mismatch");
            }
            else_ctx.variables.shift_remove(&current.driver);
            let prev = else_ctx.poll_stash.pop().unwrap_or(None);
            if let Some(prev_poll) = &prev {
                else_ctx.put(
                    &prev_poll.token_span,
                    prev_poll.driver.clone(),
                    Type::Continue(prev_poll.token_span.clone()),
                )?;
            }
            else_ctx.poll = prev;
        }

        let typed_else = else_ctx.check_process(else_)?;

        self.variables.clear();

        Ok(Arc::new(Process::Poll {
            span: span.clone(),
            kind: kind.clone(),
            driver: driver.clone(),
            point: point.clone(),
            clients: typed_clients,
            name: name.clone(),
            name_typ,
            captures: captures.clone(),
            then: typed_then,
            else_: typed_else,
        }))
    }

    fn check_process_submit(
        &mut self,
        span: &Span,
        driver: &LocalName,
        point: &LocalName,
        values: &[Arc<Expression<(), S>>],
        captures: &Captures,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let (poll_pool_type, current_point_client_type, poll_point_client_type, preserved_vars) =
            match self.poll.as_ref() {
                Some(poll) => {
                    if &poll.driver != driver {
                        panic!("submit driver does not match poll scope");
                    }
                    let preserved = poll
                        .points
                        .get(point)
                        .cloned()
                        .unwrap_or_else(|| panic!("submit to unknown poll-point: {point}"));
                    let current_point_client_type = poll
                        .points
                        .get(&poll.current_point)
                        .expect("current poll-point missing from poll scope")
                        .client_type
                        .clone();
                    (
                        poll.pool_type.clone(),
                        current_point_client_type,
                        preserved.client_type.clone(),
                        preserved.preserved.clone(),
                    )
                }
                None => return Err(TypeError::SubmitOutsidePoll(span.clone())),
            };

        if !current_point_client_type.is_assignable_to(&poll_point_client_type, &self.type_defs)? {
            return Err(TypeError::SubmitCannotTargetPollPoint(
                span.clone(),
                current_point_client_type,
                poll_point_client_type.clone(),
            ));
        }

        let mut typed_values = Vec::with_capacity(values.len());
        for value in values {
            let (typed, typ) = self.infer_expression(None, value)?;
            let mut typ = typ;
            loop {
                let next = typ.expand_definition(&self.type_defs)?;
                if next == typ {
                    break;
                }
                typ = next;
            }
            if !typ.is_assignable_to(&poll_pool_type, &self.type_defs)? {
                return Err(TypeError::SubmittedClientNotAssignableToPoll(
                    span.clone(),
                    typ.clone(),
                    poll_pool_type.clone(),
                ));
            }
            if !typ.is_assignable_to(&poll_point_client_type, &self.type_defs)? {
                return Err(TypeError::SubmittedClientDoesNotDescend(span.clone()));
            }
            typed_values.push(typed);
        }

        for (var, type_at_poll) in preserved_vars.iter() {
            let Some(current_type) = self.get_variable(var) else {
                return Err(TypeError::PollVariableNotPreserved(
                    span.clone(),
                    var.clone(),
                ));
            };
            if !current_type.is_assignable_to(type_at_poll, &self.type_defs)? {
                return Err(TypeError::PollVariableChangedType(
                    span.clone(),
                    var.clone(),
                    current_type,
                    type_at_poll.clone(),
                ));
            }
        }

        if self.get_variable(driver).is_none() {
            return Err(TypeError::SubmitOutsidePoll(span.clone()));
        }

        self.cannot_have_obligations(span)?;
        self.variables.clear();

        Ok(Arc::new(Process::Submit {
            span: span.clone(),
            driver: driver.clone(),
            point: point.clone(),
            values: typed_values,
            captures: captures.clone(),
        }))
    }

    fn check_process_telltypes(
        &mut self,
        span: &Span,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
    }

    fn check_process_unreachable(
        &mut self,
        span: &Span,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let impossible = Type::either(vec![]);
        let mut exhaustive = false;
        for typ in self.variables.values() {
            match typ.is_assignable_to(&impossible, &self.type_defs) {
                Ok(true) => {
                    exhaustive = true;
                    break;
                }
                Ok(false) => {}
                Err(error) => return Err(error),
            }
        }
        if !exhaustive {
            return Err(TypeError::NonExhaustiveIf(span.clone()));
        }
        self.variables.clear();
        Ok(Arc::new(Process::Unreachable(span.clone())))
    }

    fn check_process_block(
        &mut self,
        span: &Span,
        index: usize,
        body: &Process<(), S>,
        then: &Process<(), S>,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        if self.blocks.insert(index, Vec::new()).is_some() {
            panic!("block {} already defined", index);
        }
        let typed_then = self.check_process(then)?;
        let contexts = self
            .blocks
            .shift_remove(&index)
            .expect("block should have been registered");
        if contexts.is_empty() {
            panic!(
                "block has no incoming paths at index {} span {:?}",
                index, span
            );
        }
        let free = body.free_variables();
        let merged = merge_path_contexts(&self.type_defs, span, &contexts, &free)?;

        let saved = self.variables.clone();
        self.variables = merged;
        let typed_body = self.check_process(body)?;
        self.variables = saved;

        Ok(Arc::new(Process::Block(
            span.clone(),
            index,
            typed_body,
            typed_then,
        )))
    }

    fn check_process_goto(
        &mut self,
        span: &Span,
        index: usize,
        caps: &Captures,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        let entry = self.blocks.get_mut(&index).unwrap();
        entry.push(self.variables.clone());
        self.variables.clear();
        Ok(Arc::new(Process::Goto(span.clone(), index, caps.clone())))
    }

    fn check_command(
        &mut self,
        inference_subject: Option<&LocalName>,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        command: &Command<(), S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        if let Type::Name(_, name, args) = typ {
            return self.check_command(
                inference_subject,
                span,
                object,
                &self.type_defs.get(span, name, args)?,
                command,
                mode,
            );
        }
        if let Type::DualName(_, name, args) = typ {
            return self.check_command(
                inference_subject,
                span,
                object,
                &self.type_defs.get_dual(span, name, args)?,
                command,
                mode,
            );
        }
        if let Type::Box(_, inner) = typ {
            return self.check_command(inference_subject, span, object, inner, command, mode);
        }
        if let Type::DualBox(_, inner) = typ {
            if inner.is_positive(&self.type_defs)? {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &inner.clone().dual(Span::None),
                    command,
                    mode,
                );
            }
        }
        if !matches!(command, Command::Link(_)) {
            if let Type::Iterative {
                asc: top_asc,
                label: top_label,
                body,
                display_hint,
                ..
            } = typ
            {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_iterative(
                        span,
                        top_asc,
                        top_label,
                        body,
                        display_hint.0.as_ref(),
                    )?,
                    command,
                    mode,
                );
            }
        }
        if !matches!(command, Command::Begin { .. } | Command::Loop(_, _, _)) {
            if let Type::Recursive {
                asc: top_asc,
                label: top_label,
                body,
                display_hint,
                ..
            } = typ
            {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_recursive(top_asc, top_label, body, display_hint.0.as_ref())?,
                    command,
                    mode,
                );
            }
        }

        self.check_command_normalized(inference_subject, span, object, typ, command, mode)
    }

    fn check_command_normalized(
        &mut self,
        inference_subject: Option<&LocalName>,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        command: &Command<(), S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        match command {
            Command::Link(expression) => self.check_command_link(span, typ, expression),
            Command::Send(argument, process) => {
                self.check_command_send(span, object, typ, argument, process, mode)
            }
            Command::Receive(parameter, annotation, (), process, type_parameters) => self
                .check_command_receive(
                    span,
                    object,
                    typ,
                    parameter,
                    annotation,
                    process,
                    type_parameters,
                    mode,
                ),
            Command::Signal(chosen, process) => {
                self.check_command_signal(span, object, typ, chosen, process, mode)
            }
            Command::Case(branches, processes, else_process) => {
                self.check_command_case(span, object, typ, branches, processes, else_process, mode)
            }
            Command::Break => {
                let Type::Continue(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Break,
                        typ.clone(),
                    ));
                };
                self.cannot_have_obligations(span)?;
                Ok((Command::Break, None))
            }
            Command::Continue(process) => self.check_command_continue(span, typ, process, mode),
            Command::Begin {
                unfounded,
                label,
                captures,
                body: process,
            } => self.check_command_begin(
                inference_subject,
                span,
                object,
                typ,
                *unfounded,
                label,
                captures,
                process,
                mode,
            ),
            Command::Loop(label, driver, captures) => self.check_command_loop(
                inference_subject,
                span,
                object,
                typ,
                label,
                driver,
                captures,
            ),
            Command::SendType(argument, process) => {
                self.check_command_send_type(span, object, typ, argument, process, mode)
            }
            Command::ReceiveType(parameter, process) => {
                self.check_command_receive_type(span, object, typ, parameter, process, mode)
            }
        }
    }

    fn check_command_link(
        &mut self,
        span: &Span,
        typ: &Type<S>,
        expression: &Arc<Expression<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let expression = self.check_expression(None, expression, &typ.clone().dual(Span::None))?;
        self.cannot_have_obligations(span)?;
        Ok((Command::Link(expression), None))
    }

    fn check_command_send(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        argument: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Function(_, argument_type, then_type, vars) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Send,
                typ.clone(),
            ));
        };
        if vars.is_empty() {
            self.check_command_send_plain(
                span,
                object,
                argument,
                process,
                argument_type,
                then_type,
                mode,
            )
        } else {
            self.check_command_send_generic(
                span,
                object,
                argument,
                process,
                argument_type,
                then_type,
                vars,
                mode,
            )
        }
    }

    fn check_command_send_generic(
        &mut self,
        span: &Span,
        object: &LocalName,
        argument: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
        argument_type: &Type<S>,
        then_type: &Type<S>,
        vars: &Vec<LocalName>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let (argument, inferred_arg_type) = self.infer_expression(None, argument)?;
        let inferred_holes = infer_holes(
            span,
            &inferred_arg_type,
            argument_type,
            vars,
            &self.type_defs,
        )?;
        let then_type = then_type
            .clone()
            .substitute(inferred_holes.iter().map(|(k, v)| (k, v)).collect())?;
        self.finish_check_command_send(span, object, argument, process, then_type, mode)
    }

    fn check_command_send_plain(
        &mut self,
        span: &Span,
        object: &LocalName,
        argument: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
        argument_type: &Type<S>,
        then_type: &Type<S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let argument = self.check_expression(None, argument, argument_type)?;
        self.finish_check_command_send(span, object, argument, process, then_type.clone(), mode)
    }

    fn finish_check_command_send(
        &mut self,
        span: &Span,
        object: &LocalName,
        argument: Arc<Expression<Type<S>, S>>,
        process: &Arc<Process<(), S>>,
        then_type: Type<S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        self.put(span, object.clone(), then_type)?;
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((Command::Send(argument, process), inferred_types))
    }

    fn check_command_receive(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        parameter: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        type_parameters: &[LocalName],
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Pair(_, param_type, then_type, type_names) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Receive {
                    generics: type_parameters.len(),
                },
                typ.clone(),
            ));
        };

        if type_parameters.len() != type_names.len() {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Receive {
                    generics: type_parameters.len(),
                },
                typ.clone(),
            ));
        }

        if type_names.is_empty() {
            self.check_command_receive_plain(
                span,
                object,
                parameter,
                annotation,
                process,
                type_names,
                param_type,
                then_type,
                type_parameters,
                mode,
            )
        } else {
            self.check_command_receive_generic(
                span,
                object,
                parameter,
                annotation,
                process,
                type_names,
                param_type,
                then_type,
                type_parameters,
                mode,
            )
        }
    }

    fn check_command_receive_generic(
        &mut self,
        span: &Span,
        object: &LocalName,
        parameter: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        type_names: &[LocalName],
        param_type: &Type<S>,
        then_type: &Type<S>,
        type_parameters: &[LocalName],
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let type_vars: Vec<Type<S>> = type_parameters
            .iter()
            .map(|v| Type::Var(Span::None, v.clone()))
            .collect();
        let then_type = then_type
            .clone()
            .substitute(type_names.iter().zip(type_vars.iter()).collect())?;
        let param_type = param_type
            .clone()
            .substitute(type_names.iter().zip(type_vars.iter()).collect())?;
        self.finish_check_command_receive(
            span,
            object,
            parameter,
            annotation,
            process,
            type_names,
            type_parameters,
            param_type,
            then_type,
            mode,
        )
    }

    fn check_command_receive_plain(
        &mut self,
        span: &Span,
        object: &LocalName,
        parameter: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        type_names: &[LocalName],
        param_type: &Type<S>,
        then_type: &Type<S>,
        type_parameters: &[LocalName],
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        self.finish_check_command_receive(
            span,
            object,
            parameter,
            annotation,
            process,
            type_names,
            type_parameters,
            param_type.clone(),
            then_type.clone(),
            mode,
        )
    }

    fn finish_check_command_receive(
        &mut self,
        span: &Span,
        object: &LocalName,
        parameter: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        type_names: &[LocalName],
        type_parameters: &[LocalName],
        param_type: Type<S>,
        then_type: Type<S>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        self.type_defs.vars.extend(type_parameters.iter().cloned());

        if let Some(annotated_type) = annotation {
            self.type_defs.validate_type(annotated_type)?;
            param_type.check_assignable(span, annotated_type, &self.type_defs)?;
        }
        self.put(span, parameter.clone(), param_type.clone())?;
        self.put(span, object.clone(), then_type)?;
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((
            Command::Receive(
                parameter.clone(),
                annotation.clone(),
                param_type,
                process,
                type_names.to_vec(),
            ),
            inferred_types,
        ))
    }

    fn check_command_signal(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        chosen: &LocalName,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Choice(_, branches) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Signal,
                typ.clone(),
            ));
        };
        let Some(branch_type) = branches.get(chosen) else {
            return Err(TypeError::InvalidBranch(
                span.clone(),
                chosen.clone(),
                typ.clone(),
            ));
        };
        self.put(span, object.clone(), branch_type.clone())?;
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((Command::Signal(chosen.clone(), process), inferred_types))
    }

    fn check_command_case(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        branches: &Arc<[LocalName]>,
        processes: &Box<[Arc<Process<(), S>>]>,
        else_process: &Option<Arc<Process<(), S>>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Either(_, branch_types) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Case,
                typ.clone(),
            ));
        };

        let mut remaining_branches = branch_types.clone();

        let mut original_context = self.clone();
        let mut typed_processes = Vec::new();
        let mut inferred_type: Option<Type<S>> = None;

        for (branch, process) in branches.iter().zip(processes.iter()) {
            self.check_command_case_branch(
                span,
                object,
                typ,
                branch,
                process,
                &mut remaining_branches,
                &mut original_context,
                &mut typed_processes,
                &mut inferred_type,
                mode,
            )?;
        }

        let typed_else_process = match else_process {
            Some(process) => Some(self.check_command_case_else(
                span,
                object,
                &mut remaining_branches,
                &original_context,
                process,
                &mut inferred_type,
                mode,
            )?),
            None => None,
        };

        if let Some((missing, _)) = remaining_branches.pop_first() {
            return Err(TypeError::MissingBranch(
                span.clone(),
                missing.clone(),
                typ.clone(),
            ));
        }

        Ok((
            Command::Case(
                Arc::clone(branches),
                Box::from(typed_processes),
                typed_else_process,
            ),
            inferred_type,
        ))
    }

    fn check_command_case_branch(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        branch: &LocalName,
        process: &Arc<Process<(), S>>,
        remaining_branches: &mut BTreeMap<LocalName, Type<S>>,
        original_context: &mut Self,
        typed_processes: &mut Vec<Arc<Process<Type<S>, S>>>,
        inferred_type: &mut Option<Type<S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(), TypeError<S>> {
        *self = original_context.clone();

        let Some(branch_type) = remaining_branches.remove(branch) else {
            return Err(TypeError::RedundantBranch(
                span.clone(),
                branch.clone(),
                typ.clone(),
            ));
        };
        self.put(span, object.clone(), branch_type)?;
        let (process, inferred_in_branch) = self.analyze_process(process, mode)?;
        typed_processes.push(process);
        self.merge_command_case_inferred_type(span, inferred_type, inferred_in_branch)?;
        original_context.blocks = self.blocks.clone();
        Ok(())
    }

    fn check_command_case_else(
        &mut self,
        span: &Span,
        object: &LocalName,
        remaining_branches: &mut BTreeMap<LocalName, Type<S>>,
        original_context: &Self,
        process: &Arc<Process<(), S>>,
        inferred_type: &mut Option<Type<S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<Arc<Process<Type<S>, S>>, TypeError<S>> {
        *self = original_context.clone();
        let object_type = Type::Either(Span::None, std::mem::take(remaining_branches));
        self.put(span, object.clone(), object_type)?;
        let (process, inferred_in_branch) = self.analyze_process(process, mode)?;
        self.merge_command_case_inferred_type(span, inferred_type, inferred_in_branch)?;
        Ok(process)
    }

    fn merge_command_case_inferred_type(
        &self,
        span: &Span,
        inferred_type: &mut Option<Type<S>>,
        inferred_in_branch: Option<Type<S>>,
    ) -> Result<(), TypeError<S>> {
        *inferred_type = match (inferred_type.take(), inferred_in_branch) {
            (None, Some(t2)) => Some(t2),
            (Some(t1), Some(t2)) => Some(intersect_types(&self.type_defs, span, &t1, &t2)?),
            (t1, _) => t1,
        };
        Ok(())
    }

    fn check_command_continue(
        &mut self,
        span: &Span,
        typ: &Type<S>,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Break(_) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Continue,
                typ.clone(),
            ));
        };
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((Command::Continue(process), inferred_types))
    }

    fn check_command_begin(
        &mut self,
        inference_subject: Option<&LocalName>,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        unfounded: bool,
        label: &Option<LocalName>,
        captures: &Captures,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        if let Some(inference_subject) = inference_subject {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                inference_subject.clone(),
            ));
        }
        let Type::Recursive {
            span: typ_span,
            asc: typ_asc,
            label: typ_label,
            body: typ_body,
            display_hint,
        } = typ
        else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Begin,
                typ.clone(),
            ));
        };

        let mut typ_asc = typ_asc.clone();

        if !unfounded {
            let loop_id = LoopId::new();
            typ_asc.insert(loop_id);
        }
        self.loop_points.insert(
            label.clone(),
            (
                Type::Recursive {
                    span: typ_span.clone(),
                    asc: typ_asc.clone(),
                    label: typ_label.clone(),
                    body: typ_body.clone(),
                    display_hint: display_hint.clone(),
                },
                Arc::new(
                    self.variables
                        .iter()
                        .filter(|&(name, _)| captures.names.contains_key(name))
                        .map(|(name, typ)| (name.clone(), typ.clone()))
                        .collect::<IndexMap<_, _>>(),
                ),
            ),
        );

        self.put(
            span,
            object.clone(),
            Type::expand_recursive(&typ_asc, typ_label, typ_body, display_hint.0.as_ref())?,
        )?;
        let (process, _inferred_type) = self.analyze_process(process, mode)?;
        Ok((
            Command::Begin {
                unfounded,
                label: label.clone(),
                captures: captures.clone(),
                body: process,
            },
            None,
        ))
    }

    fn check_command_loop(
        &mut self,
        inference_subject: Option<&LocalName>,
        span: &Span,
        _object: &LocalName,
        typ: &Type<S>,
        label: &Option<LocalName>,
        driver: &LocalName,
        captures: &Captures,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        if !matches!(typ, Type::Recursive { .. }) {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::Loop,
                typ.clone(),
            ));
        }
        let Some((driver_type, variables)) = self.loop_points.get(label).cloned() else {
            return Err(TypeError::NoSuchLoopPoint(span.clone(), label.clone()));
        };
        self.put(span, driver.clone(), typ.clone())?;

        if let (Type::Recursive { asc: asc1, .. }, Type::Recursive { asc: asc2, .. }) =
            (typ, &driver_type)
        {
            for loop_id in asc2 {
                if !asc1.contains(loop_id) {
                    return Err(TypeError::DoesNotDescendSubjectOfBegin(
                        span.clone(),
                        loop_id.clone(),
                    ));
                }
            }
        }

        let mut inferred_loop = None;

        for (var, type_at_begin) in variables.iter().chain([(driver, &driver_type)]) {
            if Some(var) == inference_subject {
                inferred_loop = Some(type_at_begin.clone());
                continue;
            }
            let Some(current_type) = self.get_variable(var) else {
                return Err(TypeError::LoopVariableNotPreserved(
                    span.clone(),
                    var.clone(),
                ));
            };
            if !current_type.is_assignable_to(type_at_begin, &self.type_defs)? {
                return Err(TypeError::LoopVariableChangedType(
                    span.clone(),
                    var.clone(),
                    current_type,
                    type_at_begin.clone(),
                ));
            }
        }
        self.cannot_have_obligations(span)?;

        Ok((
            Command::Loop(label.clone(), driver.clone(), captures.clone()),
            inferred_loop.or(Some(Type::Self_(span.clone(), label.clone()))),
        ))
    }

    fn check_command_send_type(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        argument: &Type<S>,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Forall(_, type_name, then_type) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::SendType,
                typ.clone(),
            ));
        };
        let then_type = then_type
            .clone()
            .substitute(BTreeMap::from([(type_name, argument)]))?;
        self.put(span, object.clone(), then_type)?;
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((Command::SendType(argument.clone(), process), inferred_types))
    }

    fn check_command_receive_type(
        &mut self,
        span: &Span,
        object: &LocalName,
        typ: &Type<S>,
        parameter: &LocalName,
        process: &Arc<Process<(), S>>,
        mode: &ProcessAnalyzerMode,
    ) -> Result<(Command<Type<S>, S>, Option<Type<S>>), TypeError<S>> {
        let Type::Exists(_, type_name, then_type) = typ else {
            return Err(TypeError::InvalidOperation(
                span.clone(),
                Operation::ReceiveType,
                typ.clone(),
            ));
        };
        let then_type = then_type.clone().substitute(BTreeMap::from([(
            type_name,
            &Type::Var(span.clone(), parameter.clone()),
        )]))?;
        self.type_defs.vars.insert(parameter.clone());
        self.put(span, object.clone(), then_type)?;
        let (process, inferred_types) = self.analyze_process(process, mode)?;
        Ok((
            Command::ReceiveType(parameter.clone(), process),
            inferred_types,
        ))
    }

    pub(crate) fn infer_process(
        &mut self,
        process: &Process<(), S>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        match process {
            Process::Let {
                span,
                name,
                annotation,
                typ: (),
                value: expression,
                then: process,
            } => match annotation {
                Some(annotated_type) => self.infer_process_let_annotated(
                    span,
                    name,
                    annotation,
                    annotated_type,
                    expression,
                    process,
                    inference_subject,
                ),
                None => self.infer_process_let_inferred(
                    span,
                    name,
                    annotation,
                    expression,
                    process,
                    inference_subject,
                ),
            },

            Process::Do {
                span,
                name: object,
                usage,
                typ: (),
                command,
            } => self.infer_process_do(span, object, usage, command, inference_subject),

            Process::Poll {
                span,
                kind,
                driver,
                point,
                clients,
                name,
                name_typ: (),
                captures,
                then,
                else_,
            } => self.infer_process_poll(
                span,
                kind,
                driver,
                point,
                clients,
                name,
                captures,
                then,
                else_,
                inference_subject,
            ),

            Process::Submit {
                span,
                driver,
                point,
                values,
                captures,
            } => {
                self.infer_process_submit(span, driver, point, values, captures, inference_subject)
            }

            Process::Telltypes(span, _) => {
                Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
            }

            Process::Unreachable(span) => self.infer_process_unreachable(span),

            Process::Block(span, index, body, then) => {
                self.infer_process_block(span, *index, body, then, inference_subject)
            }

            Process::Goto(span, index, caps) => self.infer_process_goto(span, *index, caps),
        }
    }

    fn infer_process_let_annotated(
        &mut self,
        span: &Span,
        name: &LocalName,
        annotation: &Option<Type<S>>,
        annotated_type: &Type<S>,
        expression: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let expression =
            self.check_expression(Some(inference_subject), expression, annotated_type)?;
        self.finish_infer_process_let(
            span,
            name,
            annotation,
            annotated_type.clone(),
            expression,
            process,
            inference_subject,
        )
    }

    fn infer_process_let_inferred(
        &mut self,
        span: &Span,
        name: &LocalName,
        annotation: &Option<Type<S>>,
        expression: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let (expression, typ) = self.infer_expression(Some(inference_subject), expression)?;
        self.finish_infer_process_let(
            span,
            name,
            annotation,
            typ,
            expression,
            process,
            inference_subject,
        )
    }

    fn finish_infer_process_let(
        &mut self,
        span: &Span,
        name: &LocalName,
        annotation: &Option<Type<S>>,
        typ: Type<S>,
        expression: Arc<Expression<Type<S>, S>>,
        process: &Arc<Process<(), S>>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        self.put(span, name.clone(), typ.clone())?;
        let (process, subject_type) = self.infer_process(process, inference_subject)?;
        Ok((
            Arc::new(Process::Let {
                span: span.clone(),
                name: name.clone(),
                annotation: annotation.clone(),
                typ,
                value: expression,
                then: process,
            }),
            subject_type,
        ))
    }

    fn infer_process_do(
        &mut self,
        span: &Span,
        object: &LocalName,
        usage: &VariableUsage,
        command: &Command<(), S>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        if object == inference_subject {
            let (command, typ) = self.infer_command(span, inference_subject, command)?;
            return Ok((
                Arc::new(Process::Do {
                    span: span.clone(),
                    name: object.clone(),
                    usage: usage.clone(),
                    typ: typ.clone(),
                    command,
                }),
                typ,
            ));
        }
        let typ = self.get_variable_or_error(span, object)?;

        let (command, inferred_type) = self.check_command(
            Some(inference_subject),
            span,
            object,
            &typ,
            command,
            &ProcessAnalyzerMode::Infer(inference_subject.clone()),
        )?;

        let Some(inferred_type) = inferred_type else {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                inference_subject.clone(),
            ));
        };

        Ok((
            Arc::new(Process::Do {
                span: span.clone(),
                name: object.clone(),
                usage: usage.clone(),
                typ,
                command,
            }),
            inferred_type,
        ))
    }

    fn infer_process_poll(
        &mut self,
        span: &Span,
        kind: &PollKind,
        driver: &LocalName,
        point: &LocalName,
        clients: &[Arc<Expression<(), S>>],
        name: &LocalName,
        captures: &Captures,
        then: &Arc<Process<(), S>>,
        else_: &Arc<Process<(), S>>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let is_repoll = matches!(kind, PollKind::Repoll);

        let preserved_vars: IndexMap<_, _> = self
            .variables
            .iter()
            .filter(|&(n, _)| captures.names.contains_key(n))
            .map(|(n, t)| (n.clone(), t.clone()))
            .collect();

        let mut typed_clients = Vec::with_capacity(clients.len());

        let mut base;
        let mut then_ctx;
        let name_typ;

        if is_repoll {
            let (poll_driver, poll_pool_type, poll_points, poll_current_point) =
                match self.poll.as_ref() {
                    Some(poll) => (
                        poll.driver.clone(),
                        poll.pool_type.clone(),
                        poll.points.clone(),
                        poll.current_point.clone(),
                    ),
                    None => return Err(TypeError::RepollOutsidePoll(span.clone())),
                };
            if poll_driver != *driver {
                return Err(TypeError::RepollOutsidePoll(span.clone()));
            }

            if self.get_variable(driver).is_none() {
                return Err(TypeError::RepollOutsidePoll(span.clone()));
            }

            let mut point_client_type = poll_points
                .get(&poll_current_point)
                .expect("current poll-point missing from poll scope")
                .client_type
                .clone();

            for client in clients {
                let (typed, typ) = self.infer_expression(Some(inference_subject), client)?;
                typed_clients.push(typed);
                let mut typ = typ;
                loop {
                    let next = typ.expand_definition(&self.type_defs)?;
                    if next == typ {
                        break;
                    }
                    typ = next;
                }
                let Type::Recursive { .. } = typ else {
                    return Err(TypeError::PollClientMustBeRecursive(span.clone(), typ));
                };
                if !typ.is_assignable_to(&poll_pool_type, &self.type_defs)? {
                    return Err(TypeError::SubmittedClientNotAssignableToPoll(
                        span.clone(),
                        typ.clone(),
                        poll_pool_type.clone(),
                    ));
                }
                point_client_type = union_types(&self.type_defs, span, &point_client_type, &typ)?;
            }

            base = self.clone();

            let Type::Recursive {
                asc: point_asc,
                label: point_label,
                body: point_body,
                display_hint,
                ..
            } = point_client_type.clone()
            else {
                panic!("poll point client type must be recursive");
            };
            name_typ = Type::expand_recursive(
                &point_asc,
                &point_label,
                &point_body,
                display_hint.0.as_ref(),
            )?;

            let Some(base_poll) = base.poll.as_mut() else {
                panic!("repoll without a poll scope after validation");
            };
            if base_poll.driver != *driver {
                panic!("repoll driver does not match poll scope");
            }
            if base_poll
                .points
                .insert(
                    point.clone(),
                    PollPointScope {
                        client_type: point_client_type,
                        preserved: Arc::new(preserved_vars),
                    },
                )
                .is_some()
            {
                panic!("poll-point {} already registered", point);
            }
            base_poll.current_point = point.clone();

            then_ctx = base.clone();
        } else {
            if clients.is_empty() {
                return Err(TypeError::PollMustHaveAtLeastOneClient(span.clone()));
            }

            let mut client_type = None;
            for client in clients {
                let (client_expr, typ) = self.infer_expression(Some(inference_subject), client)?;
                typed_clients.push(client_expr);
                client_type = Some(match client_type {
                    None => typ,
                    Some(prev) => union_types(&self.type_defs, span, &prev, &typ)?,
                });
            }

            let mut client_type = client_type.expect("clients is not empty");
            loop {
                let next = client_type.expand_definition(&self.type_defs)?;
                if next == client_type {
                    break;
                }
                client_type = next;
            }

            base = self.clone();

            let Type::Recursive {
                span: typ_span,
                asc,
                label,
                body,
                display_hint,
            } = client_type.clone()
            else {
                return Err(TypeError::PollClientMustBeRecursive(
                    span.clone(),
                    client_type,
                ));
            };

            let pool_type = client_type.clone();

            let mut asc = asc.clone();
            let loop_id = LoopId::new();
            asc.insert(loop_id);
            let point_client_type = Type::Recursive {
                span: typ_span.clone(),
                asc: asc.clone(),
                label: label.clone(),
                body: body.clone(),
                display_hint: display_hint.clone(),
            };

            name_typ = Type::expand_recursive(&asc, &label, &body, display_hint.0.as_ref())?;

            then_ctx = base.clone();
            let prev_poll = then_ctx.poll.take();
            if let Some(prev_poll) = &prev_poll {
                then_ctx.variables.shift_remove(&prev_poll.driver);
            }
            then_ctx.poll_stash.push(prev_poll);
            then_ctx.poll = Some(PollScope {
                driver: driver.clone(),
                pool_type,
                points: IndexMap::from([(
                    point.clone(),
                    PollPointScope {
                        client_type: point_client_type,
                        preserved: Arc::new(preserved_vars),
                    },
                )]),
                current_point: point.clone(),
                token_span: span.clone(),
            });
        }

        then_ctx.put(span, driver.clone(), Type::Continue(span.clone()))?;
        then_ctx.put(span, name.clone(), name_typ.clone())?;
        let (typed_then, then_type) = then_ctx.infer_process(then, inference_subject)?;

        base.blocks = then_ctx.blocks.clone();

        let mut else_ctx = base;
        if is_repoll {
            let current = else_ctx
                .poll
                .take()
                .expect("repoll else branch must have a poll scope");
            if current.driver != *driver {
                panic!("repoll else branch driver mismatch");
            }
            else_ctx.variables.shift_remove(&current.driver);
            let prev = else_ctx.poll_stash.pop().unwrap_or(None);
            if let Some(prev_poll) = &prev {
                else_ctx.put(
                    &prev_poll.token_span,
                    prev_poll.driver.clone(),
                    Type::Continue(prev_poll.token_span.clone()),
                )?;
            }
            else_ctx.poll = prev;
        }

        let (typed_else, else_type) = else_ctx.infer_process(else_, inference_subject)?;

        self.variables.clear();

        Ok((
            Arc::new(Process::Poll {
                span: span.clone(),
                kind: kind.clone(),
                driver: driver.clone(),
                point: point.clone(),
                clients: typed_clients,
                name: name.clone(),
                name_typ,
                captures: captures.clone(),
                then: typed_then,
                else_: typed_else,
            }),
            intersect_types(&self.type_defs, span, &then_type, &else_type)?,
        ))
    }

    fn infer_process_submit(
        &mut self,
        span: &Span,
        driver: &LocalName,
        point: &LocalName,
        values: &[Arc<Expression<(), S>>],
        captures: &Captures,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let (poll_pool_type, current_point_client_type, poll_point_client_type, preserved_vars) =
            match self.poll.as_ref() {
                Some(poll) => {
                    if &poll.driver != driver {
                        panic!("submit driver does not match poll scope");
                    }
                    let preserved = poll
                        .points
                        .get(point)
                        .cloned()
                        .unwrap_or_else(|| panic!("submit to unknown poll-point: {point}"));
                    let current_point_client_type = poll
                        .points
                        .get(&poll.current_point)
                        .expect("current poll-point missing from poll scope")
                        .client_type
                        .clone();
                    (
                        poll.pool_type.clone(),
                        current_point_client_type,
                        preserved.client_type.clone(),
                        preserved.preserved.clone(),
                    )
                }
                None => return Err(TypeError::SubmitOutsidePoll(span.clone())),
            };

        if !current_point_client_type.is_assignable_to(&poll_point_client_type, &self.type_defs)? {
            return Err(TypeError::SubmitCannotTargetPollPoint(
                span.clone(),
                current_point_client_type,
                poll_point_client_type.clone(),
            ));
        }

        let mut typed_values = Vec::with_capacity(values.len());
        for value in values {
            let (typed, typ) = self.infer_expression(Some(inference_subject), value)?;
            let mut typ = typ;
            loop {
                let next = typ.expand_definition(&self.type_defs)?;
                if next == typ {
                    break;
                }
                typ = next;
            }
            if !typ.is_assignable_to(&poll_pool_type, &self.type_defs)? {
                return Err(TypeError::SubmittedClientNotAssignableToPoll(
                    span.clone(),
                    typ.clone(),
                    poll_pool_type.clone(),
                ));
            }
            if !typ.is_assignable_to(&poll_point_client_type, &self.type_defs)? {
                return Err(TypeError::SubmittedClientDoesNotDescend(span.clone()));
            }
            typed_values.push(typed);
        }

        for (var, type_at_poll) in preserved_vars.iter() {
            let Some(current_type) = self.get_variable(var) else {
                return Err(TypeError::PollVariableNotPreserved(
                    span.clone(),
                    var.clone(),
                ));
            };
            if !current_type.is_assignable_to(type_at_poll, &self.type_defs)? {
                return Err(TypeError::PollVariableChangedType(
                    span.clone(),
                    var.clone(),
                    current_type,
                    type_at_poll.clone(),
                ));
            }
        }

        if self.get_variable(driver).is_none() {
            return Err(TypeError::SubmitOutsidePoll(span.clone()));
        }

        self.cannot_have_obligations(span)?;
        self.variables.clear();

        Ok((
            Arc::new(Process::Submit {
                span: span.clone(),
                driver: driver.clone(),
                point: point.clone(),
                values: typed_values,
                captures: captures.clone(),
            }),
            Type::choice(vec![]),
        ))
    }

    fn infer_process_unreachable(
        &mut self,
        span: &Span,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let impossible = Type::either(vec![]);
        let mut exhaustive = false;
        for typ in self.variables.values() {
            match typ.is_assignable_to(&impossible, &self.type_defs) {
                Ok(true) => {
                    exhaustive = true;
                    break;
                }
                Ok(false) => {}
                Err(e) => return Err(e),
            }
        }
        if !exhaustive {
            return Err(TypeError::NonExhaustiveIf(span.clone()));
        }
        self.variables.clear();
        Ok((
            Arc::new(Process::Unreachable(span.clone())),
            Type::choice(vec![]),
        ))
    }

    fn infer_process_block(
        &mut self,
        span: &Span,
        index: usize,
        body: &Arc<Process<(), S>>,
        then: &Arc<Process<(), S>>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        if self.blocks.insert(index, Vec::new()).is_some() {
            panic!("block {} already defined", index);
        }
        let (typed_then, then_type) = self.infer_process(then, inference_subject)?;
        let contexts = self
            .blocks
            .shift_remove(&index)
            .expect("block should have been registered");
        if contexts.is_empty() {
            panic!("block has no incoming paths");
        }
        let free = body.free_variables();
        let contexts: Vec<_> = contexts
            .into_iter()
            .map(|mut ctx| {
                ctx.shift_remove(inference_subject);
                ctx
            })
            .collect();
        let merged = merge_path_contexts(&self.type_defs, span, &contexts, &free)?;

        let saved = self.variables.clone();
        self.variables = merged;
        let (typed_body, body_type) = self.infer_process(body, inference_subject)?;
        self.variables = saved;

        let final_type = intersect_types(&self.type_defs, span, &then_type, &body_type)?;

        Ok((
            Arc::new(Process::Block(span.clone(), index, typed_body, typed_then)),
            final_type,
        ))
    }

    fn infer_process_goto(
        &mut self,
        span: &Span,
        index: usize,
        caps: &Captures,
    ) -> Result<(Arc<Process<Type<S>, S>>, Type<S>), TypeError<S>> {
        let entry = self.blocks.get_mut(&index).unwrap();
        entry.push(self.variables.clone());
        self.variables.clear();
        Ok((
            Arc::new(Process::Goto(span.clone(), index, caps.clone())),
            Type::choice(vec![]),
        ))
    }

    pub(crate) fn infer_command(
        &mut self,
        span: &Span,
        subject: &LocalName,
        command: &Command<(), S>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        match command {
            Command::Link(expression) => self.infer_command_link(span, subject, expression),
            Command::Send(argument, process) => {
                self.infer_command_send(span, subject, argument, process)
            }
            Command::Receive(parameter, annotation, (), process, vars) => {
                self.infer_command_receive(span, subject, parameter, annotation, process, vars)
            }
            Command::Signal(chosen, process) => {
                self.infer_command_signal(span, subject, chosen, process)
            }
            Command::Case(branches, processes, else_process) => {
                self.infer_command_case(span, subject, branches, processes, else_process)
            }
            Command::Break => {
                self.cannot_have_obligations(span)?;
                Ok((Command::Break, Type::Continue(span.clone())))
            }
            Command::Continue(process) => self.infer_command_continue(span, process),
            Command::Begin { .. } => Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                subject.clone(),
            )),
            Command::Loop(label, driver, captures) => {
                self.infer_command_loop(span, label, driver, captures)
            }
            Command::SendType(_, _) => Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                subject.clone(),
            )),
            Command::ReceiveType(parameter, process) => {
                self.infer_command_receive_type(span, subject, parameter, process)
            }
        }
    }

    fn infer_command_link(
        &mut self,
        span: &Span,
        subject: &LocalName,
        expression: &Arc<Expression<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        let (expression, typ) = self.infer_expression(Some(subject), expression)?;
        self.cannot_have_obligations(span)?;
        Ok((Command::Link(expression), typ.dual(Span::None)))
    }

    fn infer_command_send(
        &mut self,
        span: &Span,
        subject: &LocalName,
        argument: &Arc<Expression<(), S>>,
        process: &Arc<Process<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        let (argument, arg_type) = self.infer_expression(Some(subject), argument)?;
        let (process, then_type) = self.infer_process(process, subject)?;
        Ok((
            Command::Send(argument, process),
            Type::Function(
                span.clone(),
                Box::new(arg_type),
                Box::new(then_type),
                vec![],
            ),
        ))
    }

    fn infer_command_receive(
        &mut self,
        span: &Span,
        subject: &LocalName,
        parameter: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        vars: &[LocalName],
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        for var in vars.iter() {
            self.type_defs.vars.insert(var.clone());
        }
        let Some(param_type) = annotation else {
            return Err(TypeError::ParameterTypeMustBeKnown(
                span.clone(),
                parameter.clone(),
            ));
        };
        self.put(span, parameter.clone(), param_type.clone())?;
        self.type_defs.vars.extend(vars.iter().cloned());
        let (process, then_type) = self.infer_process(process, subject)?;
        Ok((
            Command::Receive(
                parameter.clone(),
                annotation.clone(),
                param_type.clone(),
                process,
                vars.to_vec(),
            ),
            Type::Pair(
                span.clone(),
                Box::new(param_type.clone()),
                Box::new(then_type),
                vars.to_vec(),
            ),
        ))
    }

    fn infer_command_signal(
        &mut self,
        span: &Span,
        subject: &LocalName,
        chosen: &LocalName,
        process: &Arc<Process<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        let (process, then_type) = self.infer_process(process, subject)?;
        Ok((
            Command::Signal(chosen.clone(), process),
            Type::Choice(span.clone(), BTreeMap::from([(chosen.clone(), then_type)])),
        ))
    }

    fn infer_command_case(
        &mut self,
        span: &Span,
        subject: &LocalName,
        branches: &Arc<[LocalName]>,
        processes: &Box<[Arc<Process<(), S>>]>,
        else_process: &Option<Arc<Process<(), S>>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        if else_process.is_some() {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                subject.clone(),
            ));
        }

        let mut original_context = self.clone();
        let mut typed_processes = Vec::new();
        let mut branch_types = BTreeMap::new();

        for (branch, process) in branches.iter().zip(processes.iter()) {
            *self = original_context.clone();
            let (process, typ) = self.infer_process(process, subject)?;
            typed_processes.push(process);
            branch_types.insert(branch.clone(), typ);
            original_context.blocks = self.blocks.clone();
        }

        Ok((
            Command::Case(Arc::clone(branches), Box::from(typed_processes), None),
            Type::Either(span.clone(), branch_types),
        ))
    }

    fn infer_command_continue(
        &mut self,
        span: &Span,
        process: &Arc<Process<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        let process = self.check_process(process)?;
        Ok((Command::Continue(process), Type::Break(span.clone())))
    }

    fn infer_command_loop(
        &mut self,
        span: &Span,
        label: &Option<LocalName>,
        driver: &LocalName,
        captures: &Captures,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        let Some((driver_type, variables)) = self.loop_points.get(label).cloned() else {
            return Err(TypeError::NoSuchLoopPoint(span.clone(), label.clone()));
        };

        for (var, type_at_begin) in variables.as_ref() {
            let Some(current_type) = self.get_variable(var) else {
                return Err(TypeError::LoopVariableNotPreserved(
                    span.clone(),
                    var.clone(),
                ));
            };
            if !current_type.is_assignable_to(type_at_begin, &self.type_defs)? {
                return Err(TypeError::LoopVariableChangedType(
                    span.clone(),
                    var.clone(),
                    current_type,
                    type_at_begin.clone(),
                ));
            }
        }
        self.cannot_have_obligations(span)?;

        Ok((
            Command::Loop(label.clone(), driver.clone(), captures.clone()),
            driver_type,
        ))
    }

    fn infer_command_receive_type(
        &mut self,
        span: &Span,
        subject: &LocalName,
        parameter: &LocalName,
        process: &Arc<Process<(), S>>,
    ) -> Result<(Command<Type<S>, S>, Type<S>), TypeError<S>> {
        self.type_defs.vars.insert(parameter.clone());
        let (process, then_type) = self.infer_process(process, subject)?;
        Ok((
            Command::ReceiveType(parameter.clone(), process),
            Type::Exists(span.clone(), parameter.clone(), Box::new(then_type)),
        ))
    }

    pub(crate) fn check_expression(
        &mut self,
        inference_subject: Option<&LocalName>,
        expression: &Expression<(), S>,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        match expression {
            Expression::Global(span, name, ()) => {
                self.check_expression_global(span, name, target_type)
            }
            Expression::Variable(span, name, (), usage) => {
                self.check_expression_variable(span, name, usage, inference_subject, target_type)
            }
            Expression::Box(span, captures, expression, ()) => self.check_expression_box(
                span,
                captures,
                expression,
                inference_subject,
                target_type,
            ),
            Expression::Chan {
                span,
                captures,
                chan_name: channel,
                chan_annotation: annotation,
                process,
                ..
            } => self.check_expression_chan(
                span,
                captures,
                channel,
                annotation,
                process,
                inference_subject,
                target_type,
            ),
            Expression::Primitive(span, value, ()) => {
                self.check_expression_primitive(span, value, target_type)
            }
            Expression::External(f, ()) => self.check_expression_external(f, target_type),
        }
    }

    pub(crate) fn infer_expression(
        &mut self,
        inference_subject: Option<&LocalName>,
        expression: &Expression<(), S>,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        match expression {
            Expression::Global(span, name, ()) => self.infer_expression_global(span, name),
            Expression::Variable(span, name, (), usage) => {
                self.infer_expression_variable(span, name, usage, inference_subject)
            }
            Expression::Box(span, captures, expression, ()) => {
                self.infer_expression_box(span, captures, expression, inference_subject)
            }
            Expression::Chan {
                span,
                captures,
                chan_name: channel,
                chan_annotation: annotation,
                process,
                ..
            } => self.infer_expression_chan(
                span,
                captures,
                channel,
                annotation,
                process,
                inference_subject,
            ),
            Expression::Primitive(span, value, ()) => self.infer_expression_primitive(span, value),
            Expression::External(_f, ()) => self.infer_expression_external(),
        }
    }

    fn check_expression_global(
        &mut self,
        span: &Span,
        name: &super::super::language::GlobalName<S>,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        let typ = self.get_global(span, name)?;
        typ.check_assignable(span, target_type, &self.type_defs)?;
        Ok(Arc::new(Expression::Global(
            span.clone(),
            name.clone(),
            typ.clone(),
        )))
    }

    fn check_expression_variable(
        &mut self,
        span: &Span,
        name: &LocalName,
        usage: &VariableUsage,
        inference_subject: Option<&LocalName>,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        if Some(name) == inference_subject {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                name.clone(),
            ));
        }

        let typ = self.get_variable_or_error(span, name)?;
        typ.check_assignable(span, target_type, &self.type_defs)?;
        if !typ.is_linear(&self.type_defs)? {
            self.put(span, name.clone(), typ.clone())?;
        }
        Ok(Arc::new(Expression::Variable(
            span.clone(),
            name.clone(),
            typ.clone(),
            usage.clone(),
        )))
    }

    fn check_expression_box(
        &mut self,
        span: &Span,
        captures: &Captures,
        expression: &Arc<Expression<(), S>>,
        inference_subject: Option<&LocalName>,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        if let Some(inference_subject) = inference_subject {
            if captures.names.contains_key(inference_subject) {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    inference_subject.clone(),
                ));
            }
        }
        let mut context = self.split();
        self.capture(inference_subject, captures, true, &mut context)?;
        let mut target_inner_type = target_type.clone();
        loop {
            match target_inner_type.expand_definition(&self.type_defs)? {
                Type::Box(_, inner) => target_inner_type = *inner,
                Type::Recursive {
                    span: _,
                    asc,
                    label,
                    body,
                    display_hint,
                } => {
                    target_inner_type =
                        Type::expand_recursive(&asc, &label, &body, display_hint.0.as_ref())?;
                }
                Type::Iterative {
                    span,
                    asc,
                    label,
                    body,
                    display_hint,
                } => {
                    target_inner_type = Type::expand_iterative(
                        &span,
                        &asc,
                        &label,
                        &body,
                        display_hint.0.as_ref(),
                    )?;
                }
                _ => break,
            }
        }
        let expression =
            self.check_expression(inference_subject, expression, &target_inner_type)?;
        Ok(Arc::new(Expression::Box(
            span.clone(),
            captures.clone(),
            expression,
            target_type.clone(),
        )))
    }

    fn check_expression_chan(
        &mut self,
        span: &Span,
        captures: &Captures,
        channel: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        inference_subject: Option<&LocalName>,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        let target_dual = target_type.clone().dual(Span::None);
        let (chan_type, expr_type) = match annotation {
            Some(annotated_type) => {
                self.type_defs.validate_type(annotated_type)?;
                annotated_type.check_assignable(span, &target_dual, &self.type_defs)?;
                (annotated_type.clone(), target_type)
            }
            None => (target_dual, target_type),
        };
        let mut context = self.split();
        self.capture(inference_subject, captures, false, &mut context)?;
        context.put(span, channel.clone(), chan_type.clone())?;
        let process = context.check_process(process)?;
        Ok(Arc::new(Expression::Chan {
            span: span.clone(),
            captures: captures.clone(),
            chan_name: channel.clone(),
            chan_annotation: annotation.clone(),
            chan_type,
            expr_type: expr_type.clone(),
            process,
        }))
    }

    fn check_expression_primitive(
        &mut self,
        span: &Span,
        value: &par_runtime::primitive::Primitive,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        let typ = get_primitive_type(value);
        typ.check_assignable(span, target_type, &self.type_defs)?;
        Ok(Arc::new(Expression::Primitive(
            span.clone(),
            value.clone(),
            typ,
        )))
    }

    fn check_expression_external(
        &mut self,
        f: &par_runtime::linker::Unlinked,
        target_type: &Type<S>,
    ) -> Result<Arc<Expression<Type<S>, S>>, TypeError<S>> {
        Ok(Arc::new(Expression::External(
            f.clone(),
            target_type.clone(),
        )))
    }

    fn infer_expression_global(
        &mut self,
        span: &Span,
        name: &super::super::language::GlobalName<S>,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        let typ = self.get_global(span, name)?;
        Ok((
            Arc::new(Expression::Global(span.clone(), name.clone(), typ.clone())),
            typ.clone(),
        ))
    }

    fn infer_expression_variable(
        &mut self,
        span: &Span,
        name: &LocalName,
        usage: &VariableUsage,
        inference_subject: Option<&LocalName>,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        if Some(name) == inference_subject {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                span.clone(),
                name.clone(),
            ));
        }
        let typ = self.get_variable_or_error(span, name)?;
        if !typ.is_linear(&self.type_defs)? {
            self.put(span, name.clone(), typ.clone())?;
        }
        Ok((
            Arc::new(Expression::Variable(
                span.clone(),
                name.clone(),
                typ.clone(),
                usage.clone(),
            )),
            typ,
        ))
    }

    fn infer_expression_box(
        &mut self,
        span: &Span,
        captures: &Captures,
        expression: &Arc<Expression<(), S>>,
        inference_subject: Option<&LocalName>,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        if let Some(inference_subject) = inference_subject {
            if captures.names.contains_key(inference_subject) {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    inference_subject.clone(),
                ));
            }
        }
        let mut context = self.split();
        self.capture(inference_subject, captures, true, &mut context)?;
        let (expression, typ) = self.infer_expression(inference_subject, expression)?;
        let typ = Type::Box(span.clone(), Box::new(typ.clone()));
        Ok((
            Arc::new(Expression::Box(
                span.clone(),
                captures.clone(),
                expression,
                typ.clone(),
            )),
            typ,
        ))
    }

    fn infer_expression_chan(
        &mut self,
        span: &Span,
        captures: &Captures,
        channel: &LocalName,
        annotation: &Option<Type<S>>,
        process: &Arc<Process<(), S>>,
        inference_subject: Option<&LocalName>,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        let mut context = self.split();
        self.capture(inference_subject, captures, false, &mut context)?;
        let (process, typ) = match annotation {
            Some(typ) => {
                self.type_defs.validate_type(typ)?;
                context.put(span, channel.clone(), typ.clone())?;
                (context.check_process(process)?, typ.clone())
            }
            None => context.infer_process(process, channel)?,
        };
        let dual = typ.clone().dual(Span::None);
        Ok((
            Arc::new(Expression::Chan {
                span: span.clone(),
                captures: captures.clone(),
                chan_name: channel.clone(),
                chan_annotation: annotation.clone(),
                chan_type: typ,
                expr_type: dual.clone(),
                process,
            }),
            dual,
        ))
    }

    fn infer_expression_primitive(
        &mut self,
        span: &Span,
        value: &par_runtime::primitive::Primitive,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        let typ = get_primitive_type(value);
        Ok((
            Arc::new(Expression::Primitive(
                span.clone(),
                value.clone(),
                typ.clone(),
            )),
            typ,
        ))
    }

    fn infer_expression_external(
        &mut self,
    ) -> Result<(Arc<Expression<Type<S>, S>>, Type<S>), TypeError<S>> {
        Err(TypeError::TypeMustBeKnownAtThisPoint(
            Span::None,
            LocalName::error(),
        ))
    }
}

fn merge_path_contexts<S: Clone + Eq + std::hash::Hash>(
    typedefs: &TypeDefs<S>,
    span: &Span,
    paths: &Vec<IndexMap<LocalName, Type<S>>>,
    free_vars: &IndexSet<LocalName>,
) -> Result<IndexMap<LocalName, Type<S>>, TypeError<S>> {
    // Collect all variable names present in any path.
    let mut all_names: IndexSet<LocalName> = IndexSet::new();
    for map in paths {
        all_names.extend(map.keys().cloned());
    }

    let mut merged_variables = IndexMap::new();
    for name in all_names {
        let used = free_vars.contains(&name);
        let mut present_types: Vec<Type<S>> = Vec::new();
        let mut missing = false;
        for map in paths {
            if let Some(t) = map.get(&name) {
                present_types.push(t.clone());
            } else {
                missing = true;
            }
        }

        let is_linear = present_types
            .iter()
            .any(|t| t.is_linear(typedefs).unwrap_or(true));

        let is_absurd = present_types.iter().any(|t| {
            t.is_assignable_to(&Type::either(vec![]), typedefs)
                .unwrap_or(false)
        });

        if !used && !is_linear && !is_absurd {
            // Drop it.
            continue;
        }

        // Variable used or linear: must be present everywhere.
        if missing {
            return Err(TypeError::MergeVariableMissing(span.clone(), name.clone()));
        }

        let mut acc = present_types
            .get(0)
            .cloned()
            .expect("at least one type when not missing");
        for next in present_types.iter().skip(1) {
            acc = match union_types(typedefs, span, &acc, next) {
                Ok(t) => t,
                Err(_) => {
                    return Err(TypeError::MergeVariableTypesCannotBeUnified(
                        span.clone(),
                        name.clone(),
                        acc.clone(),
                        next.clone(),
                    ));
                }
            };
        }
        merged_variables.insert(name.clone(), acc);
    }
    Ok(merged_variables)
}
