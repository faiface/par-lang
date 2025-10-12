use super::super::language::LocalName;
use super::super::process::{Command, Expression, Process};
use super::core::{LoopId, Operation, Type};
use super::error::TypeError;
use super::Context;
use crate::location::Span;
use crate::par::types::lattice::intersect_types;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

impl Context {
    pub fn check_process(
        &mut self,
        process: &Process<()>,
    ) -> Result<Arc<Process<Type>>, TypeError> {
        match process {
            Process::Let {
                span,
                name,
                annotation,
                typ: (),
                value: expression,
                then: process,
            } => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => {
                        // Validate annotation before using it
                        self.type_defs.validate_type(annotated_type)?;
                        (
                            self.check_expression(None, expression, annotated_type)?,
                            annotated_type.clone(),
                        )
                    }
                    None => self.infer_expression(None, expression)?,
                };
                self.put(span, name.clone(), typ.clone())?;
                let process = self.check_process(process)?;
                Ok(Arc::new(Process::Let {
                    span: span.clone(),
                    name: name.clone(),
                    annotation: annotation.clone(),
                    typ: typ,
                    value: expression,
                    then: process,
                }))
            }

            Process::Do {
                span,
                name: object,
                usage,
                typ: (),
                command,
            } => {
                let typ = self.get_variable_or_error(span, object)?;

                let (command, _) = self.check_command(
                    None,
                    span,
                    object,
                    &typ,
                    command,
                    &mut |context, process| Ok((context.check_process(process)?, None)),
                )?;

                Ok(Arc::new(Process::Do {
                    span: span.clone(),
                    name: object.clone(),
                    usage: usage.clone(),
                    typ: typ,
                    command: command,
                }))
            }

            Process::Telltypes(span, _) => {
                Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
            }
        }
    }

    fn check_command(
        &mut self,
        inference_subject: Option<&LocalName>,
        span: &Span,
        object: &LocalName,
        typ: &Type,
        command: &Command<()>,
        analyze_process: &mut impl FnMut(
            &mut Self,
            &Process<()>,
        )
            -> Result<(Arc<Process<Type>>, Option<Type>), TypeError>,
    ) -> Result<(Command<Type>, Option<Type>), TypeError> {
        if let Type::Name(_, name, args) = typ {
            return self.check_command(
                inference_subject,
                span,
                object,
                &self.type_defs.get(span, name, args)?,
                command,
                analyze_process,
            );
        }
        if let Type::DualName(_, name, args) = typ {
            return self.check_command(
                inference_subject,
                span,
                object,
                &self.type_defs.get_dual(span, name, args)?,
                command,
                analyze_process,
            );
        }
        if let Type::Box(_, inner) = typ {
            return self.check_command(
                inference_subject,
                span,
                object,
                inner,
                command,
                analyze_process,
            );
        }
        if let Type::DualBox(_, inner) = typ {
            if inner.is_positive(&self.type_defs)? {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &inner.clone().dual(Span::None),
                    command,
                    analyze_process,
                );
            }
        }
        if !matches!(command, Command::Link(_)) {
            if let Type::Iterative {
                asc: top_asc,
                label: top_label,
                body,
                ..
            } = typ
            {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_iterative(span, top_asc, top_label, body)?,
                    command,
                    analyze_process,
                );
            }
        }
        if !matches!(command, Command::Begin { .. } | Command::Loop(_, _, _)) {
            if let Type::Recursive {
                asc: top_asc,
                label: top_label,
                body,
                ..
            } = typ
            {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_recursive(top_asc, top_label, body)?,
                    command,
                    analyze_process,
                );
            }
        }

        Ok(match command {
            Command::Link(expression) => {
                let expression =
                    self.check_expression(None, expression, &typ.clone().dual(Span::None))?;
                self.cannot_have_obligations(span)?;
                (Command::Link(expression), None)
            }

            Command::Send(argument, process) => {
                let Type::Function(_, argument_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Send,
                        typ.clone(),
                    ));
                };
                let argument = self.check_expression(None, argument, &argument_type)?;
                self.put(span, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Send(argument, process), inferred_types)
            }

            Command::Receive(parameter, annotation, (), process) => {
                let Type::Pair(_, parameter_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Receive,
                        typ.clone(),
                    ));
                };
                if let Some(annotated_type) = annotation {
                    // Validate annotation before using it
                    self.type_defs.validate_type(annotated_type)?;
                    parameter_type.check_assignable(span, annotated_type, &self.type_defs)?;
                }
                self.put(span, parameter.clone(), *parameter_type.clone())?;
                self.put(span, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    Command::Receive(
                        parameter.clone(),
                        annotation.clone(),
                        *parameter_type.clone(),
                        process,
                    ),
                    inferred_types,
                )
            }

            Command::Signal(chosen, process) => {
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
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Signal(chosen.clone(), process), inferred_types)
            }

            Command::Case(branches, processes) => {
                let Type::Either(_, required_branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Case,
                        typ.clone(),
                    ));
                };
                if let Some(missing) = required_branches
                    .keys()
                    .find(|&branch| !branches.contains(branch))
                {
                    return Err(TypeError::MissingBranch(
                        span.clone(),
                        missing.clone(),
                        typ.clone(),
                    ));
                }

                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut inferred_type: Option<Type> = None;

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();

                    let Some(branch_type) = required_branches.get(branch) else {
                        return Err(TypeError::RedundantBranch(
                            span.clone(),
                            branch.clone(),
                            typ.clone(),
                        ));
                    };
                    self.put(span, object.clone(), branch_type.clone())?;
                    let (process, inferred_in_branch) = analyze_process(self, process)?;
                    typed_processes.push(process);

                    match (inferred_type, inferred_in_branch) {
                        (None, Some(t2)) => inferred_type = Some(t2),
                        (Some(t1), Some(t2)) => {
                            inferred_type = Some(intersect_types(&self.type_defs, span, &t1, &t2)?);
                        }
                        (t1, _) => inferred_type = t1,
                    }
                }
                (
                    Command::Case(Arc::clone(branches), Box::from(typed_processes)),
                    inferred_type,
                )
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
                (Command::Break, None)
            }

            Command::Continue(process) => {
                let Type::Break(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Continue,
                        typ.clone(),
                    ));
                };
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Continue(process), inferred_types)
            }

            Command::Begin {
                unfounded,
                label,
                captures,
                body: process,
            } => {
                let Type::Recursive {
                    span: typ_span,
                    asc: typ_asc,
                    label: typ_label,
                    body: typ_body,
                } = typ
                else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Begin,
                        typ.clone(),
                    ));
                };

                let mut typ_asc = typ_asc.clone();

                if !*unfounded {
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
                    Type::expand_recursive(&typ_asc, typ_label, typ_body)?,
                )?;
                let (process, inferred_type) = analyze_process(self, process)?;

                let inferred_type = inferred_type.map(|body| {
                    if body.contains_self(label) {
                        Type::Iterative {
                            span: span.clone(),
                            asc: typ_asc,
                            label: label.clone(),
                            body: Box::new(body),
                        }
                    } else {
                        body
                    }
                });

                (
                    Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone(),
                        captures: captures.clone(),
                        body: process,
                    },
                    inferred_type,
                )
            }

            Command::Loop(label, driver, captures) => {
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

                (
                    Command::Loop(label.clone(), driver.clone(), captures.clone()),
                    inferred_loop.or(Some(Type::Self_(span.clone(), label.clone()))),
                )
            }

            Command::SendType(argument, process) => {
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
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::SendType(argument.clone(), process), inferred_types)
            }

            Command::ReceiveType(parameter, process) => {
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
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    Command::ReceiveType(parameter.clone(), process),
                    inferred_types,
                )
            }
        })
    }

    pub fn infer_process(
        &mut self,
        process: &Process<()>,
        inference_subject: &LocalName,
    ) -> Result<(Arc<Process<Type>>, Type), TypeError> {
        match process {
            Process::Let {
                span,
                name,
                annotation,
                typ: (),
                value: expression,
                then: process,
            } => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(Some(inference_subject), expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(Some(inference_subject), expression)?,
                };
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

            Process::Do {
                span,
                name: object,
                usage,
                typ: (),
                command,
            } => {
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
                    &mut |context, process| {
                        let (process, typ) = context.infer_process(process, inference_subject)?;
                        Ok((process, Some(typ)))
                    },
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

            Process::Telltypes(span, _) => {
                Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
            }
        }
    }

    pub fn infer_command(
        &mut self,
        span: &Span,
        subject: &LocalName,
        command: &Command<()>,
    ) -> Result<(Command<Type>, Type), TypeError> {
        Ok(match command {
            Command::Link(expression) => {
                let (expression, typ) = self.infer_expression(Some(subject), expression)?;
                self.cannot_have_obligations(span)?;
                (Command::Link(expression), typ.dual(Span::None))
            }

            Command::Send(argument, process) => {
                let (argument, arg_type) = self.infer_expression(Some(subject), argument)?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Send(argument, process),
                    Type::Function(span.clone(), Box::new(arg_type), Box::new(then_type)),
                )
            }

            Command::Receive(parameter, annotation, (), process) => {
                let Some(param_type) = annotation else {
                    return Err(TypeError::ParameterTypeMustBeKnown(
                        span.clone(),
                        parameter.clone(),
                    ));
                };
                self.put(span, parameter.clone(), param_type.clone())?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Receive(
                        parameter.clone(),
                        annotation.clone(),
                        param_type.clone(),
                        process,
                    ),
                    Type::Pair(
                        span.clone(),
                        Box::new(param_type.clone()),
                        Box::new(then_type),
                    ),
                )
            }

            Command::Signal(chosen, process) => {
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Signal(chosen.clone(), process),
                    Type::Choice(span.clone(), BTreeMap::from([(chosen.clone(), then_type)])),
                )
            }

            Command::Case(branches, processes) => {
                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut branch_types = BTreeMap::new();

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();
                    let (process, typ) = self.infer_process(process, subject)?;
                    typed_processes.push(process);
                    branch_types.insert(branch.clone(), typ);
                }

                (
                    Command::Case(Arc::clone(branches), Box::from(typed_processes)),
                    Type::Either(span.clone(), branch_types),
                )
            }

            Command::Break => {
                self.cannot_have_obligations(span)?;
                (Command::Break, Type::Continue(span.clone()))
            }

            Command::Continue(process) => {
                let process = self.check_process(process)?;
                (Command::Continue(process), Type::Break(span.clone()))
            }

            Command::Begin { .. } => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    subject.clone(),
                ));
            }

            Command::Loop(label, driver, captures) => {
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

                (
                    Command::Loop(label.clone(), driver.clone(), captures.clone()),
                    driver_type,
                )
            }

            Command::SendType(_, _) => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    subject.clone(),
                ))
            }

            Command::ReceiveType(parameter, process) => {
                self.type_defs.vars.insert(parameter.clone());
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::ReceiveType(parameter.clone(), process),
                    Type::Exists(span.clone(), parameter.clone(), Box::new(then_type)),
                )
            }
        })
    }

    pub fn check_expression(
        &mut self,
        inference_subject: Option<&LocalName>,
        expression: &Expression<()>,
        target_type: &Type,
    ) -> Result<Arc<Expression<Type>>, TypeError> {
        match expression {
            Expression::Global(span, name, ()) => {
                let typ = self.get_global(span, name)?;
                typ.check_assignable(span, target_type, &self.type_defs)?;
                Ok(Arc::new(Expression::Global(
                    span.clone(),
                    name.clone(),
                    typ.clone(),
                )))
            }

            Expression::Variable(span, name, (), usage) => {
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

            Expression::Box(span, captures, expression, ()) => {
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
                        } => {
                            target_inner_type = Type::expand_recursive(&asc, &label, &body)?;
                        }
                        Type::Iterative {
                            span,
                            asc,
                            label,
                            body,
                        } => {
                            target_inner_type = Type::expand_iterative(&span, &asc, &label, &body)?;
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

            Expression::Chan {
                span,
                captures,
                chan_name: channel,
                chan_annotation: annotation,
                process,
                ..
            } => {
                let target_dual = target_type.clone().dual(Span::None);
                let (chan_type, expr_type) = match annotation {
                    Some(annotated_type) => {
                        // Validate channel type annotation before using it
                        self.type_defs.validate_type(annotated_type)?;
                        annotated_type.check_assignable(span, &target_dual, &self.type_defs)?;
                        (annotated_type.clone(), target_type) // or annotated_type.dual() ???
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

            Expression::Primitive(span, value, ()) => {
                let typ = value.get_type();
                typ.check_assignable(span, target_type, &self.type_defs)?;
                Ok(Arc::new(Expression::Primitive(
                    span.clone(),
                    value.clone(),
                    typ,
                )))
            }

            Expression::External(claimed_type, f, ()) => {
                // Validate external claimed type before using it
                self.type_defs.validate_type(claimed_type)?;
                let typ = claimed_type.clone();
                typ.check_assignable(&Span::None, target_type, &self.type_defs)?;
                Ok(Arc::new(Expression::External(
                    claimed_type.clone(),
                    *f,
                    typ,
                )))
            }
        }
    }

    pub fn infer_expression(
        &mut self,
        inference_subject: Option<&LocalName>,
        expression: &Expression<()>,
    ) -> Result<(Arc<Expression<Type>>, Type), TypeError> {
        match expression {
            Expression::Global(span, name, ()) => {
                let typ = self.get_global(span, name)?;
                Ok((
                    Arc::new(Expression::Global(span.clone(), name.clone(), typ.clone())),
                    typ.clone(),
                ))
            }

            Expression::Variable(span, name, (), usage) => {
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

            Expression::Box(span, captures, expression, ()) => {
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

            Expression::Chan {
                span,
                captures,
                chan_name: channel,
                chan_annotation: annotation,
                process,
                ..
            } => {
                let mut context = self.split();
                self.capture(inference_subject, captures, false, &mut context)?;
                let (process, typ) = match annotation {
                    Some(typ) => {
                        // Validate channel type annotation before using it
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

            Expression::Primitive(span, value, ()) => {
                let typ = value.get_type();
                Ok((
                    Arc::new(Expression::Primitive(
                        span.clone(),
                        value.clone(),
                        typ.clone(),
                    )),
                    typ,
                ))
            }

            Expression::External(claimed_type, f, ()) => {
                // Validate external claimed type before using it
                self.type_defs.validate_type(claimed_type)?;
                let typ = claimed_type.clone();
                Ok((
                    Arc::new(Expression::External(claimed_type.clone(), *f, typ.clone())),
                    typ,
                ))
            }
        }
    }
}
