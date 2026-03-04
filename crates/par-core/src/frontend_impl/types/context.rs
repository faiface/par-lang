use crate::frontend_impl::language::{GlobalName, LocalName};
use crate::frontend_impl::process::{Captures, Expression};
use crate::frontend_impl::types::{Type, TypeDefs, TypeError};
use crate::location::Span;
use indexmap::{IndexMap, IndexSet};
use std::sync::{Arc, RwLock};

#[derive(Clone, Debug)]
pub(crate) struct PollPointScope<S> {
    pub(crate) client_type: Type<S>,
    pub(crate) preserved: Arc<IndexMap<LocalName, Type<S>>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PollScope<S> {
    pub(crate) driver: LocalName,
    pub(crate) pool_type: Type<S>,
    pub(crate) points: IndexMap<LocalName, PollPointScope<S>>,
    pub(crate) current_point: LocalName,
    pub(crate) token_span: Span,
}

#[derive(Clone, Debug)]
pub(crate) struct Context<S> {
    pub(crate) type_defs: TypeDefs<S>,
    declarations: Arc<IndexMap<GlobalName<S>, (Span, Type<S>)>>,
    unchecked_definitions: Arc<IndexMap<GlobalName<S>, (Span, Arc<Expression<(), S>>)>>,
    checked_definitions: Arc<RwLock<IndexMap<GlobalName<S>, CheckedDef<S>>>>,
    current_deps: IndexSet<GlobalName<S>>,
    pub(crate) variables: IndexMap<LocalName, Type<S>>,
    pub(crate) loop_points:
        IndexMap<Option<LocalName>, (Type<S>, Arc<IndexMap<LocalName, Type<S>>>)>,
    pub(crate) poll: Option<PollScope<S>>,
    pub(crate) poll_stash: Vec<Option<PollScope<S>>>,
    pub(crate) blocks: IndexMap<usize, Vec<IndexMap<LocalName, Type<S>>>>,
}

#[derive(Clone, Debug)]
struct CheckedDef<S> {
    span: Span,
    def: Arc<Expression<Type<S>, S>>,
    typ: Type<S>,
}

impl<S: Clone + Eq + std::hash::Hash> Context<S> {
    pub(crate) fn new(
        type_defs: TypeDefs<S>,
        declarations: IndexMap<GlobalName<S>, (Span, Type<S>)>,
        unchecked_definitions: IndexMap<GlobalName<S>, (Span, Arc<Expression<(), S>>)>,
    ) -> Self {
        Self {
            type_defs,
            declarations: Arc::new(declarations),
            unchecked_definitions: Arc::new(unchecked_definitions),
            checked_definitions: Arc::new(RwLock::new(IndexMap::new())),
            current_deps: IndexSet::new(),
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
            poll: None,
            poll_stash: Vec::new(),
            blocks: IndexMap::new(),
        }
    }

    pub(crate) fn check_definition(
        &mut self,
        span: &Span,
        name: &GlobalName<S>,
    ) -> Result<Type<S>, TypeError<S>> {
        if let Some(checked) = self.checked_definitions.read().unwrap().get(name) {
            return Ok(checked.typ.clone());
        }

        let Some((name_def, def)) = self.unchecked_definitions.get_key_value(name) else {
            return Err(TypeError::GlobalNameNotDefined(span.clone(), name.clone()));
        };
        let name_def = name_def.clone();
        let (span_def, unchecked_def) = def.clone();

        if !self.current_deps.insert(name.clone()) {
            return Err(TypeError::DependencyCycle(
                span.clone(),
                self.current_deps
                    .iter()
                    .cloned()
                    .skip_while(|dep| dep != name)
                    .collect(),
            ));
        }

        let original_variables = self.variables.drain(..).collect();
        let original_loop_points = self.loop_points.drain(..).collect();
        let original_poll = self.poll.take();
        let original_poll_stash = std::mem::take(&mut self.poll_stash);
        let original_blocks = self.blocks.drain(..).collect();

        let (checked_def, checked_type) = match self.declarations.get(name).cloned() {
            Some((_, declared_type)) => {
                self.type_defs.validate_type(&declared_type)?;
                let checked_def = self.check_expression(None, &unchecked_def, &declared_type)?;
                (checked_def, declared_type)
            }
            None => {
                let (expr, mut typ) = self.infer_expression(None, &unchecked_def)?;
                self.type_defs.validate_type(&typ)?;
                typ.remove_asc(&self.type_defs)?;
                (expr, typ)
            }
        };

        self.variables = original_variables;
        self.loop_points = original_loop_points;
        self.poll = original_poll;
        self.poll_stash = original_poll_stash;
        self.blocks = original_blocks;

        self.checked_definitions.write().unwrap().insert(
            name_def,
            CheckedDef {
                span: span_def,
                def: checked_def,
                typ: checked_type.clone(),
            },
        );

        Ok(checked_type)
    }

    pub(crate) fn get_checked_definitions(
        &self,
    ) -> IndexMap<GlobalName<S>, (Span, Arc<Expression<Type<S>, S>>, Type<S>)> {
        self.checked_definitions
            .read()
            .unwrap()
            .iter()
            .map(|(name, checked)| {
                (
                    name.clone(),
                    (
                        checked.span.clone(),
                        checked.def.clone(),
                        checked.typ.clone(),
                    ),
                )
            })
            .collect()
    }

    pub(crate) fn get_declarations(&self) -> IndexMap<GlobalName<S>, (Span, Type<S>)> {
        (*self.declarations).clone()
    }

    pub(crate) fn get_type_defs(&self) -> &TypeDefs<S> {
        &self.type_defs
    }

    pub(crate) fn split(&self) -> Self {
        Self {
            type_defs: self.type_defs.clone(),
            declarations: self.declarations.clone(),
            unchecked_definitions: self.unchecked_definitions.clone(),
            checked_definitions: self.checked_definitions.clone(),
            current_deps: self.current_deps.clone(),
            variables: IndexMap::new(),
            loop_points: self.loop_points.clone(),
            poll: self.poll.clone(),
            poll_stash: self.poll_stash.clone(),
            blocks: self.blocks.clone(),
        }
    }

    pub(crate) fn get_global(
        &mut self,
        span: &Span,
        name: &GlobalName<S>,
    ) -> Result<Type<S>, TypeError<S>> {
        self.check_definition(span, name)
    }

    pub(crate) fn get_variable(&mut self, name: &LocalName) -> Option<Type<S>> {
        self.variables.shift_remove(name)
    }

    pub(crate) fn get_variable_or_error(
        &mut self,
        span: &Span,
        name: &LocalName,
    ) -> Result<Type<S>, TypeError<S>> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => Err(TypeError::VariableDoesNotExist(span.clone(), name.clone())),
        }
    }

    pub(crate) fn put(
        &mut self,
        span: &Span,
        name: LocalName,
        typ: Type<S>,
    ) -> Result<(), TypeError<S>> {
        if let Some(typ) = self.variables.get(&name) {
            if typ.is_linear(&self.type_defs)? {
                return Err(TypeError::ShadowedObligation(span.clone(), name));
            }
        }
        self.variables.insert(name, typ);
        Ok(())
    }

    pub(crate) fn capture(
        &mut self,
        inference_subject: Option<&LocalName>,
        cap: &Captures,
        only_non_linear: bool,
        target: &mut Self,
    ) -> Result<(), TypeError<S>> {
        for (name, (span, _usage)) in &cap.names {
            if Some(name) == inference_subject {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    name.clone(),
                ));
            }
            let typ = match self.get_variable(name) {
                Some(typ) => typ,
                None => continue,
            };
            if !typ.is_linear(&self.type_defs)? {
                self.put(span, name.clone(), typ.clone())?;
            } else if only_non_linear {
                return Err(TypeError::CannotUseLinearVariableInBox(
                    span.clone(),
                    name.clone(),
                ));
            }
            target.put(span, name.clone(), typ)?;
        }
        Ok(())
    }

    pub(crate) fn obligations(&self) -> impl Iterator<Item = &LocalName> {
        self.variables
            .iter()
            .filter(|(_, typ)| typ.is_linear(&self.type_defs).ok().unwrap_or(true))
            .map(|(name, _)| name)
    }

    pub(crate) fn cannot_have_obligations(&mut self, span: &Span) -> Result<(), TypeError<S>> {
        if let Some(poll) = &self.poll {
            if self.variables.contains_key(&poll.driver) {
                return Err(TypeError::PollBranchMustSubmit(span.clone()));
            }
        }
        if self.obligations().any(|_| true) {
            return Err(TypeError::UnfulfilledObligations(
                span.clone(),
                self.obligations().cloned().collect(),
            ));
        }
        Ok(())
    }
}
