use super::super::language::{GlobalName, LocalName};
use super::super::process::{Captures, Expression};
use super::core::Type;
use super::error::TypeError;
use super::TypeDefs;
use crate::location::Span;
use indexmap::{IndexMap, IndexSet};
use std::sync::{Arc, RwLock};

#[derive(Clone, Debug)]
pub struct CheckedDef {
    pub span: Span,
    pub def: Arc<Expression<Type>>,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub type_defs: TypeDefs,
    pub declarations: Arc<IndexMap<GlobalName, (Span, Type)>>,
    pub unchecked_definitions: Arc<IndexMap<GlobalName, (Span, Arc<Expression<()>>)>>,
    pub checked_definitions: Arc<RwLock<IndexMap<GlobalName, CheckedDef>>>,
    pub current_deps: IndexSet<GlobalName>,
    pub variables: IndexMap<LocalName, Type>,
    pub loop_points: IndexMap<Option<LocalName>, (Type, Arc<IndexMap<LocalName, Type>>)>,
}

impl Context {
    pub fn new(
        type_defs: TypeDefs,
        declarations: IndexMap<GlobalName, (Span, Type)>,
        unchecked_definitions: IndexMap<GlobalName, (Span, Arc<Expression<()>>)>,
    ) -> Self {
        Self {
            type_defs,
            declarations: Arc::new(declarations),
            unchecked_definitions: Arc::new(unchecked_definitions),
            checked_definitions: Arc::new(RwLock::new(IndexMap::new())),
            current_deps: IndexSet::new(),
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
        }
    }

    pub fn check_definition(&mut self, span: &Span, name: &GlobalName) -> Result<Type, TypeError> {
        if let Some(checked) = self.checked_definitions.read().unwrap().get(name) {
            return Ok(checked.typ.clone());
        }

        let Some((span_def, unchecked_def)) = self.unchecked_definitions.get(name).cloned() else {
            return Err(TypeError::GlobalNameNotDefined(span.clone(), name.clone()));
        };

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

        let (checked_def, checked_type) = match self.declarations.get(name).cloned() {
            Some((_, declared_type)) => {
                self.type_defs.validate_type(
                    &declared_type,
                    &IndexSet::new(),
                    &IndexSet::new(),
                    &IndexSet::new(),
                    &IndexSet::new(),
                )?;
                let checked_def = self.check_expression(None, &unchecked_def, &declared_type)?;
                (checked_def, declared_type)
            }
            None => {
                let (expr, typ) = self.infer_expression(None, &unchecked_def)?;
                self.type_defs.validate_type(
                    &typ,
                    &IndexSet::new(),
                    &IndexSet::new(),
                    &IndexSet::new(),
                    &IndexSet::new(),
                )?;
                (expr, typ)
            }
        };

        self.checked_definitions.write().unwrap().insert(
            name.clone(),
            CheckedDef {
                span: span_def,
                def: checked_def,
                typ: checked_type.clone(),
            },
        );

        Ok(checked_type)
    }

    pub fn get_checked_definitions(&self) -> IndexMap<GlobalName, (Span, Arc<Expression<Type>>)> {
        self.checked_definitions
            .read()
            .unwrap()
            .iter()
            .map(|(name, checked)| (name.clone(), (checked.span.clone(), checked.def.clone())))
            .collect()
    }

    pub fn get_declarations(&self) -> IndexMap<GlobalName, (Span, Type)> {
        (*self.declarations).clone()
    }

    pub fn get_type_defs(&self) -> &TypeDefs {
        &self.type_defs
    }

    pub fn split(&self) -> Self {
        Self {
            type_defs: self.type_defs.clone(),
            declarations: self.declarations.clone(),
            unchecked_definitions: self.unchecked_definitions.clone(),
            checked_definitions: self.checked_definitions.clone(),
            current_deps: self.current_deps.clone(),
            variables: IndexMap::new(),
            loop_points: self.loop_points.clone(),
        }
    }

    pub fn get_global(&mut self, span: &Span, name: &GlobalName) -> Result<Type, TypeError> {
        self.check_definition(span, name)
    }

    pub fn get_variable(&mut self, name: &LocalName) -> Option<Type> {
        self.variables.shift_remove(name)
    }

    pub fn get_variable_or_error(
        &mut self,
        span: &Span,
        name: &LocalName,
    ) -> Result<Type, TypeError> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => Err(TypeError::VariableDoesNotExist(span.clone(), name.clone())),
        }
    }

    pub fn put(&mut self, span: &Span, name: LocalName, typ: Type) -> Result<(), TypeError> {
        if let Some(typ) = self.variables.get(&name) {
            if typ.is_linear(&self.type_defs)? {
                return Err(TypeError::ShadowedObligation(span.clone(), name));
            }
        }
        self.variables.insert(name, typ);
        Ok(())
    }

    pub fn invalidate_ascendent(&mut self, label: &Option<LocalName>) {
        for (_, t) in &mut self.variables {
            t.invalidate_ascendent(label);
        }
    }

    pub fn capture(
        &mut self,
        inference_subject: Option<&LocalName>,
        cap: &Captures,
        only_non_linear: bool,
        target: &mut Self,
    ) -> Result<(), TypeError> {
        for (name, span) in &cap.names {
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

    pub fn obligations(&self) -> impl Iterator<Item = &LocalName> {
        self.variables
            .iter()
            .filter(|(_, typ)| typ.is_linear(&self.type_defs).ok().unwrap_or(true))
            .map(|(name, _)| name)
    }

    pub fn cannot_have_obligations(&mut self, span: &Span) -> Result<(), TypeError> {
        if self.obligations().any(|_| true) {
            return Err(TypeError::UnfulfilledObligations(
                span.clone(),
                self.obligations().cloned().collect(),
            ));
        }
        Ok(())
    }
}
