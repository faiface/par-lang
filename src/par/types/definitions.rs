use crate::location::Span;
use crate::par::language::{GlobalName, LocalName};
use crate::par::types::{Type, TypeError};
use indexmap::{IndexMap, IndexSet};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct TypeDefs {
    pub globals: Arc<IndexMap<GlobalName, (Span, Vec<LocalName>, Type)>>,
    pub vars: IndexSet<LocalName>,
}

impl Default for TypeDefs {
    fn default() -> Self {
        Self {
            globals: Default::default(),
            vars: Default::default(),
        }
    }
}

impl TypeDefs {
    pub fn new_with_validation<'a>(
        globals: impl Iterator<Item = (&'a Span, &'a GlobalName, &'a Vec<LocalName>, &'a Type)>,
    ) -> Result<Self, TypeError> {
        let mut globals_map = IndexMap::new();
        for (span, name, params, typ) in globals {
            if let Some((span1, _, _)) =
                globals_map.insert(name.clone(), (span.clone(), params.clone(), typ.clone()))
            {
                return Err(TypeError::TypeNameAlreadyDefined(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let type_defs = Self {
            globals: Arc::new(globals_map),
            vars: IndexSet::new(),
        };

        let mut deps_map: IndexMap<GlobalName, Vec<GlobalName>> = Default::default();
        for (name, (_, _, typ)) in type_defs.globals.iter() {
            deps_map.insert(name.clone(), typ.get_dependencies());
        }

        for (name, _) in type_defs.globals.iter() {
            type_defs.validate_acyclic(name, &Default::default(), &deps_map)?
        }

        for (_, (_, params, typ)) in type_defs.globals.iter() {
            let mut type_defs = type_defs.clone();
            for param in params {
                type_defs.vars.insert(param.clone());
            }
            type_defs.validate_type(
                typ,
                &IndexSet::new(),
                &IndexSet::new(),
                &IndexSet::new(),
                &IndexSet::new(),
            )?;
        }

        Ok(type_defs)
    }

    pub fn get(&self, span: &Span, name: &GlobalName, args: &[Type]) -> Result<Type, TypeError> {
        match self.globals.get(name) {
            Some((_, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        span.clone(),
                        name.clone(),
                        params.len(),
                        args.len(),
                    ));
                }
                Ok(typ.clone().substitute(params.iter().zip(args).collect())?)
            }
            None => Err(TypeError::TypeNameNotDefined(span.clone(), name.clone())),
        }
    }

    pub fn get_dual(
        &self,
        span: &Span,
        name: &GlobalName,
        args: &[Type],
    ) -> Result<Type, TypeError> {
        match self.globals.get(name) {
            Some((_, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        span.clone(),
                        name.clone(),
                        params.len(),
                        args.len(),
                    ));
                }
                Ok(typ
                    .clone()
                    .dual(Span::None)
                    .substitute(params.iter().zip(args).collect())?)
            }
            None => Err(TypeError::TypeNameNotDefined(span.clone(), name.clone())),
        }
    }

    pub fn validate_acyclic(
        &self,
        name: &GlobalName,
        deps_stack: &IndexSet<GlobalName>,
        deps_map: &IndexMap<GlobalName, Vec<GlobalName>>,
    ) -> Result<(), TypeError> {
        let mut deps_stack = deps_stack.clone();
        if !deps_stack.insert(name.clone()) {
            return Err(TypeError::DependencyCycle(
                self.globals[name].0.clone(),
                deps_stack
                    .clone()
                    .into_iter()
                    .skip_while(|dep| dep != name)
                    .collect(),
            ));
        }
        if let Some(deps) = deps_map.get(name) {
            for dep in deps {
                self.validate_acyclic(dep, &deps_stack, deps_map)?;
            }
        }
        Ok(())
    }

    pub fn validate_type(
        &self,
        typ: &Type,
        self_pos: &IndexSet<Option<LocalName>>,
        self_neg: &IndexSet<Option<LocalName>>,
        unguarded_self_rec: &IndexSet<Option<LocalName>>,
        unguarded_self_iter: &IndexSet<Option<LocalName>>,
    ) -> Result<(), TypeError> {
        Ok(match typ {
            Type::Primitive(_, _) | Type::DualPrimitive(_, _) => (),

            Type::Var(span, name) | Type::DualVar(span, name) => {
                if self.vars.contains(name) {
                    ()
                } else {
                    return Err(TypeError::TypeVariableNotDefined(
                        span.clone(),
                        name.clone(),
                    ));
                }
            }
            Type::Name(span, name, args) => {
                for arg in args {
                    self.validate_type(
                        arg,
                        self_pos,
                        self_neg,
                        unguarded_self_rec,
                        unguarded_self_iter,
                    )?;
                }
                let t = self.get(span, name, args)?;
                self.validate_type(
                    &t,
                    self_pos,
                    self_neg,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
            }
            Type::DualName(span, name, args) => {
                for arg in args {
                    self.validate_type(
                        arg,
                        self_neg,
                        self_pos,
                        unguarded_self_rec,
                        unguarded_self_iter,
                    )?;
                }
                let t = self.get(span, name, args)?;
                self.validate_type(
                    &t,
                    self_neg,
                    self_pos,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
            }

            Type::Box(_, body) => self.validate_type(
                body,
                self_pos,
                self_neg,
                unguarded_self_rec,
                unguarded_self_iter,
            )?,
            Type::DualBox(_, body) => self.validate_type(
                body,
                self_neg,
                self_pos,
                unguarded_self_rec,
                unguarded_self_iter,
            )?,

            Type::Pair(_, t, u) => {
                self.validate_type(
                    t,
                    self_pos,
                    self_neg,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
                self.validate_type(
                    u,
                    self_pos,
                    self_neg,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
            }
            Type::Function(_, t, u) => {
                self.validate_type(
                    t,
                    self_neg,
                    self_pos,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
                self.validate_type(
                    u,
                    self_pos,
                    self_neg,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
            }
            Type::Either(_, branches) => {
                let unguarded_self_rec = IndexSet::new();
                for (_, t) in branches {
                    self.validate_type(
                        t,
                        self_pos,
                        self_neg,
                        &unguarded_self_rec,
                        unguarded_self_iter,
                    )?;
                }
            }
            Type::Choice(_, branches) => {
                let unguarded_self_iter = IndexSet::new();
                for (_, t) in branches {
                    self.validate_type(
                        t,
                        self_pos,
                        self_neg,
                        unguarded_self_rec,
                        &unguarded_self_iter,
                    )?;
                }
            }
            Type::Break(_) | Type::Continue(_) => (),

            Type::Recursive { label, body, .. } | Type::Iterative { label, body, .. } => {
                let (mut self_pos, mut self_neg) = (self_pos.clone(), self_neg.clone());
                self_pos.insert(label.clone());
                self_neg.shift_remove(label);
                let (mut unguarded_self_rec, mut unguarded_self_iter) =
                    (unguarded_self_rec.clone(), unguarded_self_iter.clone());
                match typ {
                    Type::Recursive { .. } => {
                        unguarded_self_rec.insert(label.clone());
                        unguarded_self_iter.shift_remove(label);
                    }
                    Type::Iterative { .. } => {
                        unguarded_self_iter.insert(label.clone());
                        unguarded_self_rec.shift_remove(label);
                    }
                    _ => unreachable!(),
                }
                self.validate_type(
                    body,
                    &self_pos,
                    &self_neg,
                    &unguarded_self_rec,
                    &unguarded_self_iter,
                )?;
            }
            Type::Self_(span, label) => {
                if self_neg.contains(label) {
                    return Err(TypeError::SelfUsedInNegativePosition(span.clone()));
                }
                if !self_pos.contains(label) {
                    return Err(TypeError::NoMatchingRecursiveOrIterative(span.clone()));
                }
                if unguarded_self_rec.contains(label) {
                    return Err(TypeError::UnguardedRecursiveSelf(span.clone()));
                }
                if unguarded_self_iter.contains(label) {
                    return Err(TypeError::UnguardedIterativeSelf(span.clone()));
                }
            }
            Type::DualSelf(span, label) => {
                if self_pos.contains(label) {
                    return Err(TypeError::SelfUsedInNegativePosition(span.clone()));
                }
                if !self_neg.contains(label) {
                    return Err(TypeError::NoMatchingRecursiveOrIterative(span.clone()));
                }
            }

            Type::Exists(_, name, body) | Type::Forall(_, name, body) => {
                let mut with_var = self.clone();
                with_var.vars.insert(name.clone());
                with_var.validate_type(
                    body,
                    self_pos,
                    self_neg,
                    unguarded_self_rec,
                    unguarded_self_iter,
                )?;
            }
        })
    }
}
