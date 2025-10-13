use crate::location::Span;
use crate::par::language::{GlobalName, LocalName};
use crate::par::types::{visit, Type, TypeError};
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
                    span1.clone(),
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
            type_defs.validate_type(typ)?;
        }

        Ok(type_defs)
    }

    pub fn get(&self, span: &Span, name: &GlobalName, args: &[Type]) -> Result<Type, TypeError> {
        self.get_with_span(span, name, args).map(|(_, typ)| typ)
    }

    pub fn get_with_span(
        &self,
        span: &Span,
        name: &GlobalName,
        args: &[Type],
    ) -> Result<(&Span, Type), TypeError> {
        match self.globals.get(name) {
            Some((span, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        span.clone(),
                        name.clone(),
                        params.len(),
                        args.len(),
                    ));
                }
                let typ = typ.clone().substitute(params.iter().zip(args).collect())?;
                Ok((span, typ))
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
        self.get_dual_with_span(span, name, args)
            .map(|(_, typ)| typ)
    }

    pub fn get_dual_with_span(
        &self,
        span: &Span,
        name: &GlobalName,
        args: &[Type],
    ) -> Result<(&Span, Type), TypeError> {
        match self.globals.get(name) {
            Some((span, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        span.clone(),
                        name.clone(),
                        params.len(),
                        args.len(),
                    ));
                }
                let typ = typ
                    .clone()
                    .dual(Span::None)
                    .substitute(params.iter().zip(args).collect())?;
                Ok((span, typ))
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

    pub fn validate_type(&self, typ: &Type) -> Result<(), TypeError> {
        #[derive(Clone)]
        struct Ctx {
            defs: TypeDefs,
            check_self: bool,
            self_polarity: IndexMap<Option<LocalName>, bool>,
            unguarded_self_rec: IndexSet<Option<LocalName>>,
            unguarded_self_iter: IndexSet<Option<LocalName>>,
        }
        fn inner(typ: &Type, positive: bool, mut ctx: Ctx) -> Result<(), TypeError> {
            match typ {
                Type::Name(_span, _name, args) | Type::DualName(_span, _name, args) => {
                    for arg in args {
                        inner(
                            arg,
                            positive,
                            Ctx {
                                defs: ctx.defs.clone(),
                                check_self: false,
                                self_polarity: IndexMap::new(),
                                unguarded_self_rec: IndexSet::new(),
                                unguarded_self_iter: IndexSet::new(),
                            },
                        )?;
                    }
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Exists(_span, name, _body) | Type::Forall(_span, name, _body) => {
                    ctx.defs.vars.insert(name.clone());
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Var(span, name) | Type::DualVar(span, name) => {
                    if ctx.defs.vars.contains(name) {
                        ()
                    } else {
                        return Err(TypeError::TypeVariableNotDefined(
                            span.clone(),
                            name.clone(),
                        ));
                    }
                }
                Type::Recursive { label, .. } if ctx.check_self => {
                    ctx.unguarded_self_rec.insert(label.clone());
                    ctx.unguarded_self_iter.shift_remove(label);
                    ctx.self_polarity.insert(label.clone(), positive);
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Iterative { label, .. } if ctx.check_self => {
                    ctx.unguarded_self_iter.insert(label.clone());
                    ctx.unguarded_self_rec.shift_remove(label);
                    ctx.self_polarity.insert(label.clone(), positive);
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Either(..) if ctx.check_self => {
                    ctx.unguarded_self_rec = IndexSet::new();
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Choice(..) if ctx.check_self => {
                    ctx.unguarded_self_iter = IndexSet::new();
                    visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                        inner(typ, positive, ctx.clone())
                    })?;
                }
                Type::Self_(span, label) if ctx.check_self => {
                    if let Some(is_positive) = ctx.self_polarity.get(label) {
                        if *is_positive != positive {
                            return Err(TypeError::SelfUsedInNegativePosition(span.clone()));
                        }
                    } else {
                        return Err(TypeError::NoMatchingRecursiveOrIterative(span.clone()));
                    }
                    if ctx.unguarded_self_rec.contains(label) {
                        return Err(TypeError::UnguardedRecursiveSelf(span.clone()));
                    }
                    if ctx.unguarded_self_iter.contains(label) {
                        return Err(TypeError::UnguardedIterativeSelf(span.clone()));
                    }
                }
                Type::DualSelf(span, label) if ctx.check_self => {
                    if let Some(is_positive) = ctx.self_polarity.get(label) {
                        if *is_positive == positive {
                            return Err(TypeError::SelfUsedInNegativePosition(span.clone()));
                        }
                    } else {
                        return Err(TypeError::NoMatchingRecursiveOrIterative(span.clone()));
                    }
                    if ctx.unguarded_self_rec.contains(label) {
                        return Err(TypeError::UnguardedRecursiveSelf(span.clone()));
                    }
                    if ctx.unguarded_self_iter.contains(label) {
                        return Err(TypeError::UnguardedIterativeSelf(span.clone()));
                    }
                }
                _ => visit::continue_deref_polarized(typ, positive, &ctx.defs, |typ, positive| {
                    inner(typ, positive, ctx.clone())
                })?,
            }
            Ok(())
        }
        inner(
            typ,
            true,
            Ctx {
                defs: self.clone(),
                check_self: true,
                self_polarity: IndexMap::new(),
                unguarded_self_rec: IndexSet::new(),
                unguarded_self_iter: IndexSet::new(),
            },
        )
    }
}
