use indexmap::{IndexMap, IndexSet};
use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
    hash::Hash,
    sync::{Arc, RwLock},
};

use super::{
    language::Program,
    process
    //process::{process::Captures, process::Command, process::Expression, process::Process},
};
use miette::LabeledSpan;
use crate::location::{Span, Spanning};
use crate::par::language::{Declaration, Definition, TypeDef};

#[derive(Clone, Debug)]
pub enum TypeError<Name> {
    // todo: records
    TypeNameAlreadyDefined(Span, Span, Name),
    NameAlreadyDeclared(Span, Span, Name),
    NameAlreadyDefined(Span, Span, Name),
    DeclaredButNotDefined(Span, Name),
    NoMatchingRecursiveOrIterative(Span, Option<Name>),
    SelfUsedInNegativePosition(Span, Option<Name>),
    TypeNameNotDefined(Span, Name),
    DependencyCycle(Span, Vec<Name>),
    WrongNumberOfTypeArgs(Span, Name, usize, usize),
    NameNotDefined(Span, Name),
    ShadowedObligation(Span, Name),
    TypeMustBeKnownAtThisPoint(Span, Name),
    ParameterTypeMustBeKnown(Span, Name, Name),
    CannotAssignFromTo(Span, Type<Name>, Type<Name>),
    UnfulfilledObligations(Span, Vec<Name>),
    InvalidOperation(Span, Operation<Name>, Type<Name>),
    InvalidBranch(Span, Name, Type<Name>),
    MissingBranch(Span, Name, Type<Name>),
    RedundantBranch(Span, Name, Type<Name>),
    TypesCannotBeUnified(Type<Name>, Type<Name>),
    NoSuchLoopPoint(Span, Option<Name>),
    DoesNotDescendSubjectOfBegin(Span, Option<Name>),
    LoopVariableNotPreserved(Span, Name),
    LoopVariableChangedType(Span, Name, Type<Name>, Type<Name>),
    Telltypes(Span, IndexMap<Name, Type<Name>>),
}

#[derive(Clone, Debug)]
pub enum Operation<Name> {
    Send(Span),
    Receive(Span),
    Choose(Span, Name),
    Match(Span, #[allow(unused)] Arc<[Name]>),
    Break(Span),
    Continue(Span),
    Begin(Span, Option<Name>),
    Loop(Span, Option<Name>),
    SendType(Span),
    ReceiveType(Span),
}

#[derive(Clone, Debug)]
pub enum Type<Name> {
    Chan(Span, Box<Self>),
    // todo: difference?
    Var(Span, Name),
    Name(Span, Name, Vec<Type<Name>>),
    Send(Span, Vec<Self>, Box<Self>),
    Receive(Span, Vec<Self>, Box<Self>),
    Either(Span, IndexMap<Name, Self>),
    Choice(Span, IndexMap<Name, Self>),
    /// ! (unit)
    Break(Span),
    /// ? (bottom)
    Continue(Span),
    Recursive {
        span: Span,
        // todo: what is this?
        asc: IndexSet<Option<Name>>,
        label: Option<Name>,
        body: Box<Self>,
    },
    Iterative { span: Span, asc: IndexSet<Option<Name>>, label: Option<Name>, body: Box<Self> },
    Self_(Span, Option<Name>),
    SendTypes(Span, Vec<Name>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct TypeDefs<Name> {
    globals: Arc<IndexMap<Name, (Span, Vec<Name>, Type<Name>)>>,
    vars: IndexSet<Name>,
}

impl<Name: Clone + Eq + Hash> TypeDefs<Name> {
    pub fn new_with_validation(
        globals: &[TypeDef<Name>],
    ) -> Result<Self, TypeError<Name>> {
        let mut globals_map = IndexMap::new();
        for TypeDef { span, name, params, typ } in globals {
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

        for (name, (_, params, typ)) in type_defs.globals.iter() {
            let mut type_defs = type_defs.clone();
            for param in params {
                type_defs.vars.insert(param.clone());
            }
            type_defs.validate_type(
                typ,
                &IndexSet::from([name.clone()]),
                &IndexSet::new(),
                &IndexSet::new(),
            )?;
        }

        Ok(type_defs)
    }

    pub fn get(
        &self,
        span: &Span,
        name: &Name,
        args: &[Type<Name>],
    ) -> Result<Type<Name>, TypeError<Name>> {
        if self.vars.contains(name) {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfTypeArgs(
                    span.clone(),
                    name.clone(),
                    0,
                    args.len(),
                ));
            }
            return Ok(Type::Var(span.clone(), name.clone()));
        }
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
                let mut typ = typ.clone();
                for i in 0..params.len() {
                    typ = typ.substitute(&params[i], &args[i])?;
                }
                Ok(typ)
            }
            None => Err(TypeError::TypeNameNotDefined(span.clone(), name.clone())),
        }
    }

    pub fn get_dual(
        &self,
        span: &Span,
        name: &Name,
        args: &[Type<Name>],
    ) -> Result<Type<Name>, TypeError<Name>> {
        if self.vars.contains(name) {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfTypeArgs(
                    span.clone(),
                    name.clone(),
                    0,
                    args.len(),
                ));
            }
            return Ok(Type::Chan(
                span.clone(),
                Box::new(Type::Var(span.clone(), name.clone())),
            ));
        }
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
                let mut typ = typ.dual(self)?;
                for i in 0..params.len() {
                    typ = typ.substitute(&params[i], &args[i])?;
                }
                Ok(typ)
            }
            None => Err(TypeError::TypeNameNotDefined(span.clone(), name.clone())),
        }
    }

    fn validate_type(
        &self,
        typ: &Type<Name>,
        deps: &IndexSet<Name>,
        self_pos: &IndexSet<Option<Name>>,
        self_neg: &IndexSet<Option<Name>>,
    ) -> Result<(), TypeError<Name>> {
        Ok(match typ {
            Type::Chan(_, t) => self.validate_type(t, deps, self_neg, self_pos)?,
            Type::Var(span, name) => {
                self.get(span, name, &[])?;
            }
            Type::Name(span, name, args) => {
                let mut deps = deps.clone();
                if !self.vars.contains(name) {
                    if !deps.insert(name.clone()) {
                        return Err(TypeError::DependencyCycle(
                            span.clone(),
                            deps.into_iter().skip_while(|dep| dep != name).collect(),
                        ));
                    }
                }
                let t = self.get(span, name, args)?;
                self.validate_type(&t, &deps, self_pos, self_neg)?;
            }
            Type::Send(_, types, u) => {
                for t in types {
                    self.validate_type(t, deps, self_pos, self_neg)?;
                }
                self.validate_type(u, deps, self_pos, self_neg)?;
            }
            Type::Receive(_, types, u) => {
                for t in types {
                    self.validate_type(t, deps, self_neg, self_pos)?;
                }
                self.validate_type(u, deps, self_pos, self_neg)?;
            }
            Type::Either(_, branches) | Type::Choice(_, branches) => {
                for (_, t) in branches {
                    self.validate_type(t, deps, self_pos, self_neg)?;
                }
            }
            Type::Break(_) | Type::Continue(_) => (),
            Type::Recursive { label, body, ..} | Type::Iterative { label, body, .. } => {
                let (mut self_pos, mut self_neg) = (self_pos.clone(), self_neg.clone());
                self_pos.insert(label.clone());
                self_neg.shift_remove(label);
                self.validate_type(body, deps, &self_pos, &self_neg)?;
            }
            Type::Self_(span, label) => {
                if self_neg.contains(label) {
                    return Err(TypeError::SelfUsedInNegativePosition(
                        span.clone(),
                        label.clone(),
                    ));
                }
                if !self_pos.contains(label) {
                    return Err(TypeError::NoMatchingRecursiveOrIterative(
                        span.clone(),
                        label.clone(),
                    ));
                }
            }
            Type::SendTypes(_, names, body) | Type::ReceiveTypes(_, names, body) => {
                let mut with_var = self.clone();
                with_var.vars.extend(names.clone());
                with_var.validate_type(body, deps, self_pos, self_neg)?;
            }
        })
    }
}

impl<Name> Spanning for Type<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Chan(span, _)
            | Self::Var(span, _)
            | Self::Name(span, _, _)
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Either(span, _)
            | Self::Choice(span, _)
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Recursive { span, .. }
            | Self::Iterative { span, .. }
            | Self::Self_(span, _)
            | Self::SendTypes(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
}

impl<Name: Eq + Hash> Type<Name> {
    pub fn map_names<N: Eq + Hash>(self, f: &mut impl FnMut(Name) -> N) -> Type<N> {
        match self {
            Self::Chan(span, t) => Type::Chan(span, Box::new(t.map_names(f))),
            Self::Var(span, name) => Type::Var(span, f(name)),
            Self::Name(span, name, args) => Type::Name(
                span,
                f(name),
                args.into_iter().map(|arg| arg.map_names(f)).collect(),
            ),
            Self::Send(span, types, u) => {
                let mapped = types.into_iter().map(|t| t.map_names(f)).collect();
                Type::Send(span, mapped, Box::new(u.map_names(f)))
            }
            Self::Receive(span, types, u) => {
                let mapped = types.into_iter().map(|t| t.map_names(f)).collect();
                Type::Receive(span, mapped, Box::new(u.map_names(f)))
            }
            Self::Either(span, branches) => Type::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Type::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Break(span) => Type::Break(span),
            Self::Continue(span) => Type::Continue(span),
            Self::Recursive { span, asc, label, body } => Type::Recursive {
                span,
                asc: asc.into_iter().map(|label| map_label(label, f)).collect(),
                label: map_label(label, f),
                body: Box::new(body.map_names(f)),
            },
            Self::Iterative { span, asc, label, body } => Type::Iterative {
                span,
                asc: asc.into_iter().map(|label| map_label(label, f)).collect(),
                label: map_label(label, f),
                body: Box::new(body.map_names(f)),
            },
            Self::Self_(span, label) => Type::Self_(span, map_label(label, f)),
            Self::SendTypes(span, names, body) => {
                let mapped = names.into_iter().map(&mut *f).collect();
                Type::SendTypes(span, mapped, Box::new(body.map_names(f)))
            }
            Self::ReceiveTypes(span, names, body) => {
                let mapped = names.into_iter().map(&mut *f).collect();
                Type::ReceiveTypes(span, mapped, Box::new(body.map_names(f)))
            }
        }
    }
}

fn map_label<Name, N>(label: Option<Name>, f: &mut impl FnMut(Name) -> N) -> Option<N> {
    label.map(f)
}

impl<Name: Clone + Eq + Hash> Type<Name> {
    pub fn substitute(self, var: &Name, typ: &Self) -> Result<Self, TypeError<Name>> {
        Ok(match self {
            Self::Chan(span, t) => Self::Chan(span, Box::new(t.substitute(var, typ)?)),
            Self::Var(span, name) => {
                if &name == var {
                    typ.clone()
                } else {
                    Self::Var(span, name)
                }
            }
            Self::Name(span, name, args) if &name == var => {
                if !args.is_empty() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        span,
                        var.clone(),
                        0,
                        args.len(),
                    ));
                }
                typ.clone()
            }
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(var, typ))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Send(span, types, u) => Self::Send(
                span,
                types.into_iter().map(|t| t.substitute(var, typ)).collect::<Result<_, _>>()?,
                Box::new(u.substitute(var, typ)?),
            ),
            Self::Receive(span, types, u) => Self::Receive(
                span,
                types.into_iter().map(|t| t.substitute(var, typ)).collect::<Result<_, _>>()?,
                Box::new(u.substitute(var, typ)?),
            ),
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(var, typ)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(var, typ)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive { span, asc, label, body } => {
                Self::Recursive { span, asc, label, body: Box::new(body.substitute(var, typ)?) }
            }
            Self::Iterative { span, asc, label, body } => {
                Self::Iterative { span, asc, label, body: Box::new(body.substitute(var, typ)?) }
            }
            Self::Self_(span, label) => Self::Self_(span, label),

            Self::SendTypes(span, names, body) => {
                if names.contains(var) {
                    Self::SendTypes(span, names, body)
                } else {
                    Self::SendTypes(span, names, Box::new(body.substitute(var, typ)?))
                }
            }
            Self::ReceiveTypes(span, names, body) => {
                if names.contains(var) {
                    Self::ReceiveTypes(span, names, body)
                } else {
                    Self::ReceiveTypes(span, names, Box::new(body.substitute(var, typ)?))
                }
            }
        })
    }

    pub fn check_assignable(
        &self,
        span: &Span,
        u: &Type<Name>,
        type_defs: &TypeDefs<Name>,
    ) -> Result<(), TypeError<Name>> {
        if !self.is_assignable_to(u, type_defs, &HashSet::new())? {
            return Err(TypeError::CannotAssignFromTo(
                span.clone(),
                self.clone(),
                u.clone(),
            ));
        }
        Ok(())
    }

    fn is_assignable_to(
        &self,
        other: &Self,
        type_defs: &TypeDefs<Name>,
        ind: &HashSet<(Option<Name>, Option<Name>)>,
    ) -> Result<bool, TypeError<Name>> {
        Ok(match (self, other) {
            (Self::Chan(_, dual_t1), Self::Chan(_, dual_t2)) => {
                dual_t2.is_assignable_to(dual_t1, type_defs, ind)?
            }
            (Self::Chan(_, dual_t1), t2) => match t2.dual(type_defs)? {
                Self::Chan(_, _) => false,
                dual_t2 => dual_t2.is_assignable_to(dual_t1, type_defs, ind)?,
            },
            (t1, Self::Chan(_, dual_t2)) => match t1.dual(type_defs)? {
                Self::Chan(_, _) => false,
                dual_t1 => dual_t2.is_assignable_to(&dual_t1, type_defs, ind)?,
            },

            (Self::Var(_, name1), Self::Var(_, name2)) => name1 == name2,
            (Self::Name(span, name, args), t2) => type_defs
                .get(span, name, args)?
                .is_assignable_to(t2, type_defs, ind)?,
            (t1, Self::Name(span, name, args)) => {
                t1.is_assignable_to(&type_defs.get(span, name, args)?, type_defs, ind)?
            }

            (Self::Send(_, ts1, u1), Self::Send(_, ts2, u2)) => {
                if ts1.len() != ts2.len() {
                    return Ok(false);
                }
                for (t1, t2) in ts1.iter().zip(ts2) {
                    if !t1.is_assignable_to(t2, type_defs, ind)? {
                        return Ok(false);
                    }
                }
                u1.is_assignable_to(u2, type_defs, ind)?
            }
            (Self::Receive(_, ts1, u1), Self::Receive(_, ts2, u2)) => {
                if ts1.len() != ts2.len() {
                    return Ok(false);
                }
                for (t1, t2) in ts1.iter().zip(ts2) {
                    if !t2.is_assignable_to(t1, type_defs, ind)? {
                        return Ok(false);
                    }
                }
                u1.is_assignable_to(u2, type_defs, ind)?
            }
            (Self::Either(_, branches1), Self::Either(_, branches2)) => {
                for (branch, t1) in branches1 {
                    let Some(t2) = branches2.get(branch) else {
                        return Ok(false);
                    };
                    if !t1.is_assignable_to(t2, type_defs, ind)? {
                        return Ok(false);
                    }
                }
                for (branch, _) in branches2 {
                    if branches1.get(branch).is_none() {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Choice(_, branches1), Self::Choice(_, branches2)) => {
                for (branch, _) in branches1 {
                    if branches2.get(branch).is_none() {
                        return Ok(false);
                    }
                }
                for (branch, t2) in branches2 {
                    let Some(t1) = branches1.get(branch) else {
                        return Ok(false);
                    };
                    if !t1.is_assignable_to(t2, type_defs, ind)? {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Break(_), Self::Break(_)) => true,
            (Self::Continue(_), Self::Continue(_)) => true,

            (Self::Recursive { asc: asc1, label: label1, body: body1, .. }, Self::Recursive { asc: asc2, label: label2, body: body2, .. }) => {
                if !asc2.iter().all(|label| asc1.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (typ, Self::Recursive { asc, label, body, .. }) => typ.is_assignable_to(
                &Self::expand_recursive(asc, label, body, type_defs)?,
                type_defs,
                ind,
            )?,
            (Self::Iterative { asc: asc1, label: label1, body: body1, .. }, Self::Iterative { asc: asc2, label: label2, body: body2, .. }) => {
                if !asc2.iter().all(|label| asc1.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (Self::Iterative { asc, label, body, .. }, typ) => {
                Self::expand_iterative(asc, label, body, type_defs)?
                    .is_assignable_to(typ, type_defs, ind)?
            }

            (Self::Self_(_, label1), Self::Self_(_, label2)) => {
                ind.contains(&(label1.clone(), label2.clone()))
            }

            (Self::SendTypes(span, names1, body1), Self::SendTypes(_, names2, body2))
            | (Self::ReceiveTypes(span, names1, body1), Self::ReceiveTypes(_, names2, body2)) => {
                if names1.len() != names2.len() {
                    return Ok(false);
                }
                let mut body2 = *body2.clone();
                for (name1, name2) in names1.iter().zip(names2) {
                    body2 = body2.substitute(name2, &Type::Var(span.clone(), name1.clone()))?;
                }
                let mut type_defs = type_defs.clone();
                type_defs.vars.extend(names1.clone());
                body1.is_assignable_to(&body2, &type_defs, ind)?
            }

            _ => false,
        })
    }

    pub fn dual(&self, type_defs: &TypeDefs<Name>) -> Result<Self, TypeError<Name>> {
        Ok(match self {
            Self::Chan(_, t) => *t.clone(),

            Self::Var(span, name) => {
                Self::Chan(span.clone(), Box::new(Self::Var(span.clone(), name.clone())))
            }
            Self::Name(span, name, args) => match type_defs.get_dual(span, name, args) {
                Ok(dual) => dual,
                Err(_) => Self::Chan(
                    span.clone(),
                    Box::new(Self::Name(span.clone(), name.clone(), args.clone())),
                ),
            },

            Self::Send(span, types, u) => {
                Self::Receive(span.clone(), types.clone(), Box::new(u.dual(type_defs)?))
            }
            Self::Receive(span, types, u) => {
                Self::Send(span.clone(), types.clone(), Box::new(u.dual(type_defs)?))
            }
            Self::Either(span, branches) => Self::Choice(
                span.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| Ok((branch.clone(), t.dual(type_defs)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Either(
                span.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| Ok((branch.clone(), t.dual(type_defs)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Continue(span.clone()),
            Self::Continue(span) => Self::Break(span.clone()),

            Self::Recursive { span, asc, label, body: t } => Self::Iterative {
                span: span.clone(),
                asc: asc.clone(),
                label: label.clone(),
                body: Box::new(t.dual(type_defs)?.chan_self(label)),
            },
            Self::Iterative { span, asc, label, body: t } => Self::Recursive {
                span: span.clone(),
                asc: asc.clone(),
                label: label.clone(),
                body: Box::new(t.dual(type_defs)?.chan_self(label)),
            },
            Self::Self_(span, label) => Self::Chan(
                span.clone(),
                Box::new(Self::Self_(span.clone(), label.clone())),
            ),

            Self::SendTypes(span, names, t) => {
                Self::ReceiveTypes(span.clone(), names.clone(), Box::new(t.dual(type_defs)?))
            }
            Self::ReceiveTypes(span, names, t) => {
                Self::SendTypes(span.clone(), names.clone(), Box::new(t.dual(type_defs)?))
            }
        })
    }

    fn chan_self(self, label: &Option<Name>) -> Self {
        match self {
            Self::Chan(span, t) => match *t {
                Self::Self_(span, label1) if &label1 == label => Self::Self_(span, label1),
                t => Self::Chan(span, Box::new(t.chan_self(label))),
            },

            Self::Var(span, name) => Self::Var(span, name),
            Self::Name(span, name, args) => Self::Name(
                span.clone(),
                name.clone(),
                args.into_iter().map(|arg| arg.chan_self(label)).collect(),
            ),

            Self::Send(span, types, u) => Self::Send(
                span.clone(),
                types.into_iter().map(|t| t.chan_self(label)).collect(),
                Box::new(u.chan_self(label)),
            ),
            Self::Receive(span, types, u) => Self::Receive(
                span.clone(),
                types.into_iter().map(|t| t.chan_self(label)).collect(),
                Box::new(u.chan_self(label)),
            ),
            Self::Either(span, branches) => Self::Either(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.chan_self(label)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Self::Choice(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.chan_self(label)))
                    .collect(),
            ),
            Self::Break(span) => Self::Break(span.clone()),
            Self::Continue(span) => Self::Continue(span.clone()),

            Self::Recursive { span, asc, label: label1, body: t } => {
                if &label1 == label {
                    Self::Recursive { span, asc, label: label1, body: t }
                } else {
                    Self::Recursive { span, asc, label: label1, body: Box::new(t.chan_self(label)) }
                }
            }
            Self::Iterative { span, asc, label: label1, body: t } => {
                if &label1 == label {
                    Self::Iterative { span, asc, label: label1, body: t }
                } else {
                    Self::Iterative { span, asc, label: label1, body: Box::new(t.chan_self(label)) }
                }
            }
            Self::Self_(span, label1) => {
                if &label1 == label {
                    Self::Chan(span.clone(), Box::new(Self::Self_(span, label1)))
                } else {
                    Self::Self_(span, label1)
                }
            }

            Self::SendTypes(span, names, t) => {
                Self::SendTypes(span.clone(), names.clone(), Box::new(t.chan_self(label)))
            }
            Self::ReceiveTypes(span, names, t) => {
                Self::ReceiveTypes(span.clone(), names.clone(), Box::new(t.chan_self(label)))
            }
        }
    }

    pub fn expand_recursive(
        asc: &IndexSet<Option<Name>>,
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Name>,
    ) -> Result<Self, TypeError<Name>> {
        body.clone()
            .expand_recursive_helper(asc, label, body, type_defs)
    }

    fn expand_recursive_helper(
        self,
        top_asc: &IndexSet<Option<Name>>,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Name>,
    ) -> Result<Self, TypeError<Name>> {
        Ok(match self {
            Self::Chan(span, t) => match *t {
                Self::Self_(span, label) if &label == top_label => Self::Iterative{
                    span,
                    asc: top_asc.clone(),
                    label: label.clone(),
                    body: Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                },
                t => Self::Chan(
                    span,
                    Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(span, name) => Self::Var(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(span, types, u) => {
                let mut expanded_types = Vec::with_capacity(types.len());
                for t in types {
                    expanded_types.push(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?);
                }
                Self::Send(
                    span,
                    expanded_types,
                    Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                )
            },
            Self::Receive(span, types, u) => {
                let mut expanded_types = Vec::with_capacity(types.len());
                for t in types {
                    expanded_types.push(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?);
                }
                Self::Receive(
                    span,
                    expanded_types,
                    Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                )
            },
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive { span, asc, label, body: t } => {
                if &label == top_label {
                    Self::Recursive { span, asc, label, body: t }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Iterative { span, asc, label, body: t } => Self::Iterative {
                span,
                asc,
                label,
                body: Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            },
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Recursive { span, asc: top_asc.clone(), label, body: Box::new(top_body.clone()) }
                } else {
                    Self::Self_(span, label)
                }
            }

            Self::SendTypes(span, names, t) => Self::SendTypes(
                span,
                names,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::ReceiveTypes(span, names, t) => Self::ReceiveTypes(
                span,
                names,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn expand_iterative(
        asc: &IndexSet<Option<Name>>,
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Name>,
    ) -> Result<Self, TypeError<Name>> {
        body.clone()
            .expand_iterative_helper(asc, label, body, type_defs)
    }

    fn expand_iterative_helper(
        self,
        top_asc: &IndexSet<Option<Name>>,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Name>,
    ) -> Result<Self, TypeError<Name>> {
        Ok(match self {
            Self::Chan(span, t) => match *t {
                Self::Self_(span, label) if &label == top_label => Self::Recursive {
                    span,
                    asc: top_asc.clone(),
                    label: label.clone(),
                    body: Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                },
                t => Self::Chan(
                    span,
                    Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(span, name) => Self::Var(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(span, types, u) => {
                let mut expanded_types = Vec::with_capacity(types.len());
                for t in types {
                    expanded_types.push(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?);
                }
                Self::Send(
                    span,
                    expanded_types,
                    Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                )
            },
            Self::Receive(span, types, u) => {
                let mut expanded_types = Vec::with_capacity(types.len());
                for t in types {
                    expanded_types.push(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?);
                }
                Self::Receive(
                    span,
                    expanded_types,
                    Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                )
            },
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive { span, asc, label, body: t } => Self::Recursive {
                span,
                asc,
                label,
                body: Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
            },
            Self::Iterative { span, asc, label, body: t } => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: t
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        )
                    }
                }
            }
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone())
                    }
                } else {
                    Self::Self_(span, label)
                }
            }

            Self::SendTypes(span, names, t) => Self::SendTypes(
                span,
                names,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::ReceiveTypes(span, names, t) => Self::ReceiveTypes(
                span,
                names,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    fn invalidate_ascendent(&mut self, label: &Option<Name>) {
        match self {
            Self::Var(_, _) => {}
            Self::Name(_, _, args) => {
                for arg in args {
                    arg.invalidate_ascendent(label);
                }
            }
            Self::Send(_, types, u) => {
                types.into_iter().for_each(|t| t.invalidate_ascendent(label));
                u.invalidate_ascendent(label);
            }
            Self::Receive(_, types, u) => {
                types.into_iter().for_each(|t| t.invalidate_ascendent(label));
                u.invalidate_ascendent(label);
            }
            Self::Either(_, branches) => {
                for (_, t) in branches {
                    t.invalidate_ascendent(label);
                }
            }
            Self::Choice(_, branches) => {
                for (_, t) in branches {
                    t.invalidate_ascendent(label);
                }
            }
            Self::Break(_) => {}
            Self::Continue(_) => {}

            Self::Recursive { span: _, asc, label: _, body: t } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Iterative { span: _, asc, label: _, body: t } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Self_(_, _) => {}

            Self::SendTypes(_, _, t) => {
                t.invalidate_ascendent(label);
            }
            Self::ReceiveTypes(_, _, t) => {
                t.invalidate_ascendent(label);
            }

            Self::Chan(_, t) => {
                t.invalidate_ascendent(label);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context<Name> {
    type_defs: TypeDefs<Name>,
    declarations: Arc<IndexMap<Name, (Span, Type<Name>)>>,
    unchecked_definitions: Arc<IndexMap<Name, (Span, Arc<process::Expression<Name, ()>>)>>,
    checked_definitions: Arc<RwLock<IndexMap<Name, CheckedDef<Name>>>>,
    current_deps: IndexSet<Name>,
    variables: IndexMap<Name, Type<Name>>,
    loop_points: IndexMap<Option<Name>, (Name, Arc<IndexMap<Name, Type<Name>>>)>,
}

#[derive(Clone, Debug)]
struct CheckedDef<Name> {
    span: Span,
    def: Arc<process::Expression<Name, Type<Name>>>,
    typ: Type<Name>,
}

impl<Name> Context<Name>
where
    Name: Clone + Eq + Hash,
{
    pub fn new_with_type_checking(
        program: &Program<Name, Arc<process::Expression<Name, ()>>>,
    ) -> Result<Self, TypeError<Name>> {
        let type_defs = TypeDefs::new_with_validation(&program.type_defs)?;

        let mut unchecked_definitions = IndexMap::new();
        for Definition { span, name, expression } in &program.definitions {
            if let Some((span1, _)) =
                unchecked_definitions.insert(name.clone(), (span.clone(), expression.clone()))
            {
                return Err(TypeError::NameAlreadyDefined(
                    span.clone(),
                    span1.clone(),
                    name.clone(),
                ));
            }
        }

        let mut declarations = IndexMap::new();
        for Declaration { span, name, typ } in &program.declarations {
            if !unchecked_definitions.contains_key(name) {
                return Err(TypeError::DeclaredButNotDefined(span.clone(), name.clone()));
            }
            if let Some((span1, _)) = declarations.insert(name.clone(), (span.clone(), typ.clone())) {
                return Err(TypeError::NameAlreadyDeclared(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let mut context = Context {
            type_defs,
            declarations: Arc::new(declarations),
            unchecked_definitions: Arc::new(unchecked_definitions),
            checked_definitions: Arc::new(RwLock::new(IndexMap::new())),
            current_deps: IndexSet::new(),
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
        };

        let names_to_check = context
            .unchecked_definitions
            .iter()
            .map(|(name, (span, _))| (span.clone(), name.clone()))
            .collect::<Vec<_>>();
        for (span, name) in names_to_check {
            context.check_definition(&span, &name)?;
        }

        Ok(context)
    }

    fn check_definition(
        &mut self,
        span: &Span,
        name: &Name,
    ) -> Result<Type<Name>, TypeError<Name>> {
        if let Some(checked) = self.checked_definitions.read().unwrap().get(name) {
            return Ok(checked.typ.clone());
        }

        let Some((span_def, unchecked_def)) = self.unchecked_definitions.get(name).cloned() else {
            return Err(TypeError::NameNotDefined(span.clone(), name.clone()));
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
                let checked_def = self.check_expression(None, &unchecked_def, &declared_type)?;
                (checked_def, declared_type)
            }
            None => self.infer_expression(None, &unchecked_def)?,
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

    pub fn get_checked_definitions(
        &self,
    ) -> Vec<Definition<Name, Arc<process::Expression<Name, Type<Name>>>>> {
        self.checked_definitions
            .read()
            .unwrap()
            .iter()
            .map(|(name, checked)| Definition {
                span: checked.span.clone(),
                name: name.clone(),
                expression: checked.def.clone()
            })
            .collect()
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

    pub fn get_variable(&mut self, name: &Name) -> Option<Type<Name>> {
        self.variables.shift_remove(name)
    }

    pub fn get(&mut self, span: &Span, name: &Name) -> Result<Type<Name>, TypeError<Name>> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => self.check_definition(span, name),
        }
    }

    pub fn put(
        &mut self,
        span: &Span,
        name: Name,
        typ: Type<Name>,
    ) -> Result<(), TypeError<Name>> {
        if let Some(_) = self.variables.get(&name) {
            return Err(TypeError::ShadowedObligation(span.clone(), name));
        }
        self.variables.insert(name, typ);
        Ok(())
    }

    fn invalidate_ascendent(&mut self, label: &Option<Name>) {
        for (_, t) in &mut self.variables {
            t.invalidate_ascendent(label);
        }
    }

    pub fn capture(
        &mut self,
        inference_subject: Option<&Name>,
        cap: &process::Captures<Span, Name>,
        target: &mut Self,
    ) -> Result<(), TypeError<Name>> {
        for (name, span) in &cap.names {
            if Some(name) == inference_subject {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    name.clone(),
                ));
            }
            let value = match self.get_variable(name) {
                Some(value) => value,
                None => continue,
            };
            target.put(span, name.clone(), value)?;
        }
        Ok(())
    }

    pub fn obligations(&self) -> impl Iterator<Item = &Name> {
        self.variables.iter().map(|(name, _)| name)
    }

    pub fn check_process(
        &mut self,
        process: &process::Process<Span, Name, ()>,
    ) -> Result<Arc<process::Process<Span, Name, Type<Name>>>, TypeError<Name>> {
        match process {
            process::Process::Let(span, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(None, expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(None, expression)?,
                };
                self.put(span, name.clone(), typ.clone())?;
                let process = self.check_process(process)?;
                Ok(Arc::new(process::Process::Let(
                    span.clone(),
                    name.clone(),
                    annotation.clone(),
                    typ,
                    expression,
                    process,
                )))
            }

            process::Process::Do(span, object, (), command) => {
                let typ = self.get(span, object)?;

                let (command, _) = self.check_command(
                    None,
                    span,
                    object,
                    &typ,
                    command,
                    &mut |context, process| Ok((context.check_process(process)?, None)),
                )?;

                Ok(Arc::new(process::Process::Do(
                    span.clone(),
                    object.clone(),
                    typ,
                    command,
                )))
            }

            process::Process::Telltypes(span, _) => {
                Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
            }
        }
    }

    fn check_command(
        &mut self,
        inference_subject: Option<&Name>,
        span: &Span,
        object: &Name,
        typ: &Type<Name>,
        command: &process::Command<Span, Name, ()>,
        analyze_process: &mut impl FnMut(
            &mut Self,
            &process::Process<Span, Name, ()>,
        ) -> Result<
            (
                Arc<process::Process<Span, Name, Type<Name>>>,
                Option<Type<Name>>,
            ),
            TypeError<Name>,
        >,
    ) -> Result<(process::Command<Span, Name, Type<Name>>, Option<Type<Name>>), TypeError<Name>>
    {
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
        if !matches!(command, process::Command::Link(_)) {
            if let Type::Iterative { asc: top_asc, label: top_label, body, .. } = typ {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_iterative(top_asc, top_label, body, &self.type_defs)?,
                    command,
                    analyze_process,
                );
            }
        }
        if !matches!(command, process::Command::Begin(_, _, _) | process::Command::Loop(_)) {
            if let Type::Recursive { asc: top_asc, label: top_label, body, .. } = typ {
                return self.check_command(
                    inference_subject,
                    span,
                    object,
                    &Type::expand_recursive(top_asc, top_label, body, &self.type_defs)?,
                    command,
                    analyze_process,
                );
            }
        }
        if let Type::Chan(_, dual_typ) = typ {
            match dual_typ.dual(&self.type_defs)? {
                Type::Chan(_, _) => {}
                typ => {
                    return self.check_command(
                        inference_subject,
                        span,
                        object,
                        &typ,
                        command,
                        analyze_process,
                    )
                }
            }
        }

        Ok(match command {
            process::Command::Link(expression) => {
                let expression =
                    self.check_expression(None, expression, &typ.dual(&self.type_defs)?)?;
                self.cannot_have_obligations(span)?;
                (process::Command::Link(expression), None)
            }

            process::Command::Send(argument, process) => {
                let Type::Receive(_, argument_types, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Send(span.clone()),
                        typ.clone(),
                    ));
                };
                let mut argument_types = argument_types.into_iter();
                let argument_type = argument_types.next().unwrap();
                let argument_types: Vec<_> = argument_types.map(Type::clone).collect();
                let then_type = if argument_types.is_empty() {
                    *then_type.clone()
                } else {
                    Type::Receive(
                        span.clone(),
                        argument_types,
                        Box::from(*then_type.clone()),
                    )
                };
                let argument = self.check_expression(None, argument, argument_type)?;
                self.put(span, object.clone(), then_type)?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (process::Command::Send(argument, process), inferred_types)
            }

            process::Command::Receive(parameter, annotation, process) => {
                let Type::Send(_, parameter_types, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Receive(span.clone()),
                        typ.clone(),
                    ));
                };
                let mut parameter_types = parameter_types.into_iter();
                let parameter_type = parameter_types.next().unwrap();
                let parameter_types: Vec<_> = parameter_types.map(Type::clone).collect();
                let then_type = if parameter_types.is_empty() {
                    *then_type.clone()
                } else {
                    Type::Send(
                        span.clone(),
                        parameter_types,
                        Box::from(*then_type.clone()),
                    )
                };
                if let Some(annotated_type) = annotation {
                    parameter_type.check_assignable(span, annotated_type, &self.type_defs)?;
                }
                self.put(span, parameter.clone(), parameter_type.clone())?;
                self.put(span, object.clone(), then_type)?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    process::Command::Receive(parameter.clone(), annotation.clone(), process),
                    inferred_types,
                )
            }

            process::Command::Choose(chosen, process) => {
                let Type::Choice(_, branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Choose(span.clone(), chosen.clone()),
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
                (process::Command::Choose(chosen.clone(), process), inferred_types)
            }

            process::Command::Match(branches, processes) => {
                let Type::Either(_, required_branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Match(span.clone(), Arc::clone(branches)),
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
                let mut inferred_type: Option<Type<Name>> = None;

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
                        (Some(t1), Some(t2))
                            if t1.is_assignable_to(&t2, &self.type_defs, &HashSet::new())? =>
                        {
                            inferred_type = Some(t2)
                        }
                        (Some(t1), Some(t2))
                            if !t2.is_assignable_to(&t1, &self.type_defs, &HashSet::new())? =>
                        {
                            return Err(TypeError::TypesCannotBeUnified(t1, t2))
                        }
                        (t1, _) => inferred_type = t1,
                    }
                }

                (
                    process::Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    inferred_type,
                )
            }

            process::Command::Break => {
                let Type::Continue(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Break(span.clone()),
                        typ.clone(),
                    ));
                };
                self.cannot_have_obligations(span)?;
                (process::Command::Break, None)
            }

            process::Command::Continue(process) => {
                let Type::Break(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Continue(span.clone()),
                        typ.clone(),
                    ));
                };
                let (process, inferred_types) = analyze_process(self, process)?;
                (process::Command::Continue(process), inferred_types)
            }

            process::Command::Begin(unfounded, label, process) => {
                let Type::Recursive { span: typ_span, asc: typ_asc, label: typ_label, body: typ_body } = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Begin(span.clone(), label.clone()),
                        typ.clone(),
                    ));
                };

                let mut typ_asc = typ_asc.clone();

                if !*unfounded {
                    typ_asc.insert(label.clone());
                }

                self.invalidate_ascendent(label);
                self.loop_points.insert(
                    label.clone(),
                    (
                        object.clone(),
                        Arc::new({
                            let mut variables = self.variables.clone();
                            variables.insert(
                                object.clone(),
                                Type::Recursive {
                                    span: typ_span.clone(),
                                    asc: typ_asc.clone(),
                                    label: typ_label.clone(),
                                    body: typ_body.clone()
                                },
                            );
                            variables
                        }),
                    ),
                );

                self.put(
                    span,
                    object.clone(),
                    Type::expand_recursive(&typ_asc, typ_label, typ_body, &self.type_defs)?,
                )?;
                let (process, inferred_type) = analyze_process(self, process)?;

                let inferred_iterative = inferred_type.map(|body| {
                    Type::Iterative {
                        span: span.clone(),
                        asc: typ_asc,
                        label: label.clone(),
                        body: Box::new(body)
                    }
                });

                (
                    process::Command::Begin(*unfounded, label.clone(), process),
                    inferred_iterative,
                )
            }

            process::Command::Loop(label) => {
                if !matches!(typ, Type::Recursive { .. }) {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::Loop(span.clone(), label.clone()),
                        typ.clone(),
                    ));
                }
                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(span.clone(), label.clone()));
                };
                self.put(span, driver.clone(), typ.clone())?;

                if let (Type::Recursive { asc: asc1, .. }, Some(Type::Recursive { asc: asc2, .. })) =
                    (typ, variables.get(&driver))
                {
                    for label in asc2 {
                        if !asc1.contains(label) {
                            return Err(TypeError::DoesNotDescendSubjectOfBegin(
                                span.clone(),
                                label.clone(),
                            ));
                        }
                    }
                }

                let mut inferred_loop = None;

                for (var, type_at_begin) in variables.as_ref() {
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
                    if !current_type.is_assignable_to(
                        type_at_begin,
                        &self.type_defs,
                        &HashSet::new(),
                    )? {
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
                    process::Command::Loop(label.clone()),
                    inferred_loop.or(Some(Type::Self_(span.clone(), label.clone()))),
                )
            }

            process::Command::SendType(argument, process) => {
                let Type::ReceiveTypes(_, type_names, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::SendType(span.clone()),
                        typ.clone(),
                    ));
                };
                let mut type_names = type_names.into_iter();
                let type_name = type_names.next().unwrap();
                let type_names: Vec<_> = type_names.map(Name::clone).collect();
                let then_type = if type_names.is_empty() {
                    *then_type.clone()
                } else {
                    Type::ReceiveTypes(
                        span.clone(),
                        type_names,
                        Box::from(*then_type.clone()),
                    )
                };
                let then_type = then_type.clone().substitute(type_name, argument)?;
                self.put(span, object.clone(), then_type)?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (process::Command::SendType(argument.clone(), process), inferred_types)
            }

            process::Command::ReceiveType(parameter, process) => {
                let Type::SendTypes(_, type_names, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        span.clone(),
                        Operation::ReceiveType(span.clone()),
                        typ.clone(),
                    ));
                };
                let mut type_names = type_names.into_iter();
                let type_name = type_names.next().unwrap();
                let type_names: Vec<_> = type_names.map(Name::clone).collect();
                let then_type = if type_names.is_empty() {
                    *then_type.clone()
                } else {
                    Type::SendTypes(
                        span.clone(),
                        type_names,
                        Box::from(*then_type.clone()),
                    )
                };
                let then_type = then_type
                    .clone()
                    .substitute(type_name, &Type::Var(span.clone(), parameter.clone()))?;
                self.type_defs.vars.insert(parameter.clone());
                self.put(span, object.clone(), then_type)?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    process::Command::ReceiveType(parameter.clone(), process),
                    inferred_types,
                )
            }
        })
    }

    pub fn infer_process(
        &mut self,
        process: &process::Process<Span, Name, ()>,
        subject: &Name,
    ) -> Result<(Arc<process::Process<Span, Name, Type<Name>>>, Type<Name>), TypeError<Name>>
    {
        match process {
            process::Process::Let(span, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(Some(subject), expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(Some(subject), expression)?,
                };
                self.put(span, name.clone(), typ.clone())?;
                let (process, subject_type) = self.infer_process(process, subject)?;
                Ok((
                    Arc::new(process::Process::Let(
                        span.clone(),
                        name.clone(),
                        annotation.clone(),
                        typ,
                        expression,
                        process,
                    )),
                    subject_type,
                ))
            }

            process::Process::Do(span, object, (), command) => {
                if object == subject {
                    let (command, typ) = self.infer_command(span, subject, command)?;
                    return Ok((
                        Arc::new(process::Process::Do(
                            span.clone(),
                            object.clone(),
                            typ.clone(),
                            command,
                        )),
                        typ,
                    ));
                }
                let typ = self.get(span, object)?;

                let (command, inferred_type) = self.check_command(
                    Some(subject),
                    span,
                    object,
                    &typ,
                    command,
                    &mut |context, process| {
                        let (process, typ) = context.infer_process(process, subject)?;
                        Ok((process, Some(typ)))
                    },
                )?;

                let Some(inferred_type) = inferred_type else {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        span.clone(),
                        subject.clone(),
                    ));
                };

                Ok((
                    Arc::new(process::Process::Do(span.clone(), object.clone(), typ, command)),
                    inferred_type,
                ))
            }

            process::Process::Telltypes(span, _) => {
                Err(TypeError::Telltypes(span.clone(), self.variables.clone()))
            }
        }
    }

    pub fn infer_command(
        &mut self,
        span: &Span,
        subject: &Name,
        command: &process::Command<Span, Name, ()>,
    ) -> Result<(process::Command<Span, Name, Type<Name>>, Type<Name>), TypeError<Name>> {
        Ok(match command {
            process::Command::Link(expression) => {
                let (expression, typ) = self.infer_expression(Some(subject), expression)?;
                (process::Command::Link(expression), typ.dual(&self.type_defs)?)
            }

            process::Command::Send(argument, process) => {
                let (argument, arg_type) = self.infer_expression(Some(subject), argument)?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    process::Command::Send(argument, process),
                    Type::Receive(span.clone(), vec![arg_type], Box::new(then_type)),
                )
            }

            process::Command::Receive(parameter, annotation, process) => {
                let Some(param_type) = annotation else {
                    return Err(TypeError::ParameterTypeMustBeKnown(
                        span.clone(),
                        subject.clone(),
                        parameter.clone(),
                    ));
                };
                self.put(span, parameter.clone(), param_type.clone())?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    process::Command::Receive(parameter.clone(), annotation.clone(), process),
                    Type::Send(
                        span.clone(),
                        vec![param_type.clone()],
                        Box::new(then_type),
                    ),
                )
            }

            process::Command::Choose(_, _) => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    subject.clone(),
                ))
            }

            process::Command::Match(branches, processes) => {
                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut branch_types = IndexMap::new();

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();
                    let (process, typ) = self.infer_process(process, subject)?;
                    typed_processes.push(process);
                    branch_types.insert(branch.clone(), typ);
                }

                (
                    process::Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    Type::Either(span.clone(), branch_types),
                )
            }

            process::Command::Break => {
                self.cannot_have_obligations(span)?;
                (process::Command::Break, Type::Continue(span.clone()))
            }

            process::Command::Continue(process) => {
                let process = self.check_process(process)?;
                (process::Command::Continue(process), Type::Break(span.clone()))
            }

            process::Command::Begin(unfounded, label, process) => {
                self.loop_points.insert(
                    label.clone(),
                    (subject.clone(), Arc::new(self.variables.clone())),
                );
                let (process, body) = self.infer_process(process, subject)?;
                (
                    process::Command::Begin(*unfounded, label.clone(), process),
                    Type::Recursive {
                        span: span.clone(),
                        asc: if *unfounded {
                            IndexSet::new()
                        } else {
                            IndexSet::from([label.clone()])
                        },
                        label: label.clone(),
                        body: Box::new(body)
                    },
                )
            }

            process::Command::Loop(label) => {
                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(span.clone(), label.clone()));
                };
                if &driver != subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        span.clone(),
                        subject.clone(),
                    ));
                }

                for (var, type_at_begin) in variables.as_ref() {
                    let Some(current_type) = self.get_variable(var) else {
                        return Err(TypeError::LoopVariableNotPreserved(
                            span.clone(),
                            var.clone(),
                        ));
                    };
                    if !current_type.is_assignable_to(
                        type_at_begin,
                        &self.type_defs,
                        &HashSet::new(),
                    )? {
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
                    process::Command::Loop(label.clone()),
                    Type::Self_(span.clone(), label.clone()),
                )
            }

            process::Command::SendType(_, _) => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    span.clone(),
                    subject.clone(),
                ))
            }

            process::Command::ReceiveType(parameter, process) => {
                self.type_defs.vars.insert(parameter.clone());
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    process::Command::ReceiveType(parameter.clone(), process),
                    Type::SendTypes(span.clone(), vec![parameter.clone()], Box::new(then_type)),
                )
            }
        })
    }

    pub fn check_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &process::Expression<Name, ()>,
        target_type: &Type<Name>,
    ) -> Result<Arc<process::Expression<Name, Type<Name>>>, TypeError<Name>> {
        match expression {
            process::Expression::Reference(span, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        span.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(span, name)?;
                typ.check_assignable(span, target_type, &self.type_defs)?;
                Ok(Arc::new(process::Expression::Reference(
                    span.clone(),
                    name.clone(),
                    typ.clone(),
                )))
            }

            process::Expression::Fork { span, captures, chan_name: channel, chan_annotation: annotation, process, .. } => {
                let target_dual = target_type.dual(&self.type_defs)?;
                let (chan_type, expr_type) = match annotation {
                    Some(annotated_type) => {
                        annotated_type.check_assignable(span, &target_dual, &self.type_defs)?;
                        (annotated_type.clone(), target_type) // or annotated_type.dual() ???
                    }
                    None => (target_dual, target_type),
                };
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                context.put(span, channel.clone(), chan_type.clone())?;
                let process = context.check_process(process)?;
                Ok(Arc::new(process::Expression::Fork {
                    span: span.clone(),
                    captures: captures.clone(),
                    chan_name: channel.clone(),
                    chan_annotation: annotation.clone(),
                    chan_type,
                    expr_type: expr_type.clone(),
                    process,
                }))
            }
        }
    }

    pub fn infer_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &process::Expression<Name, ()>,
    ) -> Result<(Arc<process::Expression<Name, Type<Name>>>, Type<Name>), TypeError<Name>>
    {
        match expression {
            process::Expression::Reference(span, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        span.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(span, name)?;
                Ok((
                    Arc::new(process::Expression::Reference(
                        span.clone(),
                        name.clone(),
                        typ.clone(),
                    )),
                    typ.clone(),
                ))
            }

            process::Expression::Fork { span, captures, chan_name: channel, chan_annotation: annotation, process, .. } => {
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                let (process, typ) = match annotation {
                    Some(typ) => {
                        context.put(span, channel.clone(), typ.clone())?;
                        (context.check_process(process)?, typ.clone())
                    }
                    None => context.infer_process(process, channel)?,
                };
                let dual = typ.dual(&self.type_defs)?;
                Ok((
                    Arc::new(process::Expression::Fork {
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
        }
    }

    pub fn cannot_have_obligations(&mut self, span: &Span) -> Result<(), TypeError<Name>> {
        if self.obligations().any(|_| true) {
            return Err(TypeError::UnfulfilledObligations(
                span.clone(),
                self.obligations().cloned().collect(),
            ));
        }
        Ok(())
    }
}

impl<Name: Display> Type<Name> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Chan(_, body) => {
                write!(f, "chan ")?;
                body.pretty(f, indent)
            }
            Self::Var(_, name) => write!(f, "{}", name),
            Self::Name(_, name, args) => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty(f, indent)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }

            Self::Send(_, args, then) => {
                write!(f, "(")?;
                let mut args = args.into_iter();
                args.next().unwrap().pretty(f, indent)?;
                for arg in args {
                    write!(f, ", ")?;
                    arg.pretty(f, indent)?;
                }
                write!(f, ") ")?;
                then.pretty(f, indent)
            }

            Self::Receive(_, params, then) => {
                write!(f, "[")?;
                let mut params = params.into_iter();
                params.next().unwrap().pretty(f, indent)?;
                for arg in params {
                    write!(f, ", ")?;
                    arg.pretty(f, indent)?;
                }
                write!(f, "] ")?;
                then.pretty(f, indent)
            }

            Self::Either(_, branches) => {
                write!(f, "either {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} ", branch)?;
                    typ.pretty(f, indent + 1)?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Choice(_, branches) => {
                write!(f, "{{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, "{} => ", branch)?;
                    typ.pretty(f, indent + 1)?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { asc, label, body, .. } => {
                write!(f, "recursive ")?;
                if let Some(label) = label {
                    write!(f, ":{} ", label)?;
                }
                if asc.len() > 0 {
                    write!(f, "/* descends ")?;
                    for (i, label) in asc.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        match label {
                            Some(label) => write!(f, ":{}", label)?,
                            None => write!(f, "unlabeled")?,
                        }
                    }
                    write!(f, " */ ")?;
                }
                body.pretty(f, indent)
            }

            Self::Iterative { asc, label, body, .. } => {
                write!(f, "iterative ")?;
                if let Some(label) = label {
                    write!(f, ":{} ", label)?;
                }
                if asc.len() > 0 {
                    write!(f, "/* descends ")?;
                    for (i, label) in asc.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        match label {
                            Some(label) => write!(f, ":{}", label)?,
                            None => write!(f, "unlabeled")?,
                        }
                    }
                    write!(f, " */ ")?;
                }
                body.pretty(f, indent)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, " :{}", label)?;
                }
                Ok(())
            }

            Self::SendTypes(_, names, body) => {
                let mut names = names.iter();
                write!(f, "(type {}", names.next().unwrap())?;
                for name in names {
                    write!(f, "{}", name)?;
                }
                write!(f, ") ")?;
                body.pretty(f, indent)
            }

            Self::ReceiveTypes(_, names, body) => {
                let mut names = names.iter();
                write!(f, "[type {}", names.next().unwrap())?;
                for name in names {
                    write!(f, "{}", name)?;
                }
                write!(f, "] ")?;
                body.pretty(f, indent)
            }
        }
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}

fn two_labels_from_two_spans(
    code: &str,
    span1: &Span,
    span2: &Span,
    label1: impl Into<Option<String>>,
    label2: impl Into<Option<String>>,
) -> Vec<LabeledSpan> {
    use crate::playground::labels_from_span;
    let mut labels = labels_from_span(code, span1);
    let label1 = label1.into();
    let label2 = label2.into();
    labels.iter_mut().for_each(|x| x.set_label(label1.clone()));
    let mut labels2 = labels_from_span(code, span2);
    labels2.iter_mut().for_each(|x| x.set_label(label2.clone()));
    labels.extend(labels2);
    labels
}

impl<Name: Display> TypeError<Name> {
    pub fn to_report(&self, source_code: Arc<str>) -> miette::Report {
        use crate::playground::labels_from_span;
        let code = &source_code;
        match self {
            Self::TypeNameAlreadyDefined(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already defined here".to_owned()),
                    "Type `{}` is already defined.", name
                )
            }
            Self::NameAlreadyDeclared(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already declared here".to_owned()),
                    "`{}` is already declared.",
                    name,
                )
            }
            Self::NameAlreadyDefined(span1, span2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(code, span1, span2, "this".to_owned(), "is already defined here".to_owned()),
                    "`{}` is already defined",
                    name,
                )
            }
            Self::DeclaredButNotDefined(span,  name) => {
                let mut labels = labels_from_span(code, span);
                labels.iter_mut().for_each(|x| {
                    x.set_label(Some("declared here".to_owned()));
                });
                miette::miette!(
                    labels = labels,
                    "`{}` is declared, but is missing a corresponding definition.",
                    name
                )
            }
            Self::NoMatchingRecursiveOrIterative(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `self` has no matching `recursive` or `iterative`.",
                )
            }
            Self::SelfUsedInNegativePosition(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `self` is used in a negative position.\n\nNegative self-references are not allowed."
                )
            }
            Self::TypeNameNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type `{}` is not defined.", name)
            }
            Self::DependencyCycle(span, deps) => {
                let labels = labels_from_span(code, span);
                let mut deps_str = String::new();
                for (i, dep) in deps.iter().enumerate() {
                    if i > 0 {
                        write!(&mut deps_str, " -> ").unwrap();
                    }
                    write!(&mut deps_str, "{}", dep).unwrap();
                }
                miette::miette!(
                    labels = labels,
                    "There is a dependency cycle:\n\n  {}\n\nDependency cycles are not allowed.",
                    deps_str
                )
            }
            Self::WrongNumberOfTypeArgs(span, name, required_number, provided_number) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Type `{}` has {} type arguments, but {} were provided.",
                    name,
                    required_number,
                    provided_number
                )
            }
            Self::NameNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "`{}` is not defined.", name)
            }
            Self::ShadowedObligation(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Cannot re-assign `{}` before handling it.",
                    name,
                )
            }
            Self::TypeMustBeKnownAtThisPoint(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type must be known at this point.")
            }
            Self::ParameterTypeMustBeKnown(span, _, param) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Type of parameter `{}` must be known.",
                    param,
                )
            }
            Self::CannotAssignFromTo(span, from_type, to_type) => {
                let labels = labels_from_span(code, span);
                let (mut from_type_str, mut to_type_str) = (String::new(), String::new());
                from_type.pretty(&mut from_type_str, 1).unwrap();
                to_type.pretty(&mut to_type_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "This type was required:\n\n  {}\n\nBut an incompatible type was provided:\n\n  {}\n",
                    to_type_str,
                    from_type_str,
                )
            }
            Self::UnfulfilledObligations(span, names) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "Cannot end this process before handling {}.",
                    names
                        .iter()
                        .enumerate()
                        .map(|(i, name)| if i == 0 {
                            format!("`{}`", name)
                        } else {
                            format!(", `{}`", name)
                        })
                        .collect::<String>()
                )
            }
            Self::InvalidOperation(span, _, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "This operation cannot be performed on:\n\n  {}\n",
                    typ_str
                )
            }
            Self::InvalidBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` is not available on:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::MissingBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` was not handled for:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::RedundantBranch(span, branch, typ) => {
                let labels = labels_from_span(code, span);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` is not possible for:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::TypesCannotBeUnified(typ1, typ2) => {
                miette::miette!(
                    labels = two_labels_from_two_spans(
                        code,
                        &typ1.span(),
                        &typ2.span(),
                        "this".to_owned(),
                        "should operate on the same type as this".to_owned()
                    ),
                    "Operations cannot be performed on the same type."
                )
            }
            Self::NoSuchLoopPoint(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "There is no matching loop point in scope.")
            }
            Self::DoesNotDescendSubjectOfBegin(span, _) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `loop` may diverge. Value does not descend from the corresponding `begin`.\n\nIf this is intended, use `unfounded begin`.",
                )
            }
            Self::LoopVariableNotPreserved(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "`{}` is used by next iteration, but is no longer defined.",
                    name,
                )
            }
            Self::LoopVariableChangedType(span, name, loop_type, begin_type) => {
                let labels = labels_from_span(code, span);
                let (mut loop_type_str, mut begin_type_str) = (String::new(), String::new());
                loop_type.pretty(&mut loop_type_str, 1).unwrap();
                begin_type.pretty(&mut begin_type_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "For next iteration, `{}` is required to be:\n\n  {}\n\nBut it has an incompatible type:\n\n  {}\n",
                    name,
                    begin_type_str,
                    loop_type_str,
                )
            }
            Self::Telltypes(span, variables) => {
                let labels = labels_from_span(code, span);
                let mut buf = String::new();
                for (name, typ) in variables {
                    write!(&mut buf, "{}: ", name).unwrap();
                    typ.pretty(&mut buf, 0).unwrap();
                    write!(&mut buf, "\n\n").unwrap();
                }
                miette::miette! {
                    labels = labels,
                    "{}",
                    buf
                }
            }
        }.with_source_code(source_code)
    }
}

impl<Name> TypeError<Name> {
    pub fn spans(&self) -> (Span, Option<Span>) {
        match self {
            | Self::TypeNameAlreadyDefined(span1, span2, _)
            | Self::NameAlreadyDeclared(span1, span2, _)
            | Self::NameAlreadyDefined(span1, span2, _)
            => (span1.clone(), Some(span2.clone())),

            | Self::DeclaredButNotDefined(span, _)
            | Self::NoMatchingRecursiveOrIterative(span, _)
            | Self::SelfUsedInNegativePosition(span, _)
            | Self::TypeNameNotDefined(span, _)
            | Self::DependencyCycle(span, _)
            | Self::WrongNumberOfTypeArgs(span, _, _, _)
            | Self::NameNotDefined(span, _)
            | Self::ShadowedObligation(span, _)
            | Self::TypeMustBeKnownAtThisPoint(span, _)
            | Self::ParameterTypeMustBeKnown(span, _, _)
            | Self::CannotAssignFromTo(span, _, _)
            | Self::UnfulfilledObligations(span, _)
            | Self::InvalidOperation(span, _, _)
            | Self::InvalidBranch(span, _, _)
            | Self::MissingBranch(span, _, _)
            | Self::RedundantBranch(span, _, _)
            | Self::NoSuchLoopPoint(span, _)
            | Self::DoesNotDescendSubjectOfBegin(span, _)
            | Self::LoopVariableNotPreserved(span, _)
            | Self::LoopVariableChangedType(span, _, _, _)
            | Self::Telltypes(span, _)
            => (span.clone(), None),

            Self::TypesCannotBeUnified(typ1, typ2)
            => (typ1.span(), Some(typ2.span())),

        }
    }
}