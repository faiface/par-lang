use indexmap::{IndexMap, IndexSet};
use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
    hash::Hash,
    sync::{Arc, RwLock},
};

use super::{
    parse::Program,
    process::{Captures, Command, Expression, Process},
};
use crate::par::parse::Loc;
use miette::LabeledSpan;

#[derive(Clone, Debug)]
pub enum TypeError<Loc, Name> {
    TypeNameAlreadyDefined(Loc, Loc, Name),
    NameAlreadyDeclared(Loc, Loc, Name),
    NameAlreadyDefined(Loc, Loc, Name),
    DeclaredButNotDefined(Loc, Name),
    NoMatchingRecursiveOrIterative(Loc, Option<Name>),
    SelfUsedInNegativePosition(Loc, Option<Name>),
    TypeNameNotDefined(Loc, Name),
    DependencyCycle(Loc, Vec<Name>),
    WrongNumberOfTypeArgs(Loc, Name, usize, usize),
    NameNotDefined(Loc, Name),
    ShadowedObligation(Loc, Name),
    TypeMustBeKnownAtThisPoint(Loc, Name),
    ParameterTypeMustBeKnown(Loc, Name, Name),
    CannotAssignFromTo(Loc, Type<Loc, Name>, Type<Loc, Name>),
    UnfulfilledObligations(Loc, Vec<Name>),
    InvalidOperation(Loc, Operation<Loc, Name>, Type<Loc, Name>),
    InvalidBranch(Loc, Name, Type<Loc, Name>),
    MissingBranch(Loc, Name, Type<Loc, Name>),
    RedundantBranch(Loc, Name, Type<Loc, Name>),
    TypesCannotBeUnified(Type<Loc, Name>, Type<Loc, Name>),
    NoSuchLoopPoint(Loc, Option<Name>),
    DoesNotDescendSubjectOfBegin(Loc, Option<Name>),
    LoopVariableNotPreserved(Loc, Name),
    LoopVariableChangedType(Loc, Name, Type<Loc, Name>, Type<Loc, Name>),
    Telltypes(Loc, IndexMap<Name, Type<Loc, Name>>),
}

#[derive(Clone, Debug)]
pub enum Operation<Loc, Name> {
    Send(Loc),
    Receive(Loc),
    Choose(Loc, Name),
    Match(Loc, #[allow(unused)] Arc<[Name]>),
    Break(Loc),
    Continue(Loc),
    Begin(Loc, Option<Name>),
    Loop(Loc, Option<Name>),
    SendType(Loc),
    ReceiveType(Loc),
}

#[derive(Clone, Debug)]
pub enum Type<Loc, Name> {
    Chan(Loc, Box<Self>),
    Var(Loc, Name),
    Name(Loc, Name, Vec<Type<Loc, Name>>),
    Send(Loc, Box<Self>, Box<Self>),
    Receive(Loc, Box<Self>, Box<Self>),
    Either(Loc, IndexMap<Name, Self>),
    Choice(Loc, IndexMap<Name, Self>),
    Break(Loc),
    Continue(Loc),
    Recursive(Loc, IndexSet<Option<Name>>, Option<Name>, Box<Self>),
    Iterative(Loc, IndexSet<Option<Name>>, Option<Name>, Box<Self>),
    Self_(Loc, Option<Name>),
    SendType(Loc, Name, Box<Self>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct TypeDefs<Loc, Name> {
    pub globals: Arc<IndexMap<Name, (Loc, Vec<Name>, Type<Loc, Name>)>>,
    pub vars: IndexSet<Name>,
}

impl<Loc: Clone, Name: Clone + Eq + Hash> Default for TypeDefs<Loc, Name> {
    fn default() -> Self {
        Self {
            globals: Default::default(),
            vars: Default::default(),
        }
    }
}

impl<Loc: Clone, Name: Clone + Eq + Hash> TypeDefs<Loc, Name> {
    pub fn new_with_validation(
        globals: &[(Loc, Name, Vec<Name>, Type<Loc, Name>)],
    ) -> Result<Self, TypeError<Loc, Name>> {
        let mut globals_map = IndexMap::new();
        for (loc, name, params, typ) in globals {
            if let Some((loc1, _, _)) =
                globals_map.insert(name.clone(), (loc.clone(), params.clone(), typ.clone()))
            {
                return Err(TypeError::TypeNameAlreadyDefined(
                    loc.clone(),
                    loc1,
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
        loc: &Loc,
        name: &Name,
        args: &[Type<Loc, Name>],
    ) -> Result<Type<Loc, Name>, TypeError<Loc, Name>> {
        if self.vars.contains(name) {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfTypeArgs(
                    loc.clone(),
                    name.clone(),
                    0,
                    args.len(),
                ));
            }
            return Ok(Type::Var(loc.clone(), name.clone()));
        }
        match self.globals.get(name) {
            Some((_, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        loc.clone(),
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
            None => Err(TypeError::TypeNameNotDefined(loc.clone(), name.clone())),
        }
    }

    pub fn get_dual(
        &self,
        loc: &Loc,
        name: &Name,
        args: &[Type<Loc, Name>],
    ) -> Result<Type<Loc, Name>, TypeError<Loc, Name>> {
        if self.vars.contains(name) {
            if !args.is_empty() {
                return Err(TypeError::WrongNumberOfTypeArgs(
                    loc.clone(),
                    name.clone(),
                    0,
                    args.len(),
                ));
            }
            return Ok(Type::Chan(
                loc.clone(),
                Box::new(Type::Var(loc.clone(), name.clone())),
            ));
        }
        match self.globals.get(name) {
            Some((_, params, typ)) => {
                if params.len() != args.len() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        loc.clone(),
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
            None => Err(TypeError::TypeNameNotDefined(loc.clone(), name.clone())),
        }
    }

    fn validate_type(
        &self,
        typ: &Type<Loc, Name>,
        deps: &IndexSet<Name>,
        self_pos: &IndexSet<Option<Name>>,
        self_neg: &IndexSet<Option<Name>>,
    ) -> Result<(), TypeError<Loc, Name>> {
        Ok(match typ {
            Type::Chan(_, t) => self.validate_type(t, deps, self_neg, self_pos)?,
            Type::Var(loc, name) => {
                self.get(loc, name, &[])?;
            }
            Type::Name(loc, name, args) => {
                let mut deps = deps.clone();
                if !self.vars.contains(name) {
                    if !deps.insert(name.clone()) {
                        return Err(TypeError::DependencyCycle(
                            loc.clone(),
                            deps.into_iter().skip_while(|dep| dep != name).collect(),
                        ));
                    }
                }
                let t = self.get(loc, name, args)?;
                self.validate_type(&t, &deps, self_pos, self_neg)?;
            }
            Type::Send(_, t, u) => {
                self.validate_type(t, deps, self_pos, self_neg)?;
                self.validate_type(u, deps, self_pos, self_neg)?;
            }
            Type::Receive(_, t, u) => {
                self.validate_type(t, deps, self_neg, self_pos)?;
                self.validate_type(u, deps, self_pos, self_neg)?;
            }
            Type::Either(_, branches) | Type::Choice(_, branches) => {
                for (_, t) in branches {
                    self.validate_type(t, deps, self_pos, self_neg)?;
                }
            }
            Type::Break(_) | Type::Continue(_) => (),
            Type::Recursive(_, _, label, body) | Type::Iterative(_, _, label, body) => {
                let (mut self_pos, mut self_neg) = (self_pos.clone(), self_neg.clone());
                self_pos.insert(label.clone());
                self_neg.shift_remove(label);
                self.validate_type(body, deps, &self_pos, &self_neg)?;
            }
            Type::Self_(loc, label) => {
                if self_neg.contains(label) {
                    return Err(TypeError::SelfUsedInNegativePosition(
                        loc.clone(),
                        label.clone(),
                    ));
                }
                if !self_pos.contains(label) {
                    return Err(TypeError::NoMatchingRecursiveOrIterative(
                        loc.clone(),
                        label.clone(),
                    ));
                }
            }
            Type::SendType(_, name, body) | Type::ReceiveType(_, name, body) => {
                let mut with_var = self.clone();
                with_var.vars.insert(name.clone());
                with_var.validate_type(body, deps, self_pos, self_neg)?;
            }
        })
    }
}

impl<Loc, Name> Type<Loc, Name> {
    pub fn get_loc(&self) -> &Loc {
        match self {
            Self::Chan(loc, _) => loc,
            Self::Var(loc, _) => loc,
            Self::Name(loc, _, _) => loc,
            Self::Send(loc, _, _) => loc,
            Self::Receive(loc, _, _) => loc,
            Self::Either(loc, _) => loc,
            Self::Choice(loc, _) => loc,
            Self::Break(loc) => loc,
            Self::Continue(loc) => loc,
            Self::Recursive(loc, _, _, _) => loc,
            Self::Iterative(loc, _, _, _) => loc,
            Self::Self_(loc, _) => loc,
            Self::SendType(loc, _, _) => loc,
            Self::ReceiveType(loc, _, _) => loc,
        }
    }
}

impl<Loc, Name: Eq + Hash> Type<Loc, Name> {
    pub fn map_names<N: Eq + Hash>(self, f: &mut impl FnMut(Name) -> N) -> Type<Loc, N> {
        match self {
            Self::Chan(loc, t) => Type::Chan(loc, Box::new(t.map_names(f))),
            Self::Var(loc, name) => Type::Var(loc, f(name)),
            Self::Name(loc, name, args) => Type::Name(
                loc,
                f(name),
                args.into_iter().map(|arg| arg.map_names(f)).collect(),
            ),
            Self::Send(loc, t, u) => {
                Type::Send(loc, Box::new(t.map_names(f)), Box::new(u.map_names(f)))
            }
            Self::Receive(loc, t, u) => {
                Type::Receive(loc, Box::new(t.map_names(f)), Box::new(u.map_names(f)))
            }
            Self::Either(loc, branches) => Type::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Type::Choice(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Break(loc) => Type::Break(loc),
            Self::Continue(loc) => Type::Continue(loc),
            Self::Recursive(loc, asc, label, body) => Type::Recursive(
                loc,
                asc.into_iter().map(|label| map_label(label, f)).collect(),
                map_label(label, f),
                Box::new(body.map_names(f)),
            ),
            Self::Iterative(loc, asc, label, body) => Type::Iterative(
                loc,
                asc.into_iter().map(|label| map_label(label, f)).collect(),
                map_label(label, f),
                Box::new(body.map_names(f)),
            ),
            Self::Self_(loc, label) => Type::Self_(loc, map_label(label, f)),
            Self::SendType(loc, name, body) => {
                Type::SendType(loc, f(name), Box::new(body.map_names(f)))
            }
            Self::ReceiveType(loc, name, body) => {
                Type::ReceiveType(loc, f(name), Box::new(body.map_names(f)))
            }
        }
    }
}

fn map_label<Name, N>(label: Option<Name>, f: &mut impl FnMut(Name) -> N) -> Option<N> {
    label.map(f)
}

impl<Loc: Clone, Name: Clone + Eq + Hash> Type<Loc, Name> {
    pub fn substitute(self, var: &Name, typ: &Self) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(loc, t) => Self::Chan(loc, Box::new(t.substitute(var, typ)?)),
            Self::Var(loc, name) => {
                if &name == var {
                    typ.clone()
                } else {
                    Self::Var(loc, name)
                }
            }
            Self::Name(loc, name, args) if &name == var => {
                if !args.is_empty() {
                    return Err(TypeError::WrongNumberOfTypeArgs(
                        loc,
                        var.clone(),
                        0,
                        args.len(),
                    ));
                }
                typ.clone()
            }
            Self::Name(loc, name, args) => Self::Name(
                loc,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(var, typ))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.substitute(var, typ)?),
                Box::new(u.substitute(var, typ)?),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.substitute(var, typ)?),
                Box::new(u.substitute(var, typ)?),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(var, typ)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(loc, branches) => Self::Choice(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(var, typ)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Continue(loc),

            Self::Recursive(loc, asc, label, body) => {
                Self::Recursive(loc, asc, label, Box::new(body.substitute(var, typ)?))
            }
            Self::Iterative(loc, asc, label, body) => {
                Self::Iterative(loc, asc, label, Box::new(body.substitute(var, typ)?))
            }
            Self::Self_(loc, label) => Self::Self_(loc, label),

            Self::SendType(loc, name, body) => {
                if &name == var {
                    Self::SendType(loc, name, body)
                } else {
                    Self::SendType(loc, name, Box::new(body.substitute(var, typ)?))
                }
            }
            Self::ReceiveType(loc, name, body) => {
                if &name == var {
                    Self::ReceiveType(loc, name, body)
                } else {
                    Self::ReceiveType(loc, name, Box::new(body.substitute(var, typ)?))
                }
            }
        })
    }

    pub fn is_linear(&self, type_defs: &TypeDefs<Loc, Name>) -> Result<bool, TypeError<Loc, Name>> {
        Ok(!self.is_positive(type_defs)?)
    }

    pub fn is_positive(
        &self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<bool, TypeError<Loc, Name>> {
        Ok(match self {
            Type::Chan(_, t) => t.is_negative(type_defs)?,
            Type::Var(_, _) => false,
            Type::Name(loc, name, args) => {
                type_defs.get(loc, name, args)?.is_positive(type_defs)?
            }
            Type::Send(_, t, u) => t.is_positive(type_defs)? && u.is_positive(type_defs)?,
            Type::Receive(_, _, _) => false,
            Type::Either(_, branches) => {
                for (_, t) in branches {
                    if !t.is_positive(type_defs)? {
                        return Ok(false);
                    }
                }
                true
            }
            Type::Choice(_, _) => false,
            Type::Break(_) => true,
            Type::Continue(_) => false,
            Type::Recursive(_, _, _, t) => t.is_positive(type_defs)?,
            Type::Iterative(_, _, _, t) => t.is_positive(type_defs)?,
            Type::Self_(_, _) => true,
            Type::SendType(loc, name, t) => t
                .clone()
                .substitute(name, &Type::Var(loc.clone(), name.clone()))?
                .is_positive(type_defs)?,
            Type::ReceiveType(loc, name, t) => t
                .clone()
                .substitute(name, &Type::Var(loc.clone(), name.clone()))?
                .is_positive(type_defs)?,
        })
    }

    pub fn is_negative(
        &self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<bool, TypeError<Loc, Name>> {
        Ok(match self {
            Type::Chan(_, t) => t.is_positive(type_defs)?,
            Type::Var(_, _) => false,
            Type::Name(loc, name, args) => {
                type_defs.get(loc, name, args)?.is_negative(type_defs)?
            }
            Type::Send(_, _, _) => false,
            Type::Receive(_, t, u) => t.is_positive(type_defs)? && u.is_negative(type_defs)?,
            Type::Either(_, _) => false,
            Type::Choice(_, branches) => {
                for (_, t) in branches {
                    if !t.is_negative(type_defs)? {
                        return Ok(false);
                    }
                }
                true
            }
            Type::Break(_) => false,
            Type::Continue(_) => true,
            Type::Recursive(_, _, _, t) => t.is_negative(type_defs)?,
            Type::Iterative(_, _, _, t) => t.is_negative(type_defs)?,
            Type::Self_(_, _) => true,
            Type::SendType(loc, name, t) => t
                .clone()
                .substitute(name, &Type::Var(loc.clone(), name.clone()))?
                .is_negative(type_defs)?,
            Type::ReceiveType(loc, name, t) => t
                .clone()
                .substitute(name, &Type::Var(loc.clone(), name.clone()))?
                .is_negative(type_defs)?,
        })
    }

    pub fn check_assignable(
        &self,
        loc: &Loc,
        u: &Type<Loc, Name>,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<(), TypeError<Loc, Name>> {
        if !self.is_assignable_to(u, type_defs, &HashSet::new())? {
            return Err(TypeError::CannotAssignFromTo(
                loc.clone(),
                self.clone(),
                u.clone(),
            ));
        }
        Ok(())
    }

    fn is_assignable_to(
        &self,
        other: &Self,
        type_defs: &TypeDefs<Loc, Name>,
        ind: &HashSet<(Option<Name>, Option<Name>)>,
    ) -> Result<bool, TypeError<Loc, Name>> {
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
            (Self::Name(loc, name, args), t2) => type_defs
                .get(loc, name, args)?
                .is_assignable_to(t2, type_defs, ind)?,
            (t1, Self::Name(loc, name, args)) => {
                t1.is_assignable_to(&type_defs.get(loc, name, args)?, type_defs, ind)?
            }

            (Self::Send(_, t1, u1), Self::Send(_, t2, u2)) => {
                t1.is_assignable_to(t2, type_defs, ind)?
                    && u1.is_assignable_to(u2, type_defs, ind)?
            }
            (Self::Receive(_, t1, u1), Self::Receive(_, t2, u2)) => {
                t2.is_assignable_to(t1, type_defs, ind)?
                    && u1.is_assignable_to(u2, type_defs, ind)?
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

            (Self::Recursive(_, asc1, label1, body1), Self::Recursive(_, asc2, label2, body2)) => {
                if !asc2.iter().all(|label| asc1.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (typ, Self::Recursive(_, asc, label, body)) => typ.is_assignable_to(
                &Self::expand_recursive(asc, label, body, type_defs)?,
                type_defs,
                ind,
            )?,
            (Self::Iterative(_, asc1, label1, body1), Self::Iterative(_, asc2, label2, body2)) => {
                if !asc2.iter().all(|label| asc1.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (Self::Iterative(_, asc, label, body), typ) => {
                Self::expand_iterative(asc, label, body, type_defs)?
                    .is_assignable_to(typ, type_defs, ind)?
            }

            (Self::Self_(_, label1), Self::Self_(_, label2)) => {
                ind.contains(&(label1.clone(), label2.clone()))
            }

            (Self::SendType(loc, name1, body1), Self::SendType(_, name2, body2)) => {
                let body2 = body2
                    .clone()
                    .substitute(name2, &Type::Var(loc.clone(), name1.clone()))?;
                let mut type_defs = type_defs.clone();
                type_defs.vars.insert(name1.clone());
                body1.is_assignable_to(&body2, &type_defs, ind)?
            }
            (Self::ReceiveType(loc, name1, body1), Self::ReceiveType(_, name2, body2)) => {
                let body2 = body2
                    .clone()
                    .substitute(name2, &Type::Var(loc.clone(), name1.clone()))?;
                let mut type_defs = type_defs.clone();
                type_defs.vars.insert(name1.clone());
                body1.is_assignable_to(&body2, &type_defs, ind)?
            }

            _ => false,
        })
    }

    pub fn dual(&self, type_defs: &TypeDefs<Loc, Name>) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(_, t) => *t.clone(),

            Self::Var(loc, name) => {
                Self::Chan(loc.clone(), Box::new(Self::Var(loc.clone(), name.clone())))
            }
            Self::Name(loc, name, args) => match type_defs.get_dual(loc, name, args) {
                Ok(dual) => dual,
                Err(_) => Self::Chan(
                    loc.clone(),
                    Box::new(Self::Name(loc.clone(), name.clone(), args.clone())),
                ),
            },

            Self::Send(loc, t, u) => {
                Self::Receive(loc.clone(), t.clone(), Box::new(u.dual(type_defs)?))
            }
            Self::Receive(loc, t, u) => {
                Self::Send(loc.clone(), t.clone(), Box::new(u.dual(type_defs)?))
            }
            Self::Either(loc, branches) => Self::Choice(
                loc.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| Ok((branch.clone(), t.dual(type_defs)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(loc, branches) => Self::Either(
                loc.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| Ok((branch.clone(), t.dual(type_defs)?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(loc) => Self::Continue(loc.clone()),
            Self::Continue(loc) => Self::Break(loc.clone()),

            Self::Recursive(loc, asc, label, t) => Self::Iterative(
                loc.clone(),
                asc.clone(),
                label.clone(),
                Box::new(t.dual(type_defs)?.chan_self(label)),
            ),
            Self::Iterative(loc, asc, label, t) => Self::Recursive(
                loc.clone(),
                asc.clone(),
                label.clone(),
                Box::new(t.dual(type_defs)?.chan_self(label)),
            ),
            Self::Self_(loc, label) => Self::Chan(
                loc.clone(),
                Box::new(Self::Self_(loc.clone(), label.clone())),
            ),

            Self::SendType(loc, name, t) => {
                Self::ReceiveType(loc.clone(), name.clone(), Box::new(t.dual(type_defs)?))
            }
            Self::ReceiveType(loc, name, t) => {
                Self::SendType(loc.clone(), name.clone(), Box::new(t.dual(type_defs)?))
            }
        })
    }

    fn chan_self(self, label: &Option<Name>) -> Self {
        match self {
            Self::Chan(loc, t) => match *t {
                Self::Self_(loc, label1) if &label1 == label => Self::Self_(loc, label1),
                t => Self::Chan(loc, Box::new(t.chan_self(label))),
            },

            Self::Var(loc, name) => Self::Var(loc, name),
            Self::Name(loc, name, args) => Self::Name(
                loc.clone(),
                name.clone(),
                args.into_iter().map(|arg| arg.chan_self(label)).collect(),
            ),

            Self::Send(loc, t, u) => Self::Send(
                loc.clone(),
                Box::new(t.chan_self(label)),
                Box::new(u.chan_self(label)),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc.clone(),
                Box::new(t.chan_self(label)),
                Box::new(u.chan_self(label)),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.chan_self(label)))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Self::Choice(
                loc.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.chan_self(label)))
                    .collect(),
            ),
            Self::Break(loc) => Self::Break(loc.clone()),
            Self::Continue(loc) => Self::Continue(loc.clone()),

            Self::Recursive(loc, asc, label1, t) => {
                if &label1 == label {
                    Self::Recursive(loc, asc, label1, t)
                } else {
                    Self::Recursive(loc, asc, label1, Box::new(t.chan_self(label)))
                }
            }
            Self::Iterative(loc, asc, label1, t) => {
                if &label1 == label {
                    Self::Iterative(loc, asc, label1, t)
                } else {
                    Self::Iterative(loc, asc, label1, Box::new(t.chan_self(label)))
                }
            }
            Self::Self_(loc, label1) => {
                if &label1 == label {
                    Self::Chan(loc.clone(), Box::new(Self::Self_(loc, label1)))
                } else {
                    Self::Self_(loc, label1)
                }
            }

            Self::SendType(loc, name, t) => {
                Self::SendType(loc.clone(), name.clone(), Box::new(t.chan_self(label)))
            }
            Self::ReceiveType(loc, name, t) => {
                Self::ReceiveType(loc.clone(), name.clone(), Box::new(t.chan_self(label)))
            }
        }
    }

    pub fn expand_recursive(
        asc: &IndexSet<Option<Name>>,
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        body.clone()
            .expand_recursive_helper(asc, label, body, type_defs)
    }

    fn expand_recursive_helper(
        self,
        top_asc: &IndexSet<Option<Name>>,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(loc, t) => match *t {
                Self::Self_(loc, label) if &label == top_label => Self::Iterative(
                    loc,
                    top_asc.clone(),
                    label.clone(),
                    Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                ),
                t => Self::Chan(
                    loc,
                    Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(loc, name) => Self::Var(loc, name),
            Self::Name(loc, name, args) => Self::Name(
                loc,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
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
            Self::Choice(loc, branches) => Self::Choice(
                loc,
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
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Continue(loc),

            Self::Recursive(loc, asc, label, t) => {
                if &label == top_label {
                    Self::Recursive(loc, asc, label, t)
                } else {
                    Self::Recursive(
                        loc,
                        asc,
                        label,
                        Box::new(
                            t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    )
                }
            }
            Self::Iterative(loc, asc, label, t) => Self::Iterative(
                loc,
                asc,
                label,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Self_(loc, label) => {
                if &label == top_label {
                    Self::Recursive(loc, top_asc.clone(), label, Box::new(top_body.clone()))
                } else {
                    Self::Self_(loc, label)
                }
            }

            Self::SendType(loc, name, t) => Self::SendType(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::ReceiveType(loc, name, t) => Self::ReceiveType(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn expand_iterative(
        asc: &IndexSet<Option<Name>>,
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        body.clone()
            .expand_iterative_helper(asc, label, body, type_defs)
    }

    fn expand_iterative_helper(
        self,
        top_asc: &IndexSet<Option<Name>>,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(loc, t) => match *t {
                Self::Self_(loc, label) if &label == top_label => Self::Recursive(
                    loc,
                    top_asc.clone(),
                    label.clone(),
                    Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                ),
                t => Self::Chan(
                    loc,
                    Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(loc, name) => Self::Var(loc, name),
            Self::Name(loc, name, args) => Self::Name(
                loc,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
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
            Self::Choice(loc, branches) => Self::Choice(
                loc,
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
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Continue(loc),

            Self::Recursive(loc, asc, label, t) => Self::Recursive(
                loc,
                asc,
                label,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Iterative(loc, asc, label, t) => {
                if &label == top_label {
                    Self::Iterative(loc, asc, label, t)
                } else {
                    Self::Iterative(
                        loc,
                        asc,
                        label,
                        Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    )
                }
            }
            Self::Self_(loc, label) => {
                if &label == top_label {
                    Self::Iterative(loc, top_asc.clone(), label, Box::new(top_body.clone()))
                } else {
                    Self::Self_(loc, label)
                }
            }

            Self::SendType(loc, name, t) => Self::SendType(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::ReceiveType(loc, name, t) => Self::ReceiveType(
                loc,
                name,
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
            Self::Send(_, t, u) => {
                t.invalidate_ascendent(label);
                u.invalidate_ascendent(label);
            }
            Self::Receive(_, t, u) => {
                t.invalidate_ascendent(label);
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

            Self::Recursive(_, asc, _, t) => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Iterative(_, asc, _, t) => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Self_(_, _) => {}

            Self::SendType(_, _, t) => {
                t.invalidate_ascendent(label);
            }
            Self::ReceiveType(_, _, t) => {
                t.invalidate_ascendent(label);
            }

            Self::Chan(_, t) => {
                t.invalidate_ascendent(label);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context<Loc, Name> {
    type_defs: TypeDefs<Loc, Name>,
    declarations: Arc<IndexMap<Name, (Loc, Type<Loc, Name>)>>,
    unchecked_definitions: Arc<IndexMap<Name, (Loc, Arc<Expression<Loc, Name, ()>>)>>,
    checked_definitions: Arc<RwLock<IndexMap<Name, CheckedDef<Loc, Name>>>>,
    current_deps: IndexSet<Name>,
    variables: IndexMap<Name, Type<Loc, Name>>,
    loop_points: IndexMap<Option<Name>, (Name, Arc<IndexMap<Name, Type<Loc, Name>>>)>,
}

#[derive(Clone, Debug)]
struct CheckedDef<Loc, Name> {
    loc: Loc,
    def: Arc<Expression<Loc, Name, Type<Loc, Name>>>,
    typ: Type<Loc, Name>,
}

impl<Loc, Name> Context<Loc, Name>
where
    Loc: std::fmt::Debug + Clone + Eq + Hash,
    Name: Clone + Eq + Hash + std::fmt::Debug,
{
    pub fn new_with_type_checking(
        program: &Program<Loc, Name, Arc<Expression<Loc, Name, ()>>>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        let type_defs = TypeDefs::new_with_validation(&program.type_defs)?;

        let mut unchecked_definitions = IndexMap::new();
        for (loc, name, expr) in &program.definitions {
            if let Some((loc1, _)) =
                unchecked_definitions.insert(name.clone(), (loc.clone(), expr.clone()))
            {
                return Err(TypeError::NameAlreadyDefined(
                    loc.clone(),
                    loc1.clone(),
                    name.clone(),
                ));
            }
        }

        let mut declarations = IndexMap::new();
        for (loc, name, typ) in &program.declarations {
            if !unchecked_definitions.contains_key(name) {
                return Err(TypeError::DeclaredButNotDefined(loc.clone(), name.clone()));
            }
            if let Some((loc1, _)) = declarations.insert(name.clone(), (loc.clone(), typ.clone())) {
                return Err(TypeError::NameAlreadyDeclared(
                    loc.clone(),
                    loc1,
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
            .map(|(name, (loc, _))| (loc.clone(), name.clone()))
            .collect::<Vec<_>>();
        for (loc, name) in names_to_check {
            context.check_definition(&loc, &name)?;
        }

        Ok(context)
    }

    fn check_definition(
        &mut self,
        loc: &Loc,
        name: &Name,
    ) -> Result<Type<Loc, Name>, TypeError<Loc, Name>> {
        if let Some(checked) = self.checked_definitions.read().unwrap().get(name) {
            return Ok(checked.typ.clone());
        }

        let Some((loc_def, unchecked_def)) = self.unchecked_definitions.get(name).cloned() else {
            return Err(TypeError::NameNotDefined(loc.clone(), name.clone()));
        };

        if !self.current_deps.insert(name.clone()) {
            return Err(TypeError::DependencyCycle(
                loc.clone(),
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
                loc: loc_def,
                def: checked_def,
                typ: checked_type.clone(),
            },
        );

        Ok(checked_type)
    }

    pub fn get_checked_definitions(
        &self,
    ) -> IndexMap<Name, (Loc, Arc<Expression<Loc, Name, Type<Loc, Name>>>)> {
        self.checked_definitions
            .read()
            .unwrap()
            .iter()
            .map(|(name, checked)| (name.clone(), (checked.loc.clone(), checked.def.clone())))
            .collect()
    }
    pub fn get_declarations(&self) -> IndexMap<Name, (Loc, Type<Loc, Name>)> {
        (*self.declarations).clone()
    }
    pub fn get_type_defs(&self) -> &TypeDefs<Loc, Name> {
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

    pub fn get_variable(&mut self, name: &Name) -> Option<Type<Loc, Name>> {
        self.variables.shift_remove(name)
    }

    pub fn get(&mut self, loc: &Loc, name: &Name) -> Result<Type<Loc, Name>, TypeError<Loc, Name>> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => self.check_definition(loc, name),
        }
    }

    pub fn put(
        &mut self,
        loc: &Loc,
        name: Name,
        typ: Type<Loc, Name>,
    ) -> Result<(), TypeError<Loc, Name>> {
        if let Some(typ) = self.variables.get(&name) {
            if typ.is_linear(&self.type_defs)? {
                return Err(TypeError::ShadowedObligation(loc.clone(), name));
            }
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
        cap: &Captures<Loc, Name>,
        target: &mut Self,
    ) -> Result<(), TypeError<Loc, Name>> {
        for (name, loc) in &cap.names {
            if Some(name) == inference_subject {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    loc.clone(),
                    name.clone(),
                ));
            }
            let typ = match self.get_variable(name) {
                Some(typ) => typ,
                None => continue,
            };
            if !typ.is_linear(&self.type_defs)? {
                self.put(loc, name.clone(), typ.clone())?;
            }
            target.put(loc, name.clone(), typ)?;
        }
        Ok(())
    }

    pub fn obligations(&self) -> impl Iterator<Item = &Name> {
        self.variables
            .iter()
            .filter(|(_, typ)| typ.is_linear(&self.type_defs).ok().unwrap_or(true))
            .map(|(name, _)| name)
    }

    pub fn check_process(
        &mut self,
        process: &Process<Loc, Name, ()>,
    ) -> Result<Arc<Process<Loc, Name, Type<Loc, Name>>>, TypeError<Loc, Name>> {
        match process {
            Process::Let(loc, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(None, expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(None, expression)?,
                };
                self.put(loc, name.clone(), typ.clone())?;
                let process = self.check_process(process)?;
                Ok(Arc::new(Process::Let(
                    loc.clone(),
                    name.clone(),
                    annotation.clone(),
                    typ,
                    expression,
                    process,
                )))
            }

            Process::Do(loc, object, (), command) => {
                let typ = self.get(loc, object)?;

                let (command, _) = self.check_command(
                    None,
                    loc,
                    object,
                    &typ,
                    command,
                    &mut |context, process| Ok((context.check_process(process)?, None)),
                )?;

                Ok(Arc::new(Process::Do(
                    loc.clone(),
                    object.clone(),
                    typ,
                    command,
                )))
            }

            Process::Telltypes(loc, _) => {
                return Err(TypeError::Telltypes(loc.clone(), self.variables.clone()))
            }
        }
    }

    fn check_command(
        &mut self,
        inference_subject: Option<&Name>,
        loc: &Loc,
        object: &Name,
        typ: &Type<Loc, Name>,
        command: &Command<Loc, Name, ()>,
        analyze_process: &mut impl FnMut(
            &mut Self,
            &Process<Loc, Name, ()>,
        ) -> Result<
            (
                Arc<Process<Loc, Name, Type<Loc, Name>>>,
                Option<Type<Loc, Name>>,
            ),
            TypeError<Loc, Name>,
        >,
    ) -> Result<(Command<Loc, Name, Type<Loc, Name>>, Option<Type<Loc, Name>>), TypeError<Loc, Name>>
    {
        if let Type::Name(_, name, args) = typ {
            return self.check_command(
                inference_subject,
                loc,
                object,
                &self.type_defs.get(loc, name, args)?,
                command,
                analyze_process,
            );
        }
        if !matches!(command, Command::Link(_)) {
            if let Type::Iterative(_, top_asc, top_label, body) = typ {
                return self.check_command(
                    inference_subject,
                    loc,
                    object,
                    &Type::expand_iterative(top_asc, top_label, body, &self.type_defs)?,
                    command,
                    analyze_process,
                );
            }
        }
        if !matches!(command, Command::Begin(_, _, _, _) | Command::Loop(_, _)) {
            if let Type::Recursive(_, top_asc, top_label, body) = typ {
                return self.check_command(
                    inference_subject,
                    loc,
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
                        loc,
                        object,
                        &typ,
                        command,
                        analyze_process,
                    )
                }
            }
        }

        Ok(match command {
            Command::Link(expression) => {
                let expression =
                    self.check_expression(None, expression, &typ.dual(&self.type_defs)?)?;
                self.cannot_have_obligations(loc)?;
                (Command::Link(expression), None)
            }

            Command::Send(argument, process) => {
                let Type::Receive(_, argument_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Send(loc.clone()),
                        typ.clone(),
                    ));
                };
                let argument = self.check_expression(None, argument, &argument_type)?;
                self.put(loc, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Send(argument, process), inferred_types)
            }

            Command::Receive(parameter, annotation, process) => {
                let Type::Send(_, parameter_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Receive(loc.clone()),
                        typ.clone(),
                    ));
                };
                if let Some(annotated_type) = annotation {
                    parameter_type.check_assignable(loc, annotated_type, &self.type_defs)?;
                }
                self.put(loc, parameter.clone(), *parameter_type.clone())?;
                self.put(loc, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    Command::Receive(parameter.clone(), annotation.clone(), process),
                    inferred_types,
                )
            }

            Command::Choose(chosen, process) => {
                let Type::Choice(_, branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Choose(loc.clone(), chosen.clone()),
                        typ.clone(),
                    ));
                };
                let Some(branch_type) = branches.get(chosen) else {
                    return Err(TypeError::InvalidBranch(
                        loc.clone(),
                        chosen.clone(),
                        typ.clone(),
                    ));
                };
                self.put(loc, object.clone(), branch_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Choose(chosen.clone(), process), inferred_types)
            }

            Command::Match(branches, processes) => {
                let Type::Either(_, required_branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Match(loc.clone(), Arc::clone(branches)),
                        typ.clone(),
                    ));
                };
                if let Some(missing) = required_branches
                    .keys()
                    .find(|&branch| !branches.contains(branch))
                {
                    return Err(TypeError::MissingBranch(
                        loc.clone(),
                        missing.clone(),
                        typ.clone(),
                    ));
                }

                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut inferred_type: Option<Type<Loc, Name>> = None;

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();

                    let Some(branch_type) = required_branches.get(branch) else {
                        return Err(TypeError::RedundantBranch(
                            loc.clone(),
                            branch.clone(),
                            typ.clone(),
                        ));
                    };
                    self.put(loc, object.clone(), branch_type.clone())?;
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
                    Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    inferred_type,
                )
            }

            Command::Break => {
                let Type::Continue(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Break(loc.clone()),
                        typ.clone(),
                    ));
                };
                self.cannot_have_obligations(loc)?;
                (Command::Break, None)
            }

            Command::Continue(process) => {
                let Type::Break(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Continue(loc.clone()),
                        typ.clone(),
                    ));
                };
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Continue(process), inferred_types)
            }

            Command::Begin(unfounded, label, captures, process) => {
                let Type::Recursive(typ_loc, typ_asc, typ_label, typ_body) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Begin(loc.clone(), label.clone()),
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
                            let mut variables = self
                                .variables
                                .iter()
                                .filter(|&(name, _)| captures.names.contains_key(name))
                                .map(|(name, typ)| (name.clone(), typ.clone()))
                                .collect::<IndexMap<_, _>>();
                            variables.insert(
                                object.clone(),
                                Type::Recursive(
                                    typ_loc.clone(),
                                    typ_asc.clone(),
                                    typ_label.clone(),
                                    typ_body.clone(),
                                ),
                            );
                            variables
                        }),
                    ),
                );

                self.put(
                    loc,
                    object.clone(),
                    Type::expand_recursive(&typ_asc, typ_label, typ_body, &self.type_defs)?,
                )?;
                let (process, inferred_type) = analyze_process(self, process)?;

                let inferred_iterative = inferred_type.map(|body| {
                    Type::Iterative(loc.clone(), typ_asc, label.clone(), Box::new(body))
                });

                (
                    Command::Begin(*unfounded, label.clone(), captures.clone(), process),
                    inferred_iterative,
                )
            }

            Command::Loop(label, captures) => {
                if !matches!(typ, Type::Recursive(_, _, _, _)) {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Loop(loc.clone(), label.clone()),
                        typ.clone(),
                    ));
                }
                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(loc.clone(), label.clone()));
                };
                self.put(loc, driver.clone(), typ.clone())?;

                if let (Type::Recursive(_, asc1, _, _), Some(Type::Recursive(_, asc2, _, _))) =
                    (typ, variables.get(&driver))
                {
                    for label in asc2 {
                        if !asc1.contains(label) {
                            return Err(TypeError::DoesNotDescendSubjectOfBegin(
                                loc.clone(),
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
                            loc.clone(),
                            var.clone(),
                        ));
                    };
                    if !current_type.is_assignable_to(
                        type_at_begin,
                        &self.type_defs,
                        &HashSet::new(),
                    )? {
                        return Err(TypeError::LoopVariableChangedType(
                            loc.clone(),
                            var.clone(),
                            current_type,
                            type_at_begin.clone(),
                        ));
                    }
                }
                self.cannot_have_obligations(loc)?;

                (
                    Command::Loop(label.clone(), captures.clone()),
                    inferred_loop.or(Some(Type::Self_(loc.clone(), label.clone()))),
                )
            }

            Command::SendType(argument, process) => {
                let Type::ReceiveType(_, type_name, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::SendType(loc.clone()),
                        typ.clone(),
                    ));
                };
                let then_type = then_type.clone().substitute(type_name, argument)?;
                self.put(loc, object.clone(), then_type)?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::SendType(argument.clone(), process), inferred_types)
            }

            Command::ReceiveType(parameter, process) => {
                let Type::SendType(_, type_name, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::ReceiveType(loc.clone()),
                        typ.clone(),
                    ));
                };
                let then_type = then_type
                    .clone()
                    .substitute(type_name, &Type::Var(loc.clone(), parameter.clone()))?;
                self.type_defs.vars.insert(parameter.clone());
                self.put(loc, object.clone(), then_type)?;
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
        process: &Process<Loc, Name, ()>,
        subject: &Name,
    ) -> Result<(Arc<Process<Loc, Name, Type<Loc, Name>>>, Type<Loc, Name>), TypeError<Loc, Name>>
    {
        match process {
            Process::Let(loc, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(Some(subject), expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(Some(subject), expression)?,
                };
                self.put(loc, name.clone(), typ.clone())?;
                let (process, subject_type) = self.infer_process(process, subject)?;
                Ok((
                    Arc::new(Process::Let(
                        loc.clone(),
                        name.clone(),
                        annotation.clone(),
                        typ,
                        expression,
                        process,
                    )),
                    subject_type,
                ))
            }

            Process::Do(loc, object, (), command) => {
                if object == subject {
                    let (command, typ) = self.infer_command(loc, subject, command)?;
                    return Ok((
                        Arc::new(Process::Do(
                            loc.clone(),
                            object.clone(),
                            typ.clone(),
                            command,
                        )),
                        typ,
                    ));
                }
                let typ = self.get(loc, object)?;

                let (command, inferred_type) = self.check_command(
                    Some(subject),
                    loc,
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
                        loc.clone(),
                        subject.clone(),
                    ));
                };

                Ok((
                    Arc::new(Process::Do(loc.clone(), object.clone(), typ, command)),
                    inferred_type,
                ))
            }

            Process::Telltypes(loc, _) => {
                return Err(TypeError::Telltypes(loc.clone(), self.variables.clone()))
            }
        }
    }

    pub fn infer_command(
        &mut self,
        loc: &Loc,
        subject: &Name,
        command: &Command<Loc, Name, ()>,
    ) -> Result<(Command<Loc, Name, Type<Loc, Name>>, Type<Loc, Name>), TypeError<Loc, Name>> {
        Ok(match command {
            Command::Link(expression) => {
                let (expression, typ) = self.infer_expression(Some(subject), expression)?;
                (Command::Link(expression), typ.dual(&self.type_defs)?)
            }

            Command::Send(argument, process) => {
                let (argument, arg_type) = self.infer_expression(Some(subject), argument)?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Send(argument, process),
                    Type::Receive(loc.clone(), Box::new(arg_type), Box::new(then_type)),
                )
            }

            Command::Receive(parameter, annotation, process) => {
                let Some(param_type) = annotation else {
                    return Err(TypeError::ParameterTypeMustBeKnown(
                        loc.clone(),
                        subject.clone(),
                        parameter.clone(),
                    ));
                };
                self.put(loc, parameter.clone(), param_type.clone())?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Receive(parameter.clone(), annotation.clone(), process),
                    Type::Send(
                        loc.clone(),
                        Box::new(param_type.clone()),
                        Box::new(then_type),
                    ),
                )
            }

            Command::Choose(_, _) => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    loc.clone(),
                    subject.clone(),
                ))
            }

            Command::Match(branches, processes) => {
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
                    Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    Type::Either(loc.clone(), branch_types),
                )
            }

            Command::Break => {
                self.cannot_have_obligations(loc)?;
                (Command::Break, Type::Continue(loc.clone()))
            }

            Command::Continue(process) => {
                let process = self.check_process(process)?;
                (Command::Continue(process), Type::Break(loc.clone()))
            }

            Command::Begin(unfounded, label, captures, process) => {
                self.loop_points.insert(
                    label.clone(),
                    (subject.clone(), Arc::new(self.variables.clone())),
                );
                let (process, body) = self.infer_process(process, subject)?;
                (
                    Command::Begin(*unfounded, label.clone(), captures.clone(), process),
                    Type::Recursive(
                        loc.clone(),
                        if *unfounded {
                            IndexSet::new()
                        } else {
                            IndexSet::from([label.clone()])
                        },
                        label.clone(),
                        Box::new(body),
                    ),
                )
            }

            Command::Loop(label, captures) => {
                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(loc.clone(), label.clone()));
                };
                if &driver != subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        subject.clone(),
                    ));
                }

                for (var, type_at_begin) in variables.as_ref() {
                    let Some(current_type) = self.get_variable(var) else {
                        return Err(TypeError::LoopVariableNotPreserved(
                            loc.clone(),
                            var.clone(),
                        ));
                    };
                    if !current_type.is_assignable_to(
                        type_at_begin,
                        &self.type_defs,
                        &HashSet::new(),
                    )? {
                        return Err(TypeError::LoopVariableChangedType(
                            loc.clone(),
                            var.clone(),
                            current_type,
                            type_at_begin.clone(),
                        ));
                    }
                }
                self.cannot_have_obligations(loc)?;

                (
                    Command::Loop(label.clone(), captures.clone()),
                    Type::Self_(loc.clone(), label.clone()),
                )
            }

            Command::SendType(_, _) => {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    loc.clone(),
                    subject.clone(),
                ))
            }

            Command::ReceiveType(parameter, process) => {
                self.type_defs.vars.insert(parameter.clone());
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::ReceiveType(parameter.clone(), process),
                    Type::SendType(loc.clone(), parameter.clone(), Box::new(then_type)),
                )
            }
        })
    }

    pub fn check_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &Expression<Loc, Name, ()>,
        target_type: &Type<Loc, Name>,
    ) -> Result<Arc<Expression<Loc, Name, Type<Loc, Name>>>, TypeError<Loc, Name>> {
        match expression {
            Expression::Reference(loc, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(loc, name)?;
                typ.check_assignable(loc, target_type, &self.type_defs)?;
                if !typ.is_linear(&self.type_defs)? {
                    self.put(loc, name.clone(), typ.clone())?;
                }
                Ok(Arc::new(Expression::Reference(
                    loc.clone(),
                    name.clone(),
                    typ.clone(),
                )))
            }

            Expression::Fork(loc, captures, channel, annotation, (), process) => {
                let target_dual = target_type.dual(&self.type_defs)?;
                let typ = match annotation {
                    Some(annotated_type) => {
                        annotated_type.check_assignable(loc, &target_dual, &self.type_defs)?;
                        annotated_type.clone()
                    }
                    None => target_dual,
                };
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                context.put(loc, channel.clone(), typ.clone())?;
                let process = context.check_process(process)?;
                Ok(Arc::new(Expression::Fork(
                    loc.clone(),
                    captures.clone(),
                    channel.clone(),
                    annotation.clone(),
                    typ,
                    process,
                )))
            }
        }
    }

    pub fn infer_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &Expression<Loc, Name, ()>,
    ) -> Result<(Arc<Expression<Loc, Name, Type<Loc, Name>>>, Type<Loc, Name>), TypeError<Loc, Name>>
    {
        match expression {
            Expression::Reference(loc, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(loc, name)?;
                if !typ.is_linear(&self.type_defs)? {
                    self.put(loc, name.clone(), typ.clone())?;
                }
                Ok((
                    Arc::new(Expression::Reference(
                        loc.clone(),
                        name.clone(),
                        typ.clone(),
                    )),
                    typ.clone(),
                ))
            }

            Expression::Fork(loc, captures, channel, annotation, (), process) => {
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                let (process, typ) = match annotation {
                    Some(typ) => {
                        context.put(loc, channel.clone(), typ.clone())?;
                        (context.check_process(process)?, typ.clone())
                    }
                    None => context.infer_process(process, channel)?,
                };
                let dual = typ.dual(&self.type_defs)?;
                Ok((
                    Arc::new(Expression::Fork(
                        loc.clone(),
                        captures.clone(),
                        channel.clone(),
                        annotation.clone(),
                        typ,
                        process,
                    )),
                    dual,
                ))
            }
        }
    }

    pub fn cannot_have_obligations(&mut self, loc: &Loc) -> Result<(), TypeError<Loc, Name>> {
        if self.obligations().any(|_| true) {
            return Err(TypeError::UnfulfilledObligations(
                loc.clone(),
                self.obligations().cloned().collect(),
            ));
        }
        Ok(())
    }
}

impl<Loc, Name: Display> Type<Loc, Name> {
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

            Self::Send(_, arg, then) => {
                write!(f, "(")?;
                arg.pretty(f, indent)?;
                write!(f, ") ")?;
                then.pretty(f, indent)
            }

            Self::Receive(_, param, then) => {
                write!(f, "[")?;
                param.pretty(f, indent)?;
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

            Self::Recursive(_, asc, label, body) => {
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

            Self::Iterative(_, asc, label, body) => {
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

            Self::SendType(_, name, body) => {
                write!(f, "(type {}) ", name)?;
                body.pretty(f, indent)
            }

            Self::ReceiveType(_, name, body) => {
                write!(f, "[type {}] ", name)?;
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

fn two_labels_from_two_locs(
    code: &str,
    loc1: &Loc,
    loc2: &Loc,
    label1: impl Into<Option<String>>,
    label2: impl Into<Option<String>>,
) -> Vec<LabeledSpan> {
    use crate::playground::labels_from_loc;
    let mut labels = labels_from_loc(code, loc1);
    let label1 = label1.into();
    let label2 = label2.into();
    labels.iter_mut().for_each(|x| x.set_label(label1.clone()));
    let mut labels2 = labels_from_loc(code, loc2);
    labels2.iter_mut().for_each(|x| x.set_label(label2.clone()));
    labels.extend(labels2);
    labels
}

impl<Name: Display> TypeError<super::parse::Loc, Name> {
    pub fn into_report(&self, source_code: Arc<str>) -> miette::Report {
        use crate::playground::labels_from_loc;
        let code = &source_code;
        match self {
            Self::TypeNameAlreadyDefined(loc1, loc2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_locs(code, loc1, loc2, "this".to_owned(), "is already defined here".to_owned()),
                    "Type `{}` is already defined.", name
                )
            }
            Self::NameAlreadyDeclared(loc1, loc2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_locs(code, loc1, loc2, "this".to_owned(), "is already declared here".to_owned()),
                    "`{}` is already declared.",
                    name,
                )
            }
            Self::NameAlreadyDefined(loc1, loc2, name) => {
                miette::miette!(
                    labels = two_labels_from_two_locs(code, loc1, loc2, "this".to_owned(), "is already defined here".to_owned()),
                    "`{}` is already defined",
                    name,
                )
            }
            Self::DeclaredButNotDefined(loc,  name) => {
                let mut labels = labels_from_loc(code, loc);
                labels.iter_mut().for_each(|x| {
                    x.set_label(Some("declared here".to_owned()));
                });
                miette::miette!(
                    labels = labels,
                    "`{}` is declared, but is missing a corresponding definition.",
                    name
                )
            }
            Self::NoMatchingRecursiveOrIterative(loc, _) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "This `self` has no matching `recursive` or `iterative`.",
                )
            }
            Self::SelfUsedInNegativePosition(loc, _) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "This `self` is used in a negative position.\n\nNegative self-references are not allowed."
                )
            }
            Self::TypeNameNotDefined(loc, name) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(labels = labels, "Type `{}` is not defined.", name)
            }
            Self::DependencyCycle(loc, deps) => {
                let labels = labels_from_loc(code, loc);
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
            Self::WrongNumberOfTypeArgs(loc, name, required_number, provided_number) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "Type `{}` has {} type arguments, but {} were provided.",
                    name,
                    required_number,
                    provided_number
                )
            }
            Self::NameNotDefined(loc, name) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(labels = labels, "`{}` is not defined.", name)
            }
            Self::ShadowedObligation(loc, name) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "Cannot re-assign `{}` before handling it.",
                    name,
                )
            }
            Self::TypeMustBeKnownAtThisPoint(loc, _) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(labels = labels, "Type must be known at this point.")
            }
            Self::ParameterTypeMustBeKnown(loc, _, param) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "Type of parameter `{}` must be known.",
                    param,
                )
            }
            Self::CannotAssignFromTo(loc, from_type, to_type) => {
                let labels = labels_from_loc(code, loc);
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
            Self::UnfulfilledObligations(loc, names) => {
                let labels = labels_from_loc(code, loc);
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
            Self::InvalidOperation(loc, _, typ) => {
                let labels = labels_from_loc(code, loc);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "This operation cannot be performed on:\n\n  {}\n",
                    typ_str
                )
            }
            Self::InvalidBranch(loc, branch, typ) => {
                let labels = labels_from_loc(code, loc);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` is not available on:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::MissingBranch(loc, branch, typ) => {
                let labels = labels_from_loc(code, loc);
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                miette::miette!(
                    labels = labels,
                    "Branch `{}` was not handled for:\n\n  {}\n",
                    branch,
                    typ_str
                )
            }
            Self::RedundantBranch(loc, branch, typ) => {
                let labels = labels_from_loc(code, loc);
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
                    labels = two_labels_from_two_locs(
                        code,
                        typ1.get_loc(),
                        typ2.get_loc(),
                        "this".to_owned(),
                        "should operate on the same type as this".to_owned()
                    ),
                    "Operations cannot be performed on the same type."
                )
            }
            Self::NoSuchLoopPoint(loc, _) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(labels = labels, "There is no matching loop point in scope.")
            }
            Self::DoesNotDescendSubjectOfBegin(loc, _) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "This `loop` may diverge. Value does not descend from the corresponding `begin`.\n\nIf this is intended, use `unfounded begin`.",
                )
            }
            Self::LoopVariableNotPreserved(loc, name) => {
                let labels = labels_from_loc(code, loc);
                miette::miette!(
                    labels = labels,
                    "`{}` is used by next iteration, but is no longer defined.",
                    name,
                )
            }
            Self::LoopVariableChangedType(loc, name, loop_type, begin_type) => {
                let labels = labels_from_loc(code, loc);
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
            Self::Telltypes(loc, variables) => {
                let labels = labels_from_loc(code, loc);
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
