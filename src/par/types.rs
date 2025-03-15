use indexmap::{IndexMap, IndexSet};
use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
    hash::Hash,
    sync::Arc,
};

use super::process::{Captures, Command, Expression, Process};

#[derive(Clone, Debug)]
pub enum TypeError<Loc, Name> {
    TypeNameNotDefined(Loc, Name),
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
    LoopVariableNotPreserved(Loc, Name),
    LoopVariableChangedType(Loc, Name, Type<Loc, Name>, Type<Loc, Name>),
    Telltypes(Loc, IndexMap<Name, Type<Loc, Name>>),
}

#[derive(Clone, Debug)]
pub enum Operation<Loc, Name> {
    Send(Loc),
    Receive(Loc),
    Choose(Loc, Name),
    Match(Loc, Arc<[Name]>),
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
    Recursive(Loc, Option<Name>, Box<Self>),
    Iterative(Loc, Option<Name>, Box<Self>),
    Self_(Loc, Option<Name>),
    SendType(Loc, Name, Box<Self>),
    ReceiveType(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct TypeDefs<Loc, Name> {
    pub globals: Arc<IndexMap<Name, (Vec<Name>, Type<Loc, Name>)>>,
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
            Some((params, typ)) => {
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
            Some((params, typ)) => {
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
}

#[derive(Clone, Debug)]
pub struct Declarations<Loc, Name>(pub IndexMap<Name, Option<Type<Loc, Name>>>);

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
            Self::Recursive(loc, _, _) => loc,
            Self::Iterative(loc, _, _) => loc,
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
            Self::Recursive(loc, label, body) => {
                Type::Recursive(loc, map_label(label, f), Box::new(body.map_names(f)))
            }
            Self::Iterative(loc, label, body) => {
                Type::Iterative(loc, map_label(label, f), Box::new(body.map_names(f)))
            }
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

            Self::Recursive(loc, label, body) => {
                Self::Recursive(loc, label, Box::new(body.substitute(var, typ)?))
            }
            Self::Iterative(loc, label, body) => {
                Self::Iterative(loc, label, Box::new(body.substitute(var, typ)?))
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

            (Self::Recursive(_, label1, body1), Self::Recursive(_, label2, body2)) => {
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (typ, Self::Recursive(_, label, body)) => typ.is_assignable_to(
                &Self::expand_recursive(label, body, type_defs)?,
                type_defs,
                ind,
            )?,
            (Self::Iterative(_, label1, body1), Self::Iterative(_, label2, body2)) => {
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (Self::Iterative(_, label, body), typ) => {
                Self::expand_iterative(label, body, type_defs)?
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

            Self::Recursive(loc, label, t) => Self::Iterative(
                loc.clone(),
                label.clone(),
                Box::new(t.dual(type_defs)?.chan_self(label)),
            ),
            Self::Iterative(loc, label, t) => Self::Recursive(
                loc.clone(),
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

            Self::Recursive(loc, label1, t) => {
                if &label1 == label {
                    Self::Recursive(loc, label1, t)
                } else {
                    Self::Recursive(loc, label1, Box::new(t.chan_self(label)))
                }
            }
            Self::Iterative(loc, label1, t) => {
                if &label1 == label {
                    Self::Iterative(loc, label1, t)
                } else {
                    Self::Iterative(loc, label1, Box::new(t.chan_self(label)))
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
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        body.clone().expand_recursive_helper(label, body, type_defs)
    }

    fn expand_recursive_helper(
        self,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(loc, t) => match *t {
                Self::Self_(loc, label) if &label == top_label => Self::Iterative(
                    loc,
                    label.clone(),
                    Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                ),
                t => Self::Chan(
                    loc,
                    Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(loc, name) => Self::Var(loc, name),
            Self::Name(loc, name, args) => Self::Name(
                loc,
                name,
                args.into_iter()
                    .map(|arg| Ok(arg.expand_recursive_helper(top_label, top_body, type_defs)?))
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_label, top_body, type_defs)?),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_label, top_body, type_defs)?),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_recursive_helper(top_label, top_body, type_defs)?,
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
                            typ.expand_recursive_helper(top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Continue(loc),

            Self::Recursive(loc, label, t) => {
                if &label == top_label {
                    Self::Recursive(loc, label, t)
                } else {
                    Self::Recursive(
                        loc,
                        label,
                        Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
                    )
                }
            }
            Self::Iterative(loc, label, t) => Self::Iterative(
                loc,
                label,
                Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
            ),
            Self::Self_(loc, label) => {
                if &label == top_label {
                    Self::Recursive(loc, label, Box::new(top_body.clone()))
                } else {
                    Self::Self_(loc, label)
                }
            }

            Self::SendType(loc, name, t) => Self::SendType(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
            ),
            Self::ReceiveType(loc, name, t) => Self::ReceiveType(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn expand_iterative(
        label: &Option<Name>,
        body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        body.clone().expand_iterative_helper(label, body, type_defs)
    }

    fn expand_iterative_helper(
        self,
        top_label: &Option<Name>,
        top_body: &Self,
        type_defs: &TypeDefs<Loc, Name>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match self {
            Self::Chan(loc, t) => match *t {
                Self::Self_(loc, label) if &label == top_label => Self::Recursive(
                    loc,
                    label.clone(),
                    Box::new(top_body.dual(type_defs)?.chan_self(&label)),
                ),
                t => Self::Chan(
                    loc,
                    Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
                ),
            },

            Self::Var(loc, name) => Self::Var(loc, name),
            Self::Name(loc, name, args) => Self::Name(
                loc,
                name,
                args.into_iter()
                    .map(|arg| Ok(arg.expand_iterative_helper(top_label, top_body, type_defs)?))
                    .collect::<Result<_, _>>()?,
            ),

            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_label, top_body, type_defs)?),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_label, top_body, type_defs)?),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| {
                        Ok((
                            branch,
                            typ.expand_iterative_helper(top_label, top_body, type_defs)?,
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
                            typ.expand_iterative_helper(top_label, top_body, type_defs)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Continue(loc),

            Self::Recursive(loc, label, t) => Self::Recursive(
                loc,
                label,
                Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
            ),
            Self::Iterative(loc, label, t) => {
                if &label == top_label {
                    Self::Iterative(loc, label, t)
                } else {
                    Self::Iterative(
                        loc,
                        label,
                        Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
                    )
                }
            }
            Self::Self_(loc, label) => {
                if &label == top_label {
                    Self::Iterative(loc, label, Box::new(top_body.clone()))
                } else {
                    Self::Self_(loc, label)
                }
            }

            Self::SendType(loc, name, t) => Self::SendType(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
            ),
            Self::ReceiveType(loc, name, t) => Self::ReceiveType(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_label, top_body, type_defs)?),
            ),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Context<Loc, Name> {
    type_defs: TypeDefs<Loc, Name>,
    declarations: Declarations<Loc, Name>,
    variables: IndexMap<Name, Type<Loc, Name>>,
    loop_points: IndexMap<Option<Name>, (Name, Arc<IndexMap<Name, Type<Loc, Name>>>)>,
}

impl<Loc, Name> Context<Loc, Name>
where
    Loc: std::fmt::Debug + Clone + Eq + Hash,
    Name: Clone + Eq + Hash + std::fmt::Debug,
{
    pub fn new(
        globals_type_defs: Arc<IndexMap<Name, (Vec<Name>, Type<Loc, Name>)>>,
        declarations: Declarations<Loc, Name>,
    ) -> Self {
        Self {
            type_defs: TypeDefs {
                globals: globals_type_defs,
                vars: IndexSet::new(),
            },
            declarations,
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
        }
    }

    pub fn split(&self) -> Self {
        Self {
            type_defs: self.type_defs.clone(),
            declarations: self.declarations.clone(),
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
            None => match self.declarations.0.get(name) {
                Some(Some(typ)) => Ok(typ.clone()),
                Some(None) => Err(TypeError::TypeMustBeKnownAtThisPoint(
                    loc.clone(),
                    name.clone(),
                )),
                None => Err(TypeError::NameNotDefined(loc.clone(), name.clone())),
            },
        }
    }

    pub fn put(
        &mut self,
        loc: &Loc,
        name: Name,
        typ: Type<Loc, Name>,
    ) -> Result<(), TypeError<Loc, Name>> {
        if let Some(_) = self.variables.get(&name) {
            return Err(TypeError::ShadowedObligation(loc.clone(), name));
        }
        self.variables.insert(name, typ);
        Ok(())
    }

    pub fn add_declaration(&mut self, name: Name, typ: Type<Loc, Name>) {
        self.declarations.0.insert(name, Some(typ));
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
            let value = match self.get_variable(name) {
                Some(value) => value,
                None => continue,
            };
            target.put(loc, name.clone(), value)?;
        }
        Ok(())
    }

    pub fn obligations(&self) -> impl Iterator<Item = &Name> {
        self.variables.iter().map(|(name, _)| name)
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
            if let Type::Iterative(_, top_label, body) = typ {
                return self.check_command(
                    inference_subject,
                    loc,
                    object,
                    &Type::expand_iterative(top_label, body, &self.type_defs)?,
                    command,
                    analyze_process,
                );
            }
        }
        if !matches!(command, Command::Begin(_, _) | Command::Loop(_)) {
            if let Type::Recursive(_, top_label, body) = typ {
                return self.check_command(
                    inference_subject,
                    loc,
                    object,
                    &Type::expand_recursive(top_label, body, &self.type_defs)?,
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

            Command::Begin(label, process) => {
                let Type::Recursive(typ_loc, typ_label, typ_body) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Begin(loc.clone(), label.clone()),
                        typ.clone(),
                    ));
                };

                self.loop_points.insert(
                    label.clone(),
                    (
                        object.clone(),
                        Arc::new({
                            let mut variables = self.variables.clone();
                            variables.insert(
                                object.clone(),
                                Type::Recursive(
                                    typ_loc.clone(),
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
                    Type::expand_recursive(typ_label, typ_body, &self.type_defs)?,
                )?;
                let (process, inferred_type) = analyze_process(self, process)?;

                let inferred_iterative = inferred_type
                    .map(|body| Type::Iterative(loc.clone(), label.clone(), Box::new(body)));

                (Command::Begin(label.clone(), process), inferred_iterative)
            }

            Command::Loop(label) => {
                let Type::Recursive(_, typ_label, typ_body) = typ else {
                    return Err(TypeError::InvalidOperation(
                        loc.clone(),
                        Operation::Loop(loc.clone(), label.clone()),
                        typ.clone(),
                    ));
                };
                let expanded = Type::expand_recursive(typ_label, typ_body, &self.type_defs)?;

                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(loc.clone(), label.clone()));
                };
                self.put(loc, driver.clone(), expanded)?;

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
                    Command::Loop(label.clone()),
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

            Command::Begin(label, process) => {
                self.loop_points.insert(
                    label.clone(),
                    (subject.clone(), Arc::new(self.variables.clone())),
                );
                let (process, body) = self.infer_process(process, subject)?;
                (
                    Command::Begin(label.clone(), process),
                    Type::Recursive(loc.clone(), label.clone(), Box::new(body)),
                )
            }

            Command::Loop(label) => {
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
                    Command::Loop(label.clone()),
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
                    for arg in args {
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

            Self::Recursive(_, label, body) => {
                write!(f, "recursive ")?;
                if let Some(label) = label {
                    write!(f, ":{} ", label)?;
                }
                body.pretty(f, indent)
            }

            Self::Iterative(_, label, body) => {
                write!(f, "iterative ")?;
                if let Some(label) = label {
                    write!(f, ":{} ", label)?;
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

impl<Loc, Name: Display> TypeError<Loc, Name> {
    pub fn pretty(&self, display_loc: impl Fn(&Loc) -> String) -> String {
        match self {
            Self::TypeNameNotDefined(loc, name) => {
                format!("{}\nType `{}` is not defined.", display_loc(loc), name)
            }
            Self::WrongNumberOfTypeArgs(loc, name, required_number, provided_number) => {
                format!(
                    "{}\nType `{}` has {} type arguments, but {} were provided.",
                    display_loc(loc),
                    name,
                    required_number,
                    provided_number
                )
            }
            Self::NameNotDefined(loc, name) => {
                format!("{}\n`{}` is not defined.", display_loc(loc), name)
            }
            Self::ShadowedObligation(loc, name) => {
                format!(
                    "{}\nCannot re-assign `{}` before handling it.",
                    display_loc(loc),
                    name,
                )
            }
            Self::TypeMustBeKnownAtThisPoint(loc, _) => {
                format!("{}\nType must be known at this point.", display_loc(loc))
            }
            Self::ParameterTypeMustBeKnown(loc, _, param) => {
                format!(
                    "{}\nType of parameter `{}` must be known.",
                    display_loc(loc),
                    param,
                )
            }
            Self::CannotAssignFromTo(loc, from_type, to_type) => {
                let (mut from_type_str, mut to_type_str) = (String::new(), String::new());
                from_type.pretty(&mut from_type_str, 1).unwrap();
                to_type.pretty(&mut to_type_str, 1).unwrap();
                format!(
                    "{}\nThis type was required:\n\n  {}\n\nBut an incompatible type was provided:\n\n  {}",
                    display_loc(loc),
                    to_type_str,
                    from_type_str,
                )
            }
            Self::UnfulfilledObligations(loc, names) => {
                format!(
                    "{}\nCannot end this process before handling {}.",
                    display_loc(loc),
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
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                format!(
                    "{}\nThis operation cannot be performed on:\n\n  {}",
                    display_loc(loc),
                    typ_str,
                )
            }
            Self::InvalidBranch(loc, branch, typ) => {
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                format!(
                    "{}\nBranch `{}` is not available on:\n\n  {}",
                    display_loc(loc),
                    branch,
                    typ_str,
                )
            }
            Self::MissingBranch(loc, branch, typ) => {
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                format!(
                    "{}\nBranch `{}` was not handled for:\n\n  {}",
                    display_loc(loc),
                    branch,
                    typ_str,
                )
            }
            Self::RedundantBranch(loc, branch, typ) => {
                let mut typ_str = String::new();
                typ.pretty(&mut typ_str, 1).unwrap();
                format!(
                    "{}\nBranch `{}` is not possible for:\n\n  {}",
                    display_loc(loc),
                    branch,
                    typ_str,
                )
            }
            Self::TypesCannotBeUnified(typ1, typ2) => {
                format!(
                    "{}\nThis is supposed to operate on the same type as this:\n\n{}\nBut these operations cannot be performed on the same type.",
                    display_loc(typ1.get_loc()),
                    display_loc(typ2.get_loc()),
                )
            }
            Self::NoSuchLoopPoint(loc, _) => {
                format!(
                    "{}\nThere is no matching loop point in scope.",
                    display_loc(loc),
                )
            }
            Self::LoopVariableNotPreserved(loc, name) => {
                format!(
                    "{}\n`{}` is used by next iteration, but is no longer defined.",
                    display_loc(loc),
                    name,
                )
            }
            Self::LoopVariableChangedType(loc, name, loop_type, begin_type) => {
                let (mut loop_type_str, mut begin_type_str) = (String::new(), String::new());
                loop_type.pretty(&mut loop_type_str, 1).unwrap();
                begin_type.pretty(&mut begin_type_str, 1).unwrap();
                format!(
                    "{}\nFor next iteration, `{}` is required to be:\n\n  {}\n\nBut it has an incompatible type:\n\n  {}",
                    display_loc(loc),
                    name,
                    begin_type_str,
                    loop_type_str,
                )
            }
            Self::Telltypes(loc, variables) => {
                let mut buf = String::new();
                write!(&mut buf, "{}\n", display_loc(loc)).unwrap();
                for (name, typ) in variables {
                    write!(&mut buf, "{}: ", name).unwrap();
                    typ.pretty(&mut buf, 0).unwrap();
                    write!(&mut buf, "\n\n").unwrap();
                }
                buf
            }
        }
    }
}
