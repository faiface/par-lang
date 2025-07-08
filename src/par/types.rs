use arcstr::ArcStr;
use indexmap::{IndexMap, IndexSet};
use std::{
    collections::{BTreeMap, HashSet},
    fmt::{self, Write},
    sync::{Arc, RwLock},
};

use super::{
    language::{GlobalName, LocalName},
    process::{Captures, Command, Expression, Process},
};
use crate::location::{Span, Spanning};
use miette::LabeledSpan;

#[derive(Clone, Debug)]
pub enum TypeError {
    TypeNameAlreadyDefined(Span, Span, GlobalName),
    NameAlreadyDeclared(Span, Span, GlobalName),
    NameAlreadyDefined(Span, Span, GlobalName),
    DeclaredButNotDefined(Span, GlobalName),
    NoMatchingRecursiveOrIterative(Span),
    SelfUsedInNegativePosition(Span),
    TypeNameNotDefined(Span, GlobalName),
    TypeVariableNotDefined(Span, LocalName),
    DependencyCycle(Span, Vec<GlobalName>),
    WrongNumberOfTypeArgs(Span, GlobalName, usize, usize),
    GlobalNameNotDefined(Span, GlobalName),
    VariableDoesNotExist(Span, LocalName),
    ShadowedObligation(Span, LocalName),
    TypeMustBeKnownAtThisPoint(Span, #[allow(unused)] LocalName),
    ParameterTypeMustBeKnown(Span, LocalName),
    CannotAssignFromTo(Span, Type, Type),
    UnfulfilledObligations(Span, Vec<LocalName>),
    InvalidOperation(Span, #[allow(unused)] Operation, Type),
    InvalidBranch(Span, LocalName, Type),
    MissingBranch(Span, LocalName, Type),
    RedundantBranch(Span, LocalName, Type),
    TypesCannotBeUnified(Type, Type),
    NoSuchLoopPoint(Span, #[allow(unused)] Option<LocalName>),
    DoesNotDescendSubjectOfBegin(Span, #[allow(unused)] Option<LocalName>),
    LoopVariableNotPreserved(Span, LocalName),
    LoopVariableChangedType(Span, LocalName, Type, Type),
    CannotUseLinearVariableInBox(Span, LocalName),
    Telltypes(Span, IndexMap<LocalName, Type>),
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Send,
    Receive,
    Signal,
    Case,
    Break,
    Continue,
    Begin,
    Loop,
    SendType,
    ReceiveType,
}

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(Span, PrimitiveType),
    DualPrimitive(Span, PrimitiveType),
    Var(Span, LocalName),
    DualVar(Span, LocalName),
    Name(Span, GlobalName, Vec<Self>),
    DualName(Span, GlobalName, Vec<Self>),
    Box(Span, Box<Self>),
    DualBox(Span, Box<Self>),
    Pair(Span, Box<Self>, Box<Self>),
    Function(Span, Box<Self>, Box<Self>),
    Either(Span, BTreeMap<LocalName, Self>),
    Choice(Span, BTreeMap<LocalName, Self>),
    Break(Span),
    Continue(Span),
    Recursive {
        span: Span,
        // The ascendents of the type (denoted by the names of the respective loop points):
        // If you `begin` on a `recursive`, and it expands, so its `self`s get replaced by new
        // `recursive`s, these new `recursive`s will have as their *ascendent* the original `recursive`.
        // This is for totality checking.
        asc: IndexSet<Option<LocalName>>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Iterative {
        span: Span,
        asc: IndexSet<Option<LocalName>>,
        label: Option<LocalName>,
        body: Box<Self>,
    },
    Self_(Span, Option<LocalName>),
    DualSelf(Span, Option<LocalName>),
    Exists(Span, LocalName, Box<Self>),
    Forall(Span, LocalName, Box<Self>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Nat,
    Int,
    String,
    Char,
}

#[allow(unused)]
impl Type {
    pub fn nat() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Nat)
    }

    pub fn int() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Int)
    }

    pub fn string() -> Self {
        Self::Primitive(Span::None, PrimitiveType::String)
    }

    pub fn char() -> Self {
        Self::Primitive(Span::None, PrimitiveType::Char)
    }

    pub fn name(module: Option<&'static str>, primary: &'static str, args: Vec<Self>) -> Self {
        Self::Name(Span::None, GlobalName::external(module, primary), args)
    }

    pub fn var(name: &'static str) -> Self {
        Self::Var(
            Span::None,
            LocalName {
                span: Span::None,
                string: ArcStr::from(name),
            },
        )
    }

    pub fn box_(t: Self) -> Self {
        Self::Box(Span::None, Box::new(t))
    }

    pub fn pair(t: Self, u: Self) -> Self {
        Self::Pair(Span::None, Box::new(t), Box::new(u))
    }

    pub fn function(t: Self, u: Self) -> Self {
        Self::Function(Span::None, Box::new(t), Box::new(u))
    }

    pub fn either(branches: Vec<(&'static str, Self)>) -> Self {
        Self::Either(
            Span::None,
            branches
                .into_iter()
                .map(|(name, typ)| {
                    (
                        LocalName {
                            span: Span::None,
                            string: ArcStr::from(name),
                        },
                        typ,
                    )
                })
                .collect(),
        )
    }

    pub fn choice(branches: Vec<(&'static str, Self)>) -> Self {
        Self::Choice(
            Span::None,
            branches
                .into_iter()
                .map(|(name, typ)| {
                    (
                        LocalName {
                            span: Span::None,
                            string: ArcStr::from(name),
                        },
                        typ,
                    )
                })
                .collect(),
        )
    }

    pub fn break_() -> Self {
        Self::Break(Span::None)
    }

    pub fn continue_() -> Self {
        Self::Continue(Span::None)
    }

    pub fn recursive(label: Option<&'static str>, body: Self) -> Self {
        Self::Recursive {
            span: Span::None,
            asc: IndexSet::new(),
            label: label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
            body: Box::new(body),
        }
    }

    pub fn iterative(label: Option<&'static str>, body: Self) -> Self {
        Self::Iterative {
            span: Span::None,
            asc: IndexSet::new(),
            label: label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
            body: Box::new(body),
        }
    }

    pub fn self_(label: Option<&'static str>) -> Self {
        Self::Self_(
            Span::None,
            label.map(|label| LocalName {
                span: Span::None,
                string: ArcStr::from(label),
            }),
        )
    }
}

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
            type_defs.validate_type(typ, &IndexSet::new(), &IndexSet::new())?;
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

    fn validate_acyclic(
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

    fn validate_type(
        &self,
        typ: &Type,
        self_pos: &IndexSet<Option<LocalName>>,
        self_neg: &IndexSet<Option<LocalName>>,
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
                    self.validate_type(arg, self_pos, self_neg)?;
                }
                let t = self.get(span, name, args)?;
                self.validate_type(&t, self_pos, self_neg)?;
            }
            Type::DualName(span, name, args) => {
                for arg in args {
                    self.validate_type(arg, self_neg, self_pos)?;
                }
                let t = self.get(span, name, args)?;
                self.validate_type(&t, self_neg, self_pos)?;
            }

            Type::Box(_, body) => self.validate_type(body, self_pos, self_neg)?,
            Type::DualBox(_, body) => self.validate_type(body, self_neg, self_pos)?,

            Type::Pair(_, t, u) => {
                self.validate_type(t, self_pos, self_neg)?;
                self.validate_type(u, self_pos, self_neg)?;
            }
            Type::Function(_, t, u) => {
                self.validate_type(t, self_neg, self_pos)?;
                self.validate_type(u, self_pos, self_neg)?;
            }
            Type::Either(_, branches) | Type::Choice(_, branches) => {
                for (_, t) in branches {
                    self.validate_type(t, self_pos, self_neg)?;
                }
            }
            Type::Break(_) | Type::Continue(_) => (),

            Type::Recursive { label, body, .. } | Type::Iterative { label, body, .. } => {
                let (mut self_pos, mut self_neg) = (self_pos.clone(), self_neg.clone());
                self_pos.insert(label.clone());
                self_neg.shift_remove(label);
                self.validate_type(body, &self_pos, &self_neg)?;
            }
            Type::Self_(span, label) => {
                if self_neg.contains(label) {
                    return Err(TypeError::SelfUsedInNegativePosition(span.clone()));
                }
                if !self_pos.contains(label) {
                    return Err(TypeError::NoMatchingRecursiveOrIterative(span.clone()));
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
                with_var.validate_type(body, self_pos, self_neg)?;
            }
        })
    }
}

impl Spanning for Type {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(span, _)
            | Self::DualPrimitive(span, _)
            | Self::Var(span, _)
            | Self::DualVar(span, _)
            | Self::Name(span, _, _)
            | Self::DualName(span, _, _)
            | Self::Box(span, _)
            | Self::DualBox(span, _)
            | Self::Pair(span, _, _)
            | Self::Function(span, _, _)
            | Self::Either(span, _)
            | Self::Choice(span, _)
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Recursive { span, .. }
            | Self::Iterative { span, .. }
            | Self::Self_(span, _)
            | Self::DualSelf(span, _)
            | Self::Exists(span, _, _)
            | Self::Forall(span, _, _) => span.clone(),
        }
    }
}

impl Type {
    pub fn substitute(self, map: BTreeMap<&LocalName, &Type>) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),
            Self::Var(span, name) => {
                if let Some(&typ) = map.get(&name) {
                    typ.clone()
                } else {
                    Self::Var(span, name)
                }
            }
            Self::DualVar(span, name) => {
                if let Some(&typ) = map.get(&name) {
                    typ.clone().dual(Span::None)
                } else {
                    Self::DualVar(span, name)
                }
            }
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(map.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.substitute(map.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Box(span, body) => Self::Box(span, Box::new(body.substitute(map)?)),
            Self::DualBox(span, body) => Self::DualBox(span, Box::new(body.substitute(map)?)),
            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.substitute(map.clone())?),
                Box::new(u.substitute(map)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.substitute(map.clone())?),
                Box::new(u.substitute(map)?),
            ),
            Self::Either(span, branches) => Self::Either(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(map.clone())?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Choice(span, branches) => Self::Choice(
                span,
                branches
                    .into_iter()
                    .map(|(branch, branch_type)| Ok((branch, branch_type.substitute(map.clone())?)))
                    .collect::<Result<_, _>>()?,
            ),
            Self::Break(span) => Self::Break(span),
            Self::Continue(span) => Self::Continue(span),

            Self::Recursive {
                span,
                asc,
                label,
                body,
            } => Self::Recursive {
                span,
                asc,
                label,
                body: Box::new(body.substitute(map)?),
            },
            Self::Iterative {
                span,
                asc,
                label,
                body,
            } => Self::Iterative {
                span,
                asc,
                label,
                body: Box::new(body.substitute(map)?),
            },
            Self::Self_(span, label) => Self::Self_(span, label),
            Self::DualSelf(span, label) => Self::DualSelf(span, label),

            Self::Exists(span, mut name, mut body) => {
                while map.values().any(|t| t.contains_var(&name)) {
                    let old_name = name.clone();
                    name.string = arcstr::format!("{}'", name.string);
                    body = Box::new(body.substitute(BTreeMap::from([(
                        &old_name,
                        &Type::Var(name.span(), name.clone()),
                    )]))?);
                }
                let mut map = map;
                map.remove(&name);
                Self::Exists(span, name, Box::new(body.substitute(map)?))
            }
            Self::Forall(span, mut name, mut body) => {
                while map.values().any(|t| t.contains_var(&name)) {
                    let old_name = name.clone();
                    name.string = arcstr::format!("{}'", name.string);
                    body = Box::new(body.substitute(BTreeMap::from([(
                        &old_name,
                        &Type::Var(name.span(), name.clone()),
                    )]))?);
                }
                let mut map = map;
                map.remove(&name);
                Self::Forall(span, name, Box::new(body.substitute(map)?))
            }
        })
    }

    pub fn is_linear(&self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        Ok(!self.is_positive(type_defs)?)
    }

    pub fn is_positive(&self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        Ok(match self {
            Self::Primitive(_, _) => true,
            Self::DualPrimitive(_, _) => false,
            Self::Var(_, _) => false,
            Self::DualVar(_, _) => false,
            Self::Name(loc, name, args) => {
                type_defs.get(loc, name, args)?.is_positive(type_defs)?
            }
            Self::DualName(loc, name, args) => type_defs
                .get_dual(loc, name, args)?
                .is_positive(type_defs)?,
            Self::Box(_, _) => true, //TODO: distinguish between box and data types
            Self::DualBox(_, _) => false,
            Self::Pair(_, t, u) => t.is_positive(type_defs)? && u.is_positive(type_defs)?,
            Self::Function(_, _, _) => false,
            Self::Either(_, branches) => {
                for (_, t) in branches {
                    if !t.is_positive(type_defs)? {
                        return Ok(false);
                    }
                }
                true
            }
            Self::Choice(_, _) => false,
            Self::Break(_) => true,
            Self::Continue(_) => false,
            Self::Recursive { body, .. } => body.is_positive(type_defs)?,
            Self::Iterative { body, .. } => body.is_positive(type_defs)?,
            Self::Self_(_, _) => true,
            Self::DualSelf(_, _) => true,
            Self::Exists(loc, name, t) => t
                .clone()
                .substitute(BTreeMap::from([(
                    name,
                    &Type::Var(loc.clone(), name.clone()),
                )]))?
                .is_positive(type_defs)?,
            Self::Forall(loc, name, t) => t
                .clone()
                .substitute(BTreeMap::from([(
                    name,
                    &Type::Var(loc.clone(), name.clone()),
                )]))?
                .is_positive(type_defs)?,
        })
    }

    pub fn check_assignable(
        &self,
        span: &Span,
        u: &Type,
        type_defs: &TypeDefs,
    ) -> Result<(), TypeError> {
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
        type_defs: &TypeDefs,
        ind: &HashSet<(Option<LocalName>, Option<LocalName>)>,
    ) -> Result<bool, TypeError> {
        Ok(match (self, other) {
            (Self::Primitive(_, PrimitiveType::Nat), Self::Primitive(_, PrimitiveType::Int)) => {
                true
            }
            (Self::Primitive(_, p1), Self::Primitive(_, p2)) => p1 == p2,
            (
                Self::DualPrimitive(_, PrimitiveType::Int),
                Self::DualPrimitive(_, PrimitiveType::Nat),
            ) => true,
            (Self::DualPrimitive(_, p1), Self::DualPrimitive(_, p2)) => p1 == p2,

            (Self::Var(_, name1), Self::Var(_, name2)) => name1 == name2,
            (Self::DualVar(_, name1), Self::DualVar(_, name2)) => name1 == name2,
            (Self::Name(span, name, args), t2) => type_defs
                .get(span, name, args)?
                .is_assignable_to(t2, type_defs, ind)?,
            (t1, Self::Name(span, name, args)) => {
                t1.is_assignable_to(&type_defs.get(span, name, args)?, type_defs, ind)?
            }
            (Self::DualName(span, name, args), t2) => type_defs
                .get_dual(span, name, args)?
                .is_assignable_to(t2, type_defs, ind)?,
            (t1, Self::DualName(span, name, args)) => {
                t1.is_assignable_to(&type_defs.get_dual(span, name, args)?, type_defs, ind)?
            }

            (t1, Self::Box(_, t2)) if t1.is_positive(type_defs)? => {
                t1.is_assignable_to(t2, type_defs, ind)?
            }
            (Self::DualBox(_, t1), t2) if t1.is_positive(type_defs)? => t1
                .clone()
                .dual(Span::None)
                .is_assignable_to(t2, type_defs, ind)?,
            (Self::Box(_, t1), Self::Box(_, t2)) => t1.is_assignable_to(t2, type_defs, ind)?,
            (Self::Box(_, t1), t2) => t1.is_assignable_to(t2, type_defs, ind)?,
            (Self::DualBox(_, t1), Self::DualBox(_, t2)) => {
                t2.is_assignable_to(t1, type_defs, ind)?
            }
            (t1, Self::DualBox(_, t2)) => t2.is_assignable_to(t1, type_defs, ind)?,

            (Self::Pair(_, t1, u1), Self::Pair(_, t2, u2)) => {
                t1.is_assignable_to(t2, type_defs, ind)?
                    && u1.is_assignable_to(u2, type_defs, ind)?
            }
            (Self::Function(_, t1, u1), Self::Function(_, t2, u2)) => {
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
                true
            }
            (Self::Choice(_, branches1), Self::Choice(_, branches2)) => {
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

            (
                Self::Recursive {
                    asc: asc1,
                    label: label1,
                    body: body1,
                    ..
                },
                Self::Recursive {
                    asc: asc2,
                    label: label2,
                    body: body2,
                    ..
                },
            ) => {
                if !asc2.iter().all(|label| asc1.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (
                typ,
                Self::Recursive {
                    asc, label, body, ..
                },
            ) => typ.is_assignable_to(
                &Self::expand_recursive(asc, label, body, type_defs)?,
                type_defs,
                ind,
            )?,
            (
                Self::Iterative {
                    asc: asc1,
                    label: label1,
                    body: body1,
                    ..
                },
                Self::Iterative {
                    asc: asc2,
                    label: label2,
                    body: body2,
                    ..
                },
            ) => {
                if !asc1.iter().all(|label| asc2.contains(label)) {
                    return Ok(false);
                }
                let mut ind = ind.clone();
                ind.insert((label1.clone(), label2.clone()));
                body1.is_assignable_to(body2, type_defs, &ind)?
            }
            (
                Self::Iterative {
                    asc, label, body, ..
                },
                typ,
            ) => Self::expand_iterative(asc, label, body, type_defs)?
                .is_assignable_to(typ, type_defs, ind)?,

            (Self::Self_(_, label1), Self::Self_(_, label2)) => {
                ind.contains(&(label1.clone(), label2.clone()))
            }
            (Self::DualSelf(_, label1), Self::DualSelf(_, label2)) => {
                ind.contains(&(label2.clone(), label1.clone()))
            }

            (Self::Exists(loc, name1, body1), Self::Exists(_, name2, body2))
            | (Self::Forall(loc, name1, body1), Self::Forall(_, name2, body2)) => {
                let body2 = body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(loc.clone(), name1.clone()),
                )]))?;
                let mut type_defs = type_defs.clone();
                type_defs.vars.insert(name1.clone());
                body1.is_assignable_to(&body2, &type_defs, ind)?
            }

            _ => false,
        })
    }

    pub fn dual(self, span0: Span) -> Self {
        match self {
            Self::Primitive(span, p) => Self::DualPrimitive(span0.join(span), p),
            Self::DualPrimitive(span, p) => Self::Primitive(span0.join(span), p),

            Self::Var(span, name) => Self::DualVar(span0.join(span), name),
            Self::DualVar(span, name) => Self::Var(span0.join(span), name),

            Self::Name(span, name, args) => Self::DualName(span0.join(span), name, args),
            Self::DualName(span, name, args) => Self::Name(span0.join(span), name, args),

            Self::Box(span, body) => Self::DualBox(span, body),
            Self::DualBox(span, body) => Self::Box(span, body),

            Self::Pair(span, t, u) => {
                Self::Function(span0.join(span), t, Box::new(u.dual(Span::None)))
            }
            Self::Function(span, t, u) => {
                Self::Pair(span0.join(span), t, Box::new(u.dual(Span::None)))
            }
            Self::Either(span, branches) => Self::Choice(
                span0.join(span),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dual(Span::None)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Self::Either(
                span0.join(span),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch.clone(), t.dual(Span::None)))
                    .collect(),
            ),
            Self::Break(span) => Self::Continue(span0.join(span)),
            Self::Continue(span) => Self::Break(span0.join(span)),

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                let body = Box::new(t.dual(Span::None).dualize_self(&label));
                Self::Iterative {
                    span: span0.join(span),
                    asc,
                    label,
                    body,
                }
            }
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                let body = Box::new(t.dual(Span::None).dualize_self(&label));
                Self::Recursive {
                    span: span0.join(span),
                    asc,
                    label,
                    body,
                }
            }
            Self::Self_(span, label) => Self::DualSelf(span0.join(span), label),
            Self::DualSelf(span, label) => Self::Self_(span0.join(span), label),

            Self::Exists(span, name, t) => {
                Self::Forall(span0.join(span), name, Box::new(t.dual(Span::None)))
            }
            Self::Forall(span, name, t) => {
                Self::Exists(span0.join(span), name, Box::new(t.dual(Span::None)))
            }
        }
    }

    fn dualize_self(self, label: &Option<LocalName>) -> Self {
        match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.dualize_self(label))
                    .collect(),
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| arg.dualize_self(label))
                    .collect(),
            ),

            Self::Box(span, body) => Self::Box(span, Box::new(body.dualize_self(label))),
            Self::DualBox(span, body) => Self::DualBox(span, Box::new(body.dualize_self(label))),

            Self::Pair(loc, t, u) => Self::Pair(
                loc.clone(),
                Box::new(t.dualize_self(label)),
                Box::new(u.dualize_self(label)),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc.clone(),
                Box::new(t.dualize_self(label)),
                Box::new(u.dualize_self(label)),
            ),
            Self::Either(span, branches) => Self::Either(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dualize_self(label)))
                    .collect(),
            ),
            Self::Choice(span, branches) => Self::Choice(
                span.clone(),
                branches
                    .into_iter()
                    .map(|(branch, t)| (branch, t.dualize_self(label)))
                    .collect(),
            ),
            Self::Break(span) => Self::Break(span.clone()),
            Self::Continue(span) => Self::Continue(span.clone()),

            Self::Recursive {
                span,
                asc,
                label: label1,
                body: t,
            } => {
                if &label1 == label {
                    Self::Recursive {
                        span,
                        asc,
                        label: label1,
                        body: t,
                    }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label: label1,
                        body: Box::new(t.dualize_self(label)),
                    }
                }
            }
            Self::Iterative {
                span,
                asc,
                label: label1,
                body: t,
            } => {
                if &label1 == label {
                    Self::Iterative {
                        span,
                        asc,
                        label: label1,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label: label1,
                        body: Box::new(t.dualize_self(label)),
                    }
                }
            }
            Self::Self_(span, label1) => {
                if &label1 == label {
                    Self::DualSelf(span, label1)
                } else {
                    Self::Self_(span, label1)
                }
            }
            Self::DualSelf(span, label1) => {
                if &label1 == label {
                    Self::Self_(span, label1)
                } else {
                    Self::DualSelf(span, label1)
                }
            }

            Self::Exists(loc, name, t) => {
                Self::Exists(loc.clone(), name.clone(), Box::new(t.dualize_self(label)))
            }
            Self::Forall(loc, name, t) => {
                Self::Forall(loc.clone(), name.clone(), Box::new(t.dualize_self(label)))
            }
        }
    }

    pub fn expand_recursive(
        asc: &IndexSet<Option<LocalName>>,
        label: &Option<LocalName>,
        body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        body.clone()
            .expand_recursive_helper(asc, label, body, type_defs)
    }

    fn expand_recursive_helper(
        self,
        top_asc: &IndexSet<Option<LocalName>>,
        top_label: &Option<LocalName>,
        top_body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Box(span, body) => Self::Box(
                span,
                Box::new(body.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::DualBox(span, body) => Self::DualBox(
                span,
                Box::new(body.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),

            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
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

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: t,
                    }
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
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    }
                } else {
                    Self::Self_(span, label)
                }
            }
            Self::DualSelf(span, label) => {
                if &label == top_label {
                    (Self::Recursive {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    })
                    .dual(Span::None)
                } else {
                    Self::DualSelf(span, label)
                }
            }

            Self::Exists(loc, name, t) => Self::Exists(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Forall(loc, name, t) => Self::Forall(
                loc,
                name,
                Box::new(t.expand_recursive_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    pub fn expand_iterative(
        asc: &IndexSet<Option<LocalName>>,
        label: &Option<LocalName>,
        body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        body.clone()
            .expand_iterative_helper(asc, label, body, type_defs)
    }

    fn expand_iterative_helper(
        self,
        top_asc: &IndexSet<Option<LocalName>>,
        top_label: &Option<LocalName>,
        top_body: &Self,
        type_defs: &TypeDefs,
    ) -> Result<Self, TypeError> {
        Ok(match self {
            Self::Primitive(span, p) => Self::Primitive(span, p),
            Self::DualPrimitive(span, p) => Self::DualPrimitive(span, p),

            Self::Var(span, name) => Self::Var(span, name),
            Self::DualVar(span, name) => Self::DualVar(span, name),
            Self::Name(span, name, args) => Self::Name(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),
            Self::DualName(span, name, args) => Self::DualName(
                span,
                name,
                args.into_iter()
                    .map(|arg| {
                        Ok(arg.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?)
                    })
                    .collect::<Result<_, _>>()?,
            ),

            Self::Box(span, body) => Self::Box(
                span,
                Box::new(body.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::DualBox(span, body) => Self::DualBox(
                span,
                Box::new(body.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),

            Self::Pair(loc, t, u) => Self::Pair(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Function(loc, t, u) => Self::Function(
                loc,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
                Box::new(u.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
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

            Self::Recursive {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Recursive {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Iterative {
                span,
                asc,
                label,
                body: t,
            } => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: t,
                    }
                } else {
                    Self::Iterative {
                        span,
                        asc,
                        label,
                        body: Box::new(
                            t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?,
                        ),
                    }
                }
            }
            Self::Self_(span, label) => {
                if &label == top_label {
                    Self::Iterative {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    }
                } else {
                    Self::Self_(span, label)
                }
            }
            Self::DualSelf(span, label) => {
                if &label == top_label {
                    (Self::Iterative {
                        span,
                        asc: top_asc.clone(),
                        label,
                        body: Box::new(top_body.clone()),
                    })
                    .dual(Span::None)
                } else {
                    Self::DualSelf(span, label)
                }
            }

            Self::Exists(loc, name, t) => Self::Exists(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
            Self::Forall(loc, name, t) => Self::Forall(
                loc,
                name,
                Box::new(t.expand_iterative_helper(top_asc, top_label, top_body, type_defs)?),
            ),
        })
    }

    fn invalidate_ascendent(&mut self, label: &Option<LocalName>) {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => {}
            Self::Var(_, _) | Self::DualVar(_, _) => {}
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                for arg in args {
                    arg.invalidate_ascendent(label);
                }
            }
            Self::Box(_, body) | Self::DualBox(_, body) => {
                body.invalidate_ascendent(label);
            }
            Self::Pair(_, t, u) => {
                t.invalidate_ascendent(label);
                u.invalidate_ascendent(label);
            }
            Self::Function(_, t, u) => {
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

            Self::Recursive {
                span: _,
                asc,
                label: _,
                body: t,
            } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Iterative {
                span: _,
                asc,
                label: _,
                body: t,
            } => {
                asc.shift_remove(label);
                t.invalidate_ascendent(label);
            }
            Self::Self_(_, _) | Self::DualSelf(_, _) => {}

            Self::Exists(_, _, t) => {
                t.invalidate_ascendent(label);
            }
            Self::Forall(_, _, t) => {
                t.invalidate_ascendent(label);
            }
        }
    }

    fn contains_self(&self, label: &Option<LocalName>) -> bool {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => false,
            Self::Var(_, _) | Self::DualVar(_, _) => false,
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                args.iter().any(|arg| arg.contains_self(label))
            }

            Self::Box(_, body) => body.contains_self(label),
            Self::DualBox(_, body) => body.contains_self(label),

            Self::Pair(_, t, u) => t.contains_self(label) || u.contains_self(label),
            Self::Function(_, t, u) => t.contains_self(label) || u.contains_self(label),
            Self::Either(_, branches) => branches.iter().any(|(_, typ)| typ.contains_self(label)),
            Self::Choice(_, branches) => branches.iter().any(|(_, typ)| typ.contains_self(label)),
            Self::Break(_) => false,
            Self::Continue(_) => false,

            Self::Recursive {
                label: label1,
                body,
                ..
            } => label1 != label && body.contains_self(label),
            Self::Iterative {
                label: label1,
                body,
                ..
            } => label1 != label && body.contains_self(label),
            Self::Self_(_, label1) | Self::DualSelf(_, label1) => label1 == label,

            Self::Exists(_, _, body) => body.contains_self(label),
            Self::Forall(_, _, body) => body.contains_self(label),
        }
    }

    fn contains_var(&self, var: &LocalName) -> bool {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => false,
            Self::Var(_, name) | Self::DualVar(_, name) => name == var,
            Self::Name(_, _, args) | Self::DualName(_, _, args) => {
                args.iter().any(|arg| arg.contains_var(var))
            }

            Self::Box(_, body) | Self::DualBox(_, body) => body.contains_var(var),

            Self::Pair(_, t, u) => t.contains_var(var) || u.contains_var(var),
            Self::Function(_, t, u) => t.contains_var(var) || u.contains_var(var),
            Self::Either(_, branches) => branches.iter().any(|(_, typ)| typ.contains_var(var)),
            Self::Choice(_, branches) => branches.iter().any(|(_, typ)| typ.contains_var(var)),
            Self::Break(_) => false,
            Self::Continue(_) => false,

            Self::Recursive { body, .. } => body.contains_var(var),
            Self::Iterative { body, .. } => body.contains_var(var),
            Self::Self_(_, _) | Self::DualSelf(_, _) => false,

            Self::Exists(_, name, body) => name != var && body.contains_var(var),
            Self::Forall(_, name, body) => name != var && body.contains_var(var),
        }
    }

    fn get_dependencies(&self) -> Vec<GlobalName> {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => vec![],
            Self::Var(_, _) | Self::DualVar(_, _) => vec![],
            Self::Name(_, name, args) | Self::DualName(_, name, args) => {
                let mut deps = vec![name.clone()];
                for arg in args {
                    deps.extend(arg.get_dependencies());
                }
                deps
            }
            Self::Box(_, body) | Self::DualBox(_, body) => body.get_dependencies(),
            Self::Pair(_, t, u) => {
                let mut deps = t.get_dependencies();
                deps.extend(u.get_dependencies());
                deps
            }
            Self::Function(_, t, u) => {
                let mut deps = t.get_dependencies();
                deps.extend(u.get_dependencies());
                deps
            }
            Self::Either(_, branches) => branches
                .iter()
                .flat_map(|(_, typ)| typ.get_dependencies())
                .collect(),
            Self::Choice(_, branches) => branches
                .iter()
                .flat_map(|(_, typ)| typ.get_dependencies())
                .collect(),
            Self::Break(_) => vec![],
            Self::Continue(_) => vec![],
            Self::Recursive { body, .. } => body.get_dependencies(),
            Self::Iterative { body, .. } => body.get_dependencies(),
            Self::Self_(_, _) | Self::DualSelf(_, _) => vec![],
            Self::Exists(_, _, body) => body.get_dependencies(),
            Self::Forall(_, _, body) => body.get_dependencies(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    type_defs: TypeDefs,
    declarations: Arc<IndexMap<GlobalName, (Span, Type)>>,
    unchecked_definitions: Arc<IndexMap<GlobalName, (Span, Arc<Expression<()>>)>>,
    checked_definitions: Arc<RwLock<IndexMap<GlobalName, CheckedDef>>>,
    current_deps: IndexSet<GlobalName>,
    variables: IndexMap<LocalName, Type>,
    loop_points: IndexMap<Option<LocalName>, (Type, Arc<IndexMap<LocalName, Type>>)>,
}

#[derive(Clone, Debug)]
struct CheckedDef {
    span: Span,
    def: Arc<Expression<Type>>,
    typ: Type,
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

    fn invalidate_ascendent(&mut self, label: &Option<LocalName>) {
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
                    Some(annotated_type) => (
                        self.check_expression(None, expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
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
                    &Type::expand_iterative(top_asc, top_label, body, &self.type_defs)?,
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
                    &Type::expand_recursive(top_asc, top_label, body, &self.type_defs)?,
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
                        (Some(t1), Some(t2))
                            if t2.is_assignable_to(&t1, &self.type_defs, &HashSet::new())? =>
                        {
                            inferred_type = Some(t2)
                        }
                        (Some(t1), Some(t2))
                            if !t1.is_assignable_to(&t2, &self.type_defs, &HashSet::new())? =>
                        {
                            return Err(TypeError::TypesCannotBeUnified(t1, t2))
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
                    typ_asc.insert(label.clone());
                }

                self.invalidate_ascendent(label);
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
                    Type::expand_recursive(&typ_asc, typ_label, typ_body, &self.type_defs)?,
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
                typ: (),
                command,
            } => {
                if object == inference_subject {
                    let (command, typ) = self.infer_command(span, inference_subject, command)?;
                    return Ok((
                        Arc::new(Process::Do {
                            span: span.clone(),
                            name: object.clone(),
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
                    Type::Either(span.clone(), BTreeMap::from([(chosen.clone(), then_type)])),
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

            Expression::Variable(span, name, ()) => {
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
                )))
            }

            Expression::Box(span, captures, expression, ()) => {
                if let Some(inference_subject) = inference_subject {
                    if captures.names.contains_key(inference_subject) {
                        return Err(TypeError::TypeMustBeKnownAtThisPoint(
                            *span,
                            inference_subject.clone(),
                        ));
                    }
                }
                let mut context = self.split();
                self.capture(inference_subject, captures, true, &mut context)?;
                let mut target_inner_type = target_type.clone();
                while let Type::Box(_, inner) =
                    target_inner_type.expand_definition(&self.type_defs)?
                {
                    target_inner_type = *inner;
                }
                let expression =
                    self.check_expression(inference_subject, expression, &target_inner_type)?;
                Ok(Arc::new(Expression::Box(
                    *span,
                    captures.clone(),
                    expression,
                    target_type.clone(),
                )))
            }

            Expression::Fork {
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
                        annotated_type.check_assignable(span, &target_dual, &self.type_defs)?;
                        (annotated_type.clone(), target_type) // or annotated_type.dual() ???
                    }
                    None => (target_dual, target_type),
                };
                let mut context = self.split();
                self.capture(inference_subject, captures, false, &mut context)?;
                context.put(span, channel.clone(), chan_type.clone())?;
                let process = context.check_process(process)?;
                Ok(Arc::new(Expression::Fork {
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

            Expression::Variable(span, name, ()) => {
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
                    )),
                    typ,
                ))
            }

            Expression::Box(span, captures, expression, ()) => {
                if let Some(inference_subject) = inference_subject {
                    if captures.names.contains_key(inference_subject) {
                        return Err(TypeError::TypeMustBeKnownAtThisPoint(
                            *span,
                            inference_subject.clone(),
                        ));
                    }
                }
                let mut context = self.split();
                self.capture(inference_subject, captures, true, &mut context)?;
                let (expression, typ) = self.infer_expression(inference_subject, expression)?;
                let typ = Type::Box(*span, Box::new(typ.clone()));
                Ok((
                    Arc::new(Expression::Box(
                        *span,
                        captures.clone(),
                        expression,
                        typ.clone(),
                    )),
                    typ,
                ))
            }

            Expression::Fork {
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
                        context.put(span, channel.clone(), typ.clone())?;
                        (context.check_process(process)?, typ.clone())
                    }
                    None => context.infer_process(process, channel)?,
                };
                let dual = typ.clone().dual(Span::None);
                Ok((
                    Arc::new(Expression::Fork {
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
                let typ = claimed_type.clone();
                Ok((
                    Arc::new(Expression::External(claimed_type.clone(), *f, typ.clone())),
                    typ,
                ))
            }
        }
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

impl Type {
    pub fn expand_definition(&self, type_defs: &TypeDefs) -> Result<Self, TypeError> {
        match self {
            Self::Name(span, name, args) => type_defs.get(span, name, args),
            Self::DualName(span, name, args) => type_defs.get_dual(span, name, args),
            _ => Ok(self.clone()),
        }
    }

    pub fn qualify(&mut self, module: &str) {
        match self {
            Self::Primitive(span, _) | Self::DualPrimitive(span, _) => *span = Span::None,
            Self::Var(span, _) | Self::DualVar(span, _) => *span = Span::None,
            Self::Name(span, name, args) | Self::DualName(span, name, args) => {
                *span = Span::None;
                name.qualify(module);
                for arg in args {
                    arg.qualify(module);
                }
            }
            Self::Box(span, body) | Self::DualBox(span, body) => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Pair(span, t, u) => {
                *span = Span::None;
                t.qualify(module);
                u.qualify(module);
            }
            Self::Function(span, t, u) => {
                *span = Span::None;
                t.qualify(module);
                u.qualify(module);
            }
            Self::Either(span, branches) => {
                *span = Span::None;
                for (_, typ) in branches {
                    typ.qualify(module);
                }
            }
            Self::Choice(span, branches) => {
                *span = Span::None;
                for (_, typ) in branches {
                    typ.qualify(module);
                }
            }
            Self::Break(span) => *span = Span::None,
            Self::Continue(span) => *span = Span::None,
            Self::Recursive { span, body, .. } => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Iterative { span, body, .. } => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Self_(span, _) | Self::DualSelf(span, _) => *span = Span::None,
            Self::Exists(span, _, body) => {
                *span = Span::None;
                body.qualify(module);
            }
            Self::Forall(span, _, body) => {
                *span = Span::None;
                body.qualify(module);
            }
        }
    }

    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Primitive(_, PrimitiveType::Nat) => write!(f, "Nat"),
            Self::Primitive(_, PrimitiveType::Int) => write!(f, "Int"),
            Self::Primitive(_, PrimitiveType::String) => write!(f, "String"),
            Self::Primitive(_, PrimitiveType::Char) => write!(f, "Char"),

            Self::DualPrimitive(_, PrimitiveType::Nat) => write!(f, "dual Nat"),
            Self::DualPrimitive(_, PrimitiveType::Int) => write!(f, "dual Int"),
            Self::DualPrimitive(_, PrimitiveType::String) => write!(f, "dual String"),
            Self::DualPrimitive(_, PrimitiveType::Char) => write!(f, "dual Char"),

            Self::Var(_, name) => write!(f, "{}", name),
            Self::DualVar(_, name) => write!(f, "dual {}", name),
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
            Self::DualName(_, name, args) => {
                write!(f, "dual {}", name)?;
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

            Self::Box(_, body) => {
                write!(f, "box ")?;
                body.pretty(f, indent)
            }
            Self::DualBox(_, body) => {
                write!(f, "dual box ")?;
                body.pretty(f, indent)
            }

            Self::Pair(_, arg, then) => {
                let mut then = then;
                write!(f, "(")?;
                arg.pretty(f, indent)?;
                while let Self::Pair(_, arg, next) = then.as_ref() {
                    write!(f, ", ")?;
                    arg.pretty(f, indent)?;
                    then = next;
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty(f, indent)
                }
            }

            Self::Function(_, param, then) => {
                let mut then = then;
                write!(f, "[")?;
                param.pretty(f, indent)?;
                while let Self::Function(_, param, next) = then.as_ref() {
                    write!(f, ", ")?;
                    param.pretty(f, indent)?;
                    then = next;
                }
                if let Self::Continue(_) = then.as_ref() {
                    write!(f, "]?")
                } else {
                    write!(f, "] ")?;
                    then.pretty(f, indent)
                }
            }

            Self::Either(_, branches) => {
                write!(f, "either {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} ", branch)?;
                    typ.pretty(f, indent + 1)?;
                    write!(f, ",")?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Choice(_, branches) => {
                write!(f, "choice {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} => ", branch)?;
                    typ.pretty(f, indent + 1)?;
                    write!(f, ",")?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { label, body, .. } => {
                write!(f, "recursive")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                write!(f, " ")?;
                body.pretty(f, indent)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }

            Self::Exists(_, name, then) => {
                let mut then = then;
                write!(f, "(type {name}")?;
                while let Self::Exists(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, ") ")?;
                then.pretty(f, indent)
            }

            Self::Forall(_, name, then) => {
                let mut then = then;
                write!(f, "[type {name}")?;
                while let Self::Forall(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, "] ")?;
                then.pretty(f, indent)
            }
        }
    }

    pub fn pretty_compact(&self, f: &mut impl Write) -> fmt::Result {
        match self {
            Self::Primitive(_, PrimitiveType::Nat) => write!(f, "Nat"),
            Self::Primitive(_, PrimitiveType::Int) => write!(f, "Int"),
            Self::Primitive(_, PrimitiveType::String) => write!(f, "String"),
            Self::Primitive(_, PrimitiveType::Char) => write!(f, "Char"),

            Self::DualPrimitive(_, PrimitiveType::Nat) => write!(f, "dual Nat"),
            Self::DualPrimitive(_, PrimitiveType::Int) => write!(f, "dual Int"),
            Self::DualPrimitive(_, PrimitiveType::String) => write!(f, "dual String"),
            Self::DualPrimitive(_, PrimitiveType::Char) => write!(f, "dual Char"),

            Self::Var(_, name) => write!(f, "{}", name),
            Self::DualVar(_, name) => write!(f, "dual {}", name),
            Self::Name(_, name, args) => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty_compact(f)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }
            Self::DualName(_, name, args) => {
                write!(f, "dual {}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        arg.pretty_compact(f)?;
                    }
                    write!(f, ">")?
                }
                Ok(())
            }

            Self::Box(_, body) => {
                write!(f, "box ")?;
                body.pretty_compact(f)
            }
            Self::DualBox(_, body) => {
                write!(f, "dual box ")?;
                body.pretty_compact(f)
            }

            Self::Pair(_, arg, then) => {
                let mut then = then;
                write!(f, "(")?;
                arg.pretty_compact(f)?;
                while let Self::Pair(_, arg, next) = then.as_ref() {
                    write!(f, ", ")?;
                    arg.pretty_compact(f)?;
                    then = next;
                }
                if let Self::Break(_) = then.as_ref() {
                    write!(f, ")!")
                } else {
                    write!(f, ") ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Function(_, param, then) => {
                let mut then = then;
                write!(f, "[")?;
                param.pretty_compact(f)?;
                while let Self::Function(_, param, next) = then.as_ref() {
                    write!(f, ", ")?;
                    param.pretty_compact(f)?;
                    then = next;
                }
                if let Self::Continue(_) = then.as_ref() {
                    write!(f, "]?")
                } else {
                    write!(f, "] ")?;
                    then.pretty_compact(f)
                }
            }

            Self::Either(_, branches) => {
                let branches = branches
                    .iter()
                    .map(|(branch, _)| format!(".{branch}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "either {{ {branches} }}")
            }

            Self::Choice(_, branches) => {
                let branches = branches
                    .iter()
                    .map(|(branch, _)| format!(".{branch}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "choice {{ {branches} }}")
            }

            Self::Break(_) => write!(f, "!"),
            Self::Continue(_) => write!(f, "?"),

            Self::Recursive { label, body, .. } => {
                write!(f, "recursive")?;
                if !matches!(body.as_ref(), Self::Either(..)) {
                    if let Some(label) = label {
                        write!(f, "/{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Iterative { label, body, .. } => {
                write!(f, "iterative")?;
                if !matches!(body.as_ref(), Self::Choice(..)) {
                    if let Some(label) = label {
                        write!(f, "/{}", label)?;
                    }
                }
                write!(f, " ")?;
                body.pretty_compact(f)
            }

            Self::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }
            Self::DualSelf(_, label) => {
                write!(f, "dual self")?;
                if let Some(label) = label {
                    write!(f, "/{}", label)?;
                }
                Ok(())
            }

            Self::Exists(_, name, then) => {
                let mut then = then;
                write!(f, "(type {name}")?;
                while let Self::Exists(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, ") ")?;
                then.pretty_compact(f)
            }

            Self::Forall(_, name, then) => {
                let mut then = then;
                write!(f, "[type {name}")?;
                while let Self::Forall(_, name, next) = then.as_ref() {
                    write!(f, ", {name}")?;
                    then = next;
                }
                write!(f, "] ")?;
                then.pretty_compact(f)
            }
        }
    }

    pub fn types_at_spans(
        &self,
        type_defs: &TypeDefs,
        consume: &mut impl FnMut(Span, Option<String>, Type),
    ) {
        match self {
            Self::Primitive(_, _) | Self::DualPrimitive(_, _) => {}
            Self::Var(_, _) | Self::DualVar(_, _) => {}
            Self::Name(span, _, args) | Self::DualName(span, _, args) => {
                consume(
                    *span,
                    None,
                    self.expand_definition(type_defs)
                        .ok()
                        .unwrap_or_else(|| self.clone()),
                );
                for arg in args {
                    arg.types_at_spans(type_defs, consume);
                }
            }
            Self::Box(_, body) | Self::DualBox(_, body) => body.types_at_spans(type_defs, consume),
            Self::Pair(_, t, u) => {
                t.types_at_spans(type_defs, consume);
                u.types_at_spans(type_defs, consume);
            }
            Self::Function(_, t, u) => {
                t.types_at_spans(type_defs, consume);
                u.types_at_spans(type_defs, consume);
            }
            Self::Either(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, consume);
                }
            }
            Self::Choice(_, branches) => {
                for (_, t) in branches.iter() {
                    t.types_at_spans(type_defs, consume);
                }
            }
            Self::Break(_) => {}
            Self::Continue(_) => {}
            Self::Recursive { body, .. } => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Iterative { body, .. } => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Self_(_, _) | Self::DualSelf(_, _) => {}
            Self::Exists(_, _, body) => {
                body.types_at_spans(type_defs, consume);
            }
            Self::Forall(_, _, body) => {
                body.types_at_spans(type_defs, consume);
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

impl TypeError {
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
            Self::NoMatchingRecursiveOrIterative(span) => {
                let labels = labels_from_span(code, span);
                miette::miette!(
                    labels = labels,
                    "This `self` has no matching `recursive` or `iterative`.",
                )
            }
            Self::SelfUsedInNegativePosition(span) => {
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
            Self::TypeVariableNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Type variable `{}` is not defined.", name)
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
            Self::GlobalNameNotDefined(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "`{}` is not defined.", name)
            }
            Self::VariableDoesNotExist(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Variable `{}` does not exist.", name)
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
            Self::ParameterTypeMustBeKnown(span, param) => {
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
            Self::CannotUseLinearVariableInBox(span, name) => {
                let labels = labels_from_span(code, span);
                miette::miette!(labels = labels, "Cannot use linear variable `{}` in a `box` expression.", name)
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

impl TypeError {
    pub fn spans(&self) -> (Span, Option<Span>) {
        match self {
            Self::TypeNameAlreadyDefined(span1, span2, _)
            | Self::NameAlreadyDeclared(span1, span2, _)
            | Self::NameAlreadyDefined(span1, span2, _) => (span1.clone(), Some(span2.clone())),

            Self::DeclaredButNotDefined(span, _)
            | Self::NoMatchingRecursiveOrIterative(span)
            | Self::SelfUsedInNegativePosition(span)
            | Self::TypeNameNotDefined(span, _)
            | Self::TypeVariableNotDefined(span, _)
            | Self::DependencyCycle(span, _)
            | Self::WrongNumberOfTypeArgs(span, _, _, _)
            | Self::GlobalNameNotDefined(span, _)
            | Self::VariableDoesNotExist(span, _)
            | Self::ShadowedObligation(span, _)
            | Self::TypeMustBeKnownAtThisPoint(span, _)
            | Self::ParameterTypeMustBeKnown(span, _)
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
            | Self::CannotUseLinearVariableInBox(span, _)
            | Self::Telltypes(span, _) => (span.clone(), None),

            Self::TypesCannotBeUnified(typ1, typ2) => (typ1.span(), Some(typ2.span())),
        }
    }
}
