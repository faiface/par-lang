use crate::location::Span;
use crate::par::types::{Type, TypeDefs, TypeError};
use std::collections::BTreeMap;

pub fn union_types(
    typedefs: &TypeDefs,
    span: &Span,
    type1: &Type,
    type2: &Type,
) -> Result<Type, TypeError> {
    if type1.is_assignable_to(type2, typedefs)? {
        return Ok(type2.clone());
    }
    if type2.is_assignable_to(type1, typedefs)? {
        return Ok(type1.clone());
    }

    Ok(match (type1, type2) {
        (Type::Either(_, branches), t2) if branches.is_empty() => t2.clone(),
        (t1, Type::Either(_, branches)) if branches.is_empty() => t1.clone(),
        (t1 @ Type::Choice(_, branches), _t2) if branches.is_empty() => t1.clone(),
        (_t1, t2 @ Type::Choice(_, branches)) if branches.is_empty() => t2.clone(),
        (Type::Name(span1, name1, args1), t2) => {
            union_types(typedefs, span, &typedefs.get(span1, name1, args1)?, t2)?
        }
        (t1, Type::Name(span2, name2, args2)) => {
            union_types(typedefs, span, t1, &typedefs.get(span2, name2, args2)?)?
        }
        (Type::DualName(span1, name1, args1), t2) => {
            union_types(typedefs, span, &typedefs.get_dual(span1, name1, args1)?, t2)?
        }
        (t1, Type::DualName(span2, name2, args2)) => {
            union_types(typedefs, span, t1, &typedefs.get_dual(span2, name2, args2)?)?
        }
        (Type::Var(_, name1), Type::Var(_, name2)) if name1 == name2 => {
            Type::Var(span.clone(), name1.clone())
        }
        (Type::DualVar(_, name1), Type::DualVar(_, name2)) if name1 == name2 => {
            Type::DualVar(span.clone(), name1.clone())
        }
        (Type::Box(_, inner1), Type::Box(_, inner2)) => Type::Box(
            span.clone(),
            Box::new(union_types(typedefs, span, inner1, inner2)?),
        ),
        (Type::DualBox(_, inner1), Type::DualBox(_, inner2)) => Type::DualBox(
            span.clone(),
            Box::new(intersect_types(typedefs, span, inner1, inner2)?),
        ),
        (Type::Break(_), Type::Break(_)) => Type::Break(span.clone()),
        (Type::Continue(_), Type::Continue(_)) => Type::Continue(span.clone()),
        (Type::Primitive(_, p1), Type::Primitive(_, p2)) if p1 == p2 => {
            Type::Primitive(span.clone(), p1.clone())
        }
        (Type::DualPrimitive(_, p1), Type::DualPrimitive(_, p2)) if p1 == p2 => {
            Type::DualPrimitive(span.clone(), p1.clone())
        }
        (Type::Pair(_, left1, right1), Type::Pair(_, left2, right2)) => Type::Pair(
            span.clone(),
            Box::new(union_types(typedefs, span, left1, left2)?),
            Box::new(union_types(typedefs, span, right1, right2)?),
        ),
        (Type::Function(_, arg1, ret1), Type::Function(_, arg2, ret2)) => Type::Function(
            span.clone(),
            Box::new(intersect_types(typedefs, span, arg1, arg2)?),
            Box::new(union_types(typedefs, span, ret1, ret2)?),
        ),
        (Type::Either(_, branches1), Type::Either(_, branches2)) => {
            let mut new_branches = branches1.clone();
            for (name, typ2) in branches2 {
                if let Some(typ1) = new_branches.get(name) {
                    new_branches.insert(name.clone(), union_types(typedefs, span, typ1, typ2)?);
                } else {
                    new_branches.insert(name.clone(), typ2.clone());
                }
            }
            Type::Either(span.clone(), new_branches)
        }
        (Type::Choice(_, branches1), Type::Choice(_, branches2)) => {
            let mut new_branches = BTreeMap::new();
            for (name, typ1) in branches1 {
                if let Some(typ2) = branches2.get(name) {
                    new_branches.insert(name.clone(), union_types(typedefs, span, typ1, typ2)?);
                }
            }
            Type::Choice(span.clone(), new_branches)
        }
        (Type::Forall(_, name1, body1), Type::Forall(_, name2, body2)) => Type::Forall(
            span.clone(),
            name1.clone(),
            Box::new(union_types(
                typedefs,
                span,
                body1,
                &body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(Span::None, name1.clone()),
                )]))?,
            )?),
        ),
        (Type::Exists(_, name1, body1), Type::Exists(_, name2, body2)) => Type::Exists(
            span.clone(),
            name1.clone(),
            Box::new(union_types(
                typedefs,
                span,
                body1,
                &body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(Span::None, name1.clone()),
                )]))?,
            )?),
        ),
        (Type::Box(_, t1), t2) => union_types(typedefs, span, t1, t2)?,
        (t1, Type::Box(_, t2)) => union_types(typedefs, span, t1, t2)?,
        (t1, t2) => return Err(TypeError::TypesCannotBeUnified(t1.clone(), t2.clone())),
    })
}

pub fn intersect_types(
    typedefs: &TypeDefs,
    span: &Span,
    type1: &Type,
    type2: &Type,
) -> Result<Type, TypeError> {
    if type1.is_assignable_to(type2, typedefs)? {
        return Ok(type2.clone());
    }
    if type2.is_assignable_to(type1, typedefs)? {
        return Ok(type1.clone());
    }

    Ok(match (type1, type2) {
        (Type::Either(_, branches), t2) if branches.is_empty() => t2.clone(),
        (t1, Type::Either(_, branches)) if branches.is_empty() => t1.clone(),
        (t1 @ Type::Choice(_, branches), _t2) if branches.is_empty() => t1.clone(),
        (_t1, t2 @ Type::Choice(_, branches)) if branches.is_empty() => t2.clone(),
        (Type::Name(span1, name1, args1), t2) => {
            intersect_types(typedefs, span, &typedefs.get(span1, name1, args1)?, t2)?
        }
        (t1, Type::Name(span2, name2, args2)) => {
            intersect_types(typedefs, span, t1, &typedefs.get(span2, name2, args2)?)?
        }
        (Type::DualName(span1, name1, args1), t2) => {
            intersect_types(typedefs, span, &typedefs.get_dual(span1, name1, args1)?, t2)?
        }
        (t1, Type::DualName(span2, name2, args2)) => {
            intersect_types(typedefs, span, t1, &typedefs.get_dual(span2, name2, args2)?)?
        }
        (Type::Var(_, name1), Type::Var(_, name2)) if name1 == name2 => {
            Type::Var(span.clone(), name1.clone())
        }
        (Type::DualVar(_, name1), Type::DualVar(_, name2)) if name1 == name2 => {
            Type::DualVar(span.clone(), name1.clone())
        }
        (Type::Box(_, inner1), Type::Box(_, inner2)) => Type::Box(
            span.clone(),
            Box::new(intersect_types(typedefs, span, inner1, inner2)?),
        ),
        (Type::DualBox(_, inner1), Type::DualBox(_, inner2)) => Type::DualBox(
            span.clone(),
            Box::new(union_types(typedefs, span, &inner1, &inner2)?),
        ),
        (Type::Break(_), Type::Break(_)) => Type::Break(span.clone()),
        (Type::Continue(_), Type::Continue(_)) => Type::Continue(span.clone()),
        (Type::Primitive(_, p1), Type::Primitive(_, p2)) if p1 == p2 => {
            Type::Primitive(span.clone(), p1.clone())
        }
        (Type::DualPrimitive(_, p1), Type::DualPrimitive(_, p2)) if p1 == p2 => {
            Type::DualPrimitive(span.clone(), p1.clone())
        }
        (Type::Pair(_, left1, right1), Type::Pair(_, left2, right2)) => Type::Pair(
            span.clone(),
            Box::new(intersect_types(typedefs, span, left1, left2)?),
            Box::new(intersect_types(typedefs, span, right1, right2)?),
        ),
        (Type::Function(_, arg1, ret1), Type::Function(_, arg2, ret2)) => Type::Function(
            span.clone(),
            Box::new(union_types(typedefs, span, &arg1, &arg2)?),
            Box::new(intersect_types(typedefs, span, ret1, ret2)?),
        ),
        (Type::Either(_, branches1), Type::Either(_, branches2)) => {
            let mut new_branches = BTreeMap::new();
            for (name, typ1) in branches1 {
                if let Some(typ2) = branches2.get(name) {
                    new_branches.insert(name.clone(), union_types(typedefs, span, typ1, typ2)?);
                }
            }
            Type::Either(span.clone(), new_branches)
        }
        (Type::Choice(_, branches1), Type::Choice(_, branches2)) => {
            let mut new_branches = branches1.clone();
            for (name, typ2) in branches2 {
                if let Some(typ1) = new_branches.get(name) {
                    new_branches.insert(name.clone(), intersect_types(typedefs, span, typ1, typ2)?);
                } else {
                    new_branches.insert(name.clone(), typ2.clone());
                }
            }
            Type::Choice(span.clone(), new_branches)
        }
        (Type::Forall(_, name1, body1), Type::Forall(_, name2, body2)) => Type::Forall(
            span.clone(),
            name1.clone(),
            Box::new(intersect_types(
                typedefs,
                span,
                body1,
                &body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(Span::None, name1.clone()),
                )]))?,
            )?),
        ),
        (Type::Exists(_, name1, body1), Type::Exists(_, name2, body2)) => Type::Exists(
            span.clone(),
            name1.clone(),
            Box::new(intersect_types(
                typedefs,
                span,
                body1,
                &body2.clone().substitute(BTreeMap::from([(
                    name2,
                    &Type::Var(Span::None, name1.clone()),
                )]))?,
            )?),
        ),
        (Type::Box(_, t1), t2) => intersect_types(typedefs, span, t1, t2)?,
        (t1, Type::Box(_, t2)) => intersect_types(typedefs, span, t1, t2)?,
        (t1, t2) => return Err(TypeError::TypesCannotBeUnified(t1.clone(), t2.clone())),
    })
}
