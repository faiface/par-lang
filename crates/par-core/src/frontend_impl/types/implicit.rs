use crate::frontend_impl::language::{LocalName, TypeConstraint, TypeParameter};
use crate::frontend_impl::types::core::Hole;
use crate::frontend_impl::types::lattice::{intersect_types, union_types};
use crate::frontend_impl::types::{Type, TypeDefs, TypeError};
use crate::location::Span;
use im::HashMap;
use std::collections::BTreeMap;

fn solve_constraints<S: Clone + Eq + std::hash::Hash>(
    hole: &Hole<S>,
    constraint: TypeConstraint,
    type_defs: &TypeDefs<S>,
    span: &Span,
) -> Result<Option<Type<S>>, TypeError<S>> {
    let (lower_bounds, upper_bounds) = hole.get_constraints();
    let mut lower = Type::Either(Span::None, BTreeMap::new());
    let mut upper = Type::Choice(Span::None, BTreeMap::new());
    for typ in lower_bounds {
        lower = union_types(type_defs, span, &lower, &typ)?;
    }
    for typ in upper_bounds {
        upper = intersect_types(type_defs, span, &upper, &typ)?;
    }
    if !lower.is_assignable_to(&upper, type_defs)? {
        return Ok(None);
    }

    if matches!(constraint, TypeConstraint::Signed)
        && lower.is_assignable_to(&Type::nat(), type_defs)?
        && Type::nat().is_assignable_to(&lower, type_defs)?
    {
        let promoted = Type::int();
        if promoted.is_assignable_to(&upper, type_defs)? {
            return Ok(Some(promoted));
        }
    }

    if let Type::Choice(_, branches) = &upper {
        if branches.is_empty() {
            return Ok(Some(lower));
        }
    }

    if let Type::Either(_, branches) = &lower {
        if branches.is_empty() {
            return Ok(Some(upper));
        }
    }

    Ok(Some(lower))
}

pub(crate) fn infer_holes<S: Clone + Eq + std::hash::Hash>(
    span: &Span,
    typ: &Type<S>,
    pattern: &Type<S>,
    names: &[TypeParameter],
    type_defs: &TypeDefs<S>,
) -> Result<BTreeMap<LocalName, Type<S>>, TypeError<S>> {
    let mut holed_pattern = pattern.clone();
    let mut holes_map: HashMap<LocalName, Hole<S>> = HashMap::new();
    for name in names.iter() {
        let (hole_typ, hole) = Type::hole(name.name.clone());
        holes_map.insert(name.name.clone(), hole);
        holed_pattern = holed_pattern
            .clone()
            .substitute(BTreeMap::from([(&name.name, &hole_typ)]))?;
    }

    if !typ.is_assignable_to(&holed_pattern, type_defs)? {
        return Err(TypeError::CannotAssignFromTo(
            span.clone(),
            typ.clone(),
            pattern.clone(),
        ));
    }

    let mut res = BTreeMap::new();

    for name in names {
        let hole = holes_map.get(&name.name).unwrap();
        match solve_constraints(hole, name.constraint, type_defs, span)? {
            Some(solved_type) => {
                if !solved_type.satisfies_constraint(name.constraint, type_defs)? {
                    return Err(TypeError::TypeDoesNotSatisfyConstraint(
                        span.clone(),
                        name.name.clone(),
                        solved_type,
                        name.constraint,
                    ));
                }
                res.insert(name.name.clone(), solved_type);
            }
            _ => {
                return Err(TypeError::CannotAssignFromTo(
                    span.clone(),
                    typ.clone(),
                    pattern.clone(),
                ));
            }
        }
    }
    Ok(res)
}
