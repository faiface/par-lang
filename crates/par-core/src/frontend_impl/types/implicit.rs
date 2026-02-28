use crate::frontend_impl::language::LocalName;
use crate::frontend_impl::types::core::Hole;
use crate::frontend_impl::types::lattice::{intersect_types, union_types};
use crate::frontend_impl::types::{Type, TypeDefs, TypeError};
use crate::location::Span;
use im::HashMap;
use std::collections::BTreeMap;

fn solve_constraints(
    hole: &Hole,
    type_defs: &TypeDefs,
    span: &Span,
) -> Result<Option<Type>, TypeError> {
    let (lower_bounds, upper_bounds) = hole.get_constraints();
    let mut lower = Type::either(vec![]);
    let mut upper = Type::choice(vec![]);
    for typ in lower_bounds {
        lower = union_types(type_defs, span, &lower, &typ)?;
    }
    for typ in upper_bounds {
        upper = intersect_types(type_defs, span, &upper, &typ)?;
    }
    if !Type::is_assignable_to(&lower, &upper, type_defs)? {
        return Ok(None);
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

pub(crate) fn infer_holes(
    span: &Span,
    typ: &Type,
    pattern: &Type,
    names: &Vec<LocalName>,
    type_defs: &TypeDefs,
) -> Result<BTreeMap<LocalName, Type>, TypeError> {
    let mut holed_pattern = pattern.clone();
    let mut holes_map: HashMap<LocalName, Hole> = HashMap::new();
    for name in names.iter() {
        let (hole_typ, hole) = Type::hole(name.clone());
        holes_map.insert(name.clone(), hole);
        holed_pattern = holed_pattern
            .clone()
            .substitute(BTreeMap::from([(name, &hole_typ)]))?;
    }

    if !Type::is_assignable_to(&typ, &holed_pattern, type_defs)? {
        return Err(TypeError::CannotAssignFromTo(
            span.clone(),
            typ.clone(),
            pattern.clone(),
        ));
    }

    let mut res = BTreeMap::new();

    for name in names {
        let hole = holes_map.get(&name).unwrap();
        match solve_constraints(hole, type_defs, span)? {
            Some(solved_type) => {
                res.insert(name.clone(), solved_type);
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
