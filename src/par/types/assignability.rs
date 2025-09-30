use crate::location::Span;
use crate::par::types::assignability::SubtypeResult::{Compatible, Cycle, Incompatible};
use crate::par::types::{PrimitiveType, Type, TypeDefs, TypeError};
use indexmap::IndexSet;
use std::cmp::max;
use std::collections::BTreeMap;
use std::env;
use std::ops::BitAnd;

#[derive(Clone)]
struct SubtypeContext<'a> {
    type_defs: &'a TypeDefs,
    visited: IndexSet<(Type, Type)>,
}

impl<'a> SubtypeContext<'a> {
    fn new<'b>(type_defs: &'b TypeDefs) -> SubtypeContext<'b> {
        SubtypeContext {
            type_defs,
            visited: Default::default(),
        }
    }
    fn normalize(&mut self, typ: Type) -> Result<Type, TypeError> {
        Ok(match typ {
            Type::Name(span, name, args) => {
                self.normalize(self.type_defs.get(&span, &name, &args)?)?
            }
            Type::DualName(span, name, args) => {
                self.normalize(self.type_defs.get(&span, &name, &args)?.dual(Span::None))?
            }
            t => t,
        })
    }
}

enum SubtypeResult {
    Compatible,
    Incompatible,
    Cycle {
        min_left: Type,
        size_left: u32,
        min_right: Type,
        size_right: u32,
        /**
        Time To Live. To avoid merging cycles that don't intersect, as we bubble up the recursive call stack,
        we want to keep the cycle only until its starting point, then simplify it to Compatible.
        Any cycles encountered before that do not intersect it.

        In order to do that, we set ttl to the length of the cycle at creation, and decrease it at any return.
        Once it reaches 0, we simplify it to Compatible.
        */
        ttl: usize,
    },
}

impl BitAnd for SubtypeResult {
    type Output = SubtypeResult;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Compatible, Compatible) => Compatible,
            (c @ Cycle { .. }, Compatible) | (Compatible, c @ Cycle { .. }) => c,
            (
                Cycle {
                    min_left: min_left1,
                    size_left: size_left1,
                    min_right: min_right1,
                    size_right: size_right1,
                    ttl: ttl1,
                },
                Cycle {
                    min_left: min_left2,
                    size_left: size_left2,
                    min_right: min_right2,
                    size_right: size_right2,
                    ttl: ttl2,
                },
            ) => {
                let (min_left, size_left) = if size_left1 <= size_left2 {
                    (min_left1, size_left1)
                } else {
                    (min_left2, size_left2)
                };
                let (min_right, size_right) = if size_right1 <= size_right2 {
                    (min_right1, size_right1)
                } else {
                    (min_right2, size_right2)
                };
                let ttl = max(ttl1, ttl2);
                if !matches!(min_left, Type::Recursive { .. })
                    && !matches!(min_right, Type::Iterative { .. })
                {
                    Incompatible
                } else {
                    Cycle {
                        min_left,
                        size_left,
                        min_right,
                        size_right,
                        ttl,
                    }
                }
            }
            (_, Incompatible) | (Incompatible, _) => Incompatible,
        }
    }
}

impl SubtypeResult {
    fn ttl_dec(mut self) -> Self {
        match &mut self {
            Cycle { ttl, .. } => {
                if *ttl == 0 {
                    Compatible
                } else {
                    *ttl -= 1;
                    self
                }
            }
            _ => self,
        }
    }
}

impl Type {
    pub fn check_assignable(
        &self,
        span: &Span,
        u: &Type,
        type_defs: &TypeDefs,
    ) -> Result<(), TypeError> {
        if !self.is_assignable_to(u, type_defs)? {
            return Err(TypeError::CannotAssignFromTo(
                span.clone(),
                self.clone(),
                u.clone(),
            ));
        }
        Ok(())
    }

    pub fn is_assignable_to(&self, other: &Self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        match Type::is_subtype_helper(self.clone(), other.clone(), SubtypeContext::new(type_defs))?
        {
            Compatible => Ok(true),
            Incompatible => Ok(false),
            Cycle {
                min_left,
                min_right,
                ..
            } => {
                if matches!(min_left, Type::Recursive { .. }) {
                    Ok(true)
                } else if matches!(min_right, Type::Iterative { .. }) {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    /**
    This function checks if `self` <: `other`.

    The algorithm is based on the subtyping relation in `A Logical Account of Subtyping for Session Types (2023)`.

    The implementation takes inspiration from `Subtyping recursive types (1993)`.
    */
    pub(crate) fn is_primitive_subtype(p1: &PrimitiveType, p2: &PrimitiveType) -> bool {
        match (p1, p2) {
            (PrimitiveType::Nat, PrimitiveType::Int) => true,
            (PrimitiveType::Char, PrimitiveType::String) => true,
            (PrimitiveType::Byte, PrimitiveType::Bytes) => true,
            _ => p1 == p2,
        }
    }

    fn is_subtype_helper(
        mut type1: Self,
        mut type2: Self,
        mut ctx: SubtypeContext,
    ) -> Result<SubtypeResult, TypeError> {
        // Debug trace helper
        if debug_enabled() {
            debug_log_entry(&type1, &type2, &ctx);
        }

        type1 = ctx.normalize(type1)?;
        type2 = ctx.normalize(type2)?;

        if type1 == type2 {
            return Ok(Compatible);
        }

        let pair = (type1, type2);

        if let Some(ind) = ctx.visited.get_index_of(&pair) {
            if debug_enabled() {
                debug_log_stack(&ctx);
            }
            let min_left = ctx
                .visited
                .iter()
                .skip(ind)
                .map(|(t1, _t2)| t1)
                .filter(|t1| t1.is_fixpoint())
                .min_by_key(|t1| t1.size(ctx.type_defs).unwrap())
                .expect("minimum should exist");
            let min_right = ctx
                .visited
                .iter()
                .skip(ind)
                .map(|(_t1, t2)| t2)
                .filter(|t2| t2.is_fixpoint())
                .min_by_key(|t2| t2.size(ctx.type_defs).unwrap())
                .expect("minimum should exist");
            if debug_enabled() {
                eprintln!("min_left: {:}", min_left);
                eprintln!("min_right: {:}", min_right);
            }
            if !matches!(min_left, Type::Recursive { .. })
                && !matches!(min_right, Type::Iterative { .. })
            {
                return Ok(Incompatible);
            }
            return Ok(Cycle {
                min_left: min_left.clone(),
                size_left: min_left.size(ctx.type_defs)?,
                min_right: min_right.clone(),
                size_right: min_right.size(ctx.type_defs)?,
                ttl: ctx.visited.len(),
            });
        }

        ctx.visited.insert(pair.clone());
        let (type1, type2) = pair;

        if let Type::Iterative { asc: asc1, .. } = &type1 {
            if !asc1.is_empty() {
                return if let Self::Recursive { asc: asc2, .. } = &type2 {
                    if asc1.is_subset(asc2) {
                        Ok(Compatible)
                    } else {
                        Ok(Incompatible)
                    }
                } else {
                    Ok(Incompatible)
                };
            }
        }

        if let Type::Recursive { asc: asc2, .. } = &type2 {
            if !asc2.is_empty() {
                return if let Self::Recursive { asc: asc1, .. } = &type1 {
                    if asc2.is_subset(asc1) {
                        Ok(Compatible)
                    } else {
                        Ok(Incompatible)
                    }
                } else {
                    Ok(Incompatible)
                };
            }
        }

        if let Type::Recursive { .. } | Type::Iterative { .. } = &type1 {
            let type1 = Type::expand_fixpoint_unfounded(&type1)?;
            return Ok(Type::is_subtype_helper(type1, type2.clone(), ctx.clone())?.ttl_dec());
        }

        if let Type::Recursive { .. } | Type::Iterative { .. } = &type2 {
            let type2 = Type::expand_fixpoint_unfounded(&type2)?;
            return Ok(Type::is_subtype_helper(type1.clone(), type2, ctx.clone())?.ttl_dec());
        }

        let res: SubtypeResult = match (type1, type2) {
            (Self::Primitive(_, p1), Self::Primitive(_, p2)) => {
                if Self::is_primitive_subtype(&p1, &p2) {
                    Compatible
                } else {
                    Incompatible
                }
            }
            (Self::DualPrimitive(_, p1), Self::DualPrimitive(_, p2)) => {
                if Self::is_primitive_subtype(&p2, &p1) {
                    Compatible
                } else {
                    Incompatible
                }
            }

            (Self::Var(_, name1), Self::Var(_, name2)) => {
                if name1 == name2 {
                    Compatible
                } else {
                    Incompatible
                }
            }
            (Self::DualVar(_, name1), Self::DualVar(_, name2)) => {
                if name1 == name2 {
                    Compatible
                } else {
                    Incompatible
                }
            }

            (t1, Self::Box(_, t2)) if t1.is_positive(ctx.type_defs)? => {
                Type::is_subtype_helper(t1, *t2, ctx)?
            }
            (Self::DualBox(_, t1), t2) if t1.is_positive(ctx.type_defs)? => {
                Type::is_subtype_helper(t1.clone().dual(Span::None), t2, ctx)?
            }
            (Self::Box(_, t1), Self::Box(_, t2)) => Type::is_subtype_helper(*t1, *t2, ctx)?,
            (Self::Box(_, t1), t2) => Type::is_subtype_helper(*t1, t2, ctx)?,
            (Self::DualBox(_, t1), Self::DualBox(_, t2)) => {
                let t1 = t1.clone().dual(Span::None);
                let t2 = t2.clone().dual(Span::None);
                Type::is_subtype_helper(t1, t2, ctx)?
            }
            (t1, Self::DualBox(_, t2)) => {
                let t2 = t2.clone().dual(Span::None);
                Type::is_subtype_helper(t1, t2, ctx)?
            }

            (Self::Pair(_, t1, u1), Self::Pair(_, t2, u2)) => {
                Type::is_subtype_helper(*t1, *t2, ctx.clone())?
                    & Type::is_subtype_helper(*u1, *u2, ctx)?
            }
            (Self::Function(_, t1, u1), Self::Function(_, t2, u2)) => {
                let t1 = t1.clone().dual(Span::None);
                let t2 = t2.clone().dual(Span::None);
                Type::is_subtype_helper(t1, t2, ctx.clone())?
                    & Type::is_subtype_helper(*u1, *u2, ctx)?
            }
            (Self::Either(_, branches1), _) if branches1.is_empty() => Compatible,
            (Self::Either(_, branches1), Self::Either(_, branches2)) => {
                let mut res = Compatible;
                for (branch, t1) in branches1 {
                    let Some(t2) = branches2.get(&branch) else {
                        return Ok(Incompatible);
                    };
                    res = res & Type::is_subtype_helper(t1.clone(), t2.clone(), ctx.clone())?
                }
                res
            }
            (_, Self::Choice(_, branches2)) if branches2.is_empty() => Compatible,
            (Self::Choice(_, branches1), Self::Choice(_, branches2)) => {
                let mut res = Compatible;
                for (branch, t2) in branches2 {
                    let Some(t1) = branches1.get(&branch) else {
                        return Ok(Incompatible);
                    };
                    res = res & Type::is_subtype_helper(t1.clone(), t2.clone(), ctx.clone())?;
                }
                res
            }
            (Self::Break(_), Self::Break(_)) => Compatible,
            (Self::Continue(_), Self::Continue(_)) => Compatible,

            (Self::Exists(loc, name1, body1), Self::Exists(_, name2, body2))
            | (Self::Forall(loc, name1, body1), Self::Forall(_, name2, body2)) => {
                let body2 = body2.clone().substitute(BTreeMap::from([(
                    &name2,
                    &Type::Var(loc.clone(), name1.clone()),
                )]))?;
                // type_defs.vars.insert(name1.clone());
                Type::is_subtype_helper(*body1, body2, ctx)?
            }

            _ => {
                if debug_enabled() {
                    debug_log("fallback => false");
                    debug_log_stack(&ctx);
                }
                Incompatible
            }
        };
        Ok(res.ttl_dec())
    }
}

fn debug_enabled() -> bool {
    env::var("PAR_SUBTYPE_DEBUG").is_ok()
}

fn debug_log(msg: &str) {
    eprintln!("[subtype] {}", msg);
}

fn debug_log_entry(left: &Type, right: &Type, ctx: &SubtypeContext) {
    eprintln!("-----------------------");
    eprintln!("[subtype] {} <= {}", left, right);
    eprintln!("[subtype]   visited={}", ctx.visited.len());
}

fn debug_log_stack(ctx: &SubtypeContext) {
    eprintln!("[subtype] -------Stack-------");
    for (i, (type1, type2)) in ctx.visited.iter().rev().enumerate() {
        eprintln!("[subtype] #{i}: {} <= {}", type1, type2);
    }
    eprintln!("[subtype] -------Stack-End-------");
}
