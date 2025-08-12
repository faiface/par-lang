use super::core::Type;
use super::error::TypeError;
use super::TypeDefs;
use std::collections::BTreeMap;

impl Type {
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
}
