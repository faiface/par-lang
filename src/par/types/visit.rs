use super::{Type, TypeDefs, TypeError};

pub fn continue_<E, F>(typ: &Type, mut visit: F) -> Result<(), E>
where
    F: FnMut(&Type) -> Result<(), E>,
{
    match typ {
        Type::Name(_, _, args) => {
            for arg in args {
                visit(arg)?;
            }
        }
        Type::DualName(_, _, args) => {
            for arg in args {
                visit(arg)?;
            }
        }
        Type::Box(_, inner) | Type::DualBox(_, inner) => {
            visit(inner)?;
        }
        Type::Pair(_, left, right) | Type::Function(_, left, right) => {
            visit(left)?;
            visit(right)?;
        }
        Type::Either(_, branches) | Type::Choice(_, branches) => {
            for (_name, typ) in branches {
                visit(typ)?;
            }
        }
        Type::Recursive { body, .. } | Type::Iterative { body, .. } => {
            visit(body)?;
        }
        Type::Exists(_, _, body) | Type::Forall(_, _, body) => {
            visit(body)?;
        }
        Type::Primitive(..)
        | Type::DualPrimitive(..)
        | Type::Var(..)
        | Type::DualVar(..)
        | Type::Break(..)
        | Type::Continue(..)
        | Type::Self_(..)
        | Type::DualSelf(..) => {}
    }
    Ok(())
}

pub fn continue_mut<E, F>(typ: &mut Type, mut visit: F) -> Result<(), E>
where
    F: FnMut(&mut Type) -> Result<(), E>,
{
    match typ {
        Type::Name(_, _, args) => {
            for arg in args {
                visit(arg)?;
            }
        }
        Type::DualName(_, _, args) => {
            for arg in args {
                visit(arg)?;
            }
        }
        Type::Box(_, inner) | Type::DualBox(_, inner) => {
            visit(inner)?;
        }
        Type::Pair(_, left, right) | Type::Function(_, left, right) => {
            visit(left)?;
            visit(right)?;
        }
        Type::Either(_, branches) | Type::Choice(_, branches) => {
            for (_name, typ) in branches {
                visit(typ)?;
            }
        }
        Type::Recursive { body, .. } | Type::Iterative { body, .. } => {
            visit(body)?;
        }
        Type::Exists(_, _, body) | Type::Forall(_, _, body) => {
            visit(body)?;
        }
        Type::Primitive(..)
        | Type::DualPrimitive(..)
        | Type::Var(..)
        | Type::DualVar(..)
        | Type::Break(..)
        | Type::Continue(..)
        | Type::Self_(..)
        | Type::DualSelf(..) => {}
    }
    Ok(())
}

pub fn continue_deref<F>(typ: &Type, defs: &TypeDefs, mut visit: F) -> Result<(), TypeError>
where
    F: FnMut(&Type) -> Result<(), TypeError>,
{
    match typ {
        Type::Name(span, name, args) => {
            let typ = defs.get(span, name, args).expect("Type not found");
            visit(&typ)?;
        }
        Type::DualName(span, name, args) => {
            let typ = defs.get_dual(span, name, args).expect("Type not found");
            visit(&typ)?;
        }
        _ => {
            continue_(typ, visit)?;
        }
    }
    Ok(())
}
