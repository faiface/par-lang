use super::TypeDefs;
use super::core::Type;
use super::error::TypeError;
use super::visit;

impl Type {
    pub fn is_linear(&self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        Ok(!self.is_positive(type_defs)?)
    }

    pub fn is_positive(&self, type_defs: &TypeDefs) -> Result<bool, TypeError> {
        fn visit(result: &mut bool, typ: &Type, type_defs: &TypeDefs) -> Result<(), TypeError> {
            match typ {
                // Box is always positive
                Type::Box(..) => {}

                // Negative cases
                Type::DualPrimitive(_, _)
                | Type::Var(_, _)
                | Type::DualVar(_, _)
                | Type::DualBox(_, _)
                | Type::Function(_, _, _, _)
                | Type::Choice(_, _)
                | Type::Continue(_) => {
                    *result = false;
                }
                _ => {
                    visit::continue_deref(typ, type_defs, |child| visit(result, child, type_defs))?;
                }
            }
            Ok(())
        }
        let mut result = true;

        visit(&mut result, self, type_defs)?;
        Ok(result)
    }
}
