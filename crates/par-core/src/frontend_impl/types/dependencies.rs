use super::super::language::GlobalName;
use super::core::Type;
use crate::frontend_impl::types::visit;

impl Type {
    pub fn get_dependencies(&self) -> Vec<GlobalName> {
        fn inner(typ: &Type, deps: &mut Vec<GlobalName>) -> Result<(), ()> {
            match typ {
                Type::Name(_, name, args) | Type::DualName(_, name, args) => {
                    deps.push(name.clone());
                    for arg in args {
                        inner(arg, deps)?;
                    }
                }
                _ => {
                    visit::continue_(typ, |child| inner(child, deps))?;
                }
            }
            Ok(())
        }
        let mut res = vec![];
        inner(self, &mut res).unwrap();
        res
    }
}
