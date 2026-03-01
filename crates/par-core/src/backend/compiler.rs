use crate::frontend_impl::{language::GlobalName, types::Type};
use std::collections::HashMap;

use crate::backend::flat::transpiler::Transpiled;
use std::fmt::Display;

#[derive(Clone)]
pub struct Compiled {
    pub code: Transpiled,
    pub name_to_ty: HashMap<GlobalName, Type>,
}

impl Display for Compiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.code.fmt(f)
    }
}

impl Compiled {
    pub fn compile_file(
        module: &crate::frontend_impl::program::CheckedModule,
        max_interactions: u32,
    ) -> Result<Self, crate::runtime_impl::RuntimeCompilerError> {
        Ok(Self {
            code: Transpiled::compile_file(module, max_interactions)?,
            name_to_ty: module
                .definitions
                .iter()
                .map(|(a, (_, typ))| (a.clone(), typ.clone()))
                .collect(),
        })
    }
    pub fn get_type_of(&self, name: &GlobalName) -> Option<Type> {
        self.name_to_ty.get(name).cloned()
    }
}
