use crate::frontend_impl::language::{GlobalName, Universal};
use crate::frontend_impl::types::Type;
use std::collections::HashMap;

use crate::backend::flat::transpiler::{Transpiled, link_transpiled};
use par_runtime::linker::{Linked, Unlinked};
use std::fmt::Display;

#[derive(Clone)]
pub struct Compiled<Ext: Clone> {
    pub code: Transpiled<Ext>,
    pub name_to_ty: HashMap<GlobalName<Universal>, Type<Universal>>,
}

impl<Ext: Clone> Display for Compiled<Ext> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.code.fmt(f)
    }
}

impl Compiled<Unlinked> {
    pub fn compile_file(
        module: &crate::frontend_impl::program::CheckedModule<Universal>,
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

    pub fn link(self) -> Compiled<Linked> {
        Compiled {
            code: link_transpiled(self.code),
            name_to_ty: self.name_to_ty,
        }
    }
}

impl<Ext: Clone> Compiled<Ext> {
    pub fn get_type_of(&self, name: &GlobalName<Universal>) -> Option<Type<Universal>> {
        self.name_to_ty.get(name).cloned()
    }
}
