use crate::frontend_impl::language::{GlobalName, Universal};
use crate::frontend_impl::types::Type;
use crate::location::Span;
use std::collections::HashMap;

use crate::backend::flat::transpiler::{Transpiled, link_transpiled};
use par_runtime::linker::{LinkError, Linked, Unlinked};
use std::fmt::Display;

#[derive(Clone)]
pub struct Compiled<Ext: Clone> {
    pub code: Transpiled<Ext>,
    pub name_to_ty: HashMap<GlobalName<Universal>, Type<Universal>>,
}

#[derive(Clone, Debug)]
pub enum RuntimeCompilerError {
    Inet(crate::backend::tree::compiler::Error),
    Link(LinkError),
}

impl RuntimeCompilerError {
    pub fn display(&self, code: &str) -> String {
        match self {
            Self::Inet(error) => error.display(code),
            Self::Link(error) => error.to_string(),
        }
    }

    pub fn spans(&self) -> (Span, Vec<Span>) {
        match self {
            Self::Inet(error) => error.spans(),
            Self::Link(_) => (Span::None, vec![]),
        }
    }
}

impl From<crate::backend::tree::compiler::Error> for RuntimeCompilerError {
    fn from(value: crate::backend::tree::compiler::Error) -> Self {
        Self::Inet(value)
    }
}

impl From<LinkError> for RuntimeCompilerError {
    fn from(value: LinkError) -> Self {
        Self::Link(value)
    }
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
    ) -> Result<Self, RuntimeCompilerError> {
        Ok(Self {
            code: Transpiled::compile_file(module, max_interactions)?,
            name_to_ty: module
                .definitions
                .iter()
                .map(|(a, (_, typ))| (a.clone(), typ.clone()))
                .collect(),
        })
    }

    pub fn link(self) -> Result<Compiled<Linked>, RuntimeCompilerError> {
        Ok(Compiled {
            code: link_transpiled(self.code)?,
            name_to_ty: self.name_to_ty,
        })
    }
}

impl<Ext: Clone> Compiled<Ext> {
    pub fn get_type_of(&self, name: &GlobalName<Universal>) -> Option<Type<Universal>> {
        self.name_to_ty.get(name).cloned()
    }
}
