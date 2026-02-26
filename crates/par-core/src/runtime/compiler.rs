use futures::{future::RemoteHandle, task::SpawnExt};

use crate::{
    par::{language::GlobalName, types::Type},
    runtime_impl::{
        flat::{stats::Rewrites, transpiler::Transpiled},
        readback::Handle,
    },
};
use std::collections::HashMap;

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
        module: &crate::par::program::CheckedModule,
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
    pub async fn start_and_instantiate(
        &self,
        name: &GlobalName,
    ) -> (Handle, RemoteHandle<Rewrites>) {
        let (reducer, net_handle) = self.code.new_reducer();
        let spawner = reducer.spawner();
        let reducer_future = reducer.spawn_reducer();
        (
            Handle::from(self.code.instantiate(net_handle, name).unwrap()),
            spawner
                .spawn_with_handle(async move {
                    let reducer = reducer_future.await;
                    reducer.runtime.rewrites
                })
                .unwrap(),
        )
    }
}
