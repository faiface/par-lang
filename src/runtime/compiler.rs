use futures::future::RemoteHandle;

use crate::{
    par::{language::GlobalName, types::Type},
    runtime::{new::transpiler::Transpiled, old::compiler::IcCompiled, readback::Handle},
    spawn::TokioSpawn,
};
use std::collections::HashMap;

use std::{fmt::Display, sync::Arc};

#[derive(Clone)]
pub enum Backend {
    Old(IcCompiled),
    New(Transpiled),
}

#[derive(Clone)]
pub struct Compiled {
    pub backend: Backend,
    pub name_to_ty: HashMap<GlobalName, Type>,
}

impl Display for Compiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.backend {
            Backend::Old(ic_compiled) => ic_compiled.fmt(f),
            Backend::New(new_compiled) => new_compiled.fmt(f),
        }
    }
}

impl Compiled {
    pub fn compile_file(
        module: &crate::par::program::CheckedModule,
        use_new: bool,
    ) -> Result<Self, crate::runtime::RuntimeCompilerError> {
        Ok(Self {
            backend:
            if use_new {
                Backend::New(Transpiled::compile_file(module)?)
            } else {
                Backend::Old(IcCompiled::compile_file(module)?)
            },
            name_to_ty: module.declarations.iter().map(|(a, b)| {
                (a.clone(), b.typ.clone())
            }).collect(),
        })
    }
    pub fn get_type_of(&self, name: &GlobalName) -> Option<Type> {
        self.name_to_ty.get(name).cloned()
    }
    pub async fn start_and_instantiate(&self, name: &GlobalName) -> (Handle, RemoteHandle<()>) {
        match &self.backend {
            Backend::Old(ic_compiled) => {
                let mut net = ic_compiled.create_net();
                let child_net = ic_compiled.get_with_name(name).unwrap();
                let tree = net.inject_net(child_net);
                let spawner = Arc::new(TokioSpawn::new());
                let (net_wrapper, reducer_future) = net.start_reducer(spawner.clone());
                (Handle::Old(net_wrapper.new_handle(tree)), reducer_future)
            }
            Backend::New(transpiled) => {
                let mut reducer = transpiled.new_reducer();
                let net_handle = reducer.net_handle().clone();
                let reducer_future = reducer.spawn_reducer();
                (
                    Handle::New(transpiled.instantiate(net_handle, name).await.unwrap()),
                    reducer_future,
                )
            }
        }
    }
}
