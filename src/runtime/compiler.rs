use futures::future::RemoteHandle;

use crate::{
    par::{language::GlobalName, types::Type},
    runtime::{new::transpiler::Transpiled, old::compiler::IcCompiled, readback::Handle},
    spawn::TokioSpawn,
};

use std::{fmt::Display, sync::Arc};

#[derive(Clone)]
pub enum Compiled {
    Old(IcCompiled),
    New(Transpiled),
}

impl Display for Compiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Compiled::Old(ic_compiled) => ic_compiled.fmt(f),
            Compiled::New(new_compiled) => new_compiled.fmt(f),
        }
    }
}

impl Compiled {
    pub fn compile_file(
        module: &crate::par::program::CheckedModule,
        use_new: bool,
    ) -> Result<Self, crate::runtime::RuntimeCompilerError> {
        if use_new {
            Transpiled::compile_file(module).map(Compiled::New)
        } else {
            Ok(IcCompiled::compile_file(module).unwrap()).map(Compiled::Old)
        }
    }
    pub fn get_type_of(&self, name: &GlobalName) -> Option<Type> {
        match self {
            Compiled::Old(ic_compiled) => ic_compiled.get_type_of(name),
            Compiled::New(transpiled) => transpiled.get_type_of(name),
        }
    }
    pub async fn start_and_instantiate(&self, name: &GlobalName) -> (Handle, RemoteHandle<()>) {
        match self {
            Compiled::Old(ic_compiled) => {
                let mut net = ic_compiled.create_net();
                let child_net = ic_compiled.get_with_name(name).unwrap();
                let tree = net.inject_net(child_net);
                let spawner = Arc::new(TokioSpawn::new());
                let (net_wrapper, reducer_future) = net.start_reducer(spawner.clone());
                (Handle::Old(net_wrapper.new_handle(tree)), reducer_future)
            }
            Compiled::New(transpiled) => {
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
