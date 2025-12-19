use futures::{future::RemoteHandle, task::SpawnExt};

use crate::{
    par::{language::GlobalName, types::Type},
    runtime::{
        new::{self, transpiler::Transpiled},
        old::{self, compiler::IcCompiled},
        readback::Handle,
    },
    spawn::TokioSpawn,
};
use std::{collections::HashMap, time::Duration};

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
            backend: if use_new {
                Backend::New(Transpiled::compile_file(module)?)
            } else {
                Backend::Old(IcCompiled::compile_file(module)?)
            },
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
    ) -> (Handle, RemoteHandle<RunResult>) {
        match &self.backend {
            Backend::Old(ic_compiled) => {
                let mut net = ic_compiled.create_net();
                let child_net = ic_compiled.get_with_name(name).unwrap();
                let tree = net.inject_net(child_net);
                let spawner = Arc::new(TokioSpawn::new());
                let (net_wrapper, reducer_future) = net.start_reducer(spawner.clone());
                let net_2 = net_wrapper.clone().net();
                (
                    Handle::Old(net_wrapper.new_handle(tree)),
                    spawner
                        .spawn_with_handle(async move {
                            reducer_future.await;
                            RunResult::Old(net_2.lock().unwrap().rewrites.clone())
                        })
                        .unwrap(),
                )
            }
            Backend::New(transpiled) => {
                let mut reducer = transpiled.new_reducer();
                let net_handle = reducer.net_handle().clone();
                let spawner = reducer.spawner();
                let reducer_future = reducer.spawn_reducer();
                (
                    Handle::New(transpiled.instantiate(net_handle, name).await.unwrap()),
                    spawner
                        .spawn_with_handle(async move {
                            let reducer = reducer_future.await;
                            RunResult::New(reducer.runtime.rewrites)
                        })
                        .unwrap(),
                )
            }
        }
    }
}

pub enum RunResult {
    Old(old::net::Rewrites),
    New(new::stats::Rewrites),
}

impl RunResult {
    pub fn show(&self, elapsed: Duration) -> String {
        match self {
            RunResult::Old(rewrites) => {
                use std::fmt::Write;
                let mut s = String::new();
                writeln!(&mut s, "\tAnnihilate: {}", rewrites.annihilate).unwrap();
                writeln!(&mut s, "\tCommute: {}", rewrites.commute).unwrap();
                writeln!(&mut s, "\tSignal: {}", rewrites.signal).unwrap();
                writeln!(&mut s, "\tErase: {}", rewrites.era).unwrap();
                writeln!(&mut s, "\tExpand: {}", rewrites.expand).unwrap();
                writeln!(&mut s, "\tResponds: {}", rewrites.resp).unwrap();
                writeln!(&mut s, "\tTotal: {}", rewrites.total()).unwrap();
                writeln!(
                    &mut s,
                    "\tBusy time (ms): {}",
                    rewrites.busy_duration.as_millis()
                )
                .unwrap();
                writeln!(&mut s, "\tTotal / s: {}", rewrites.total_per_second()).unwrap();
                s
            }
            RunResult::New(rewrites) => rewrites.show(elapsed),
        }
    }
}
