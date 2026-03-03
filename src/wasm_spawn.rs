use std::future::IntoFuture;

use futures::task::Spawn;
use wasm_bindgen_futures::spawn_local;
pub struct WasmSpawn {}

impl WasmSpawn {
    pub fn new() -> Self {
        Self {}
    }
}

impl Spawn for WasmSpawn {
    fn spawn_obj(
        &self,
        future: futures::task::FutureObj<'static, ()>,
    ) -> Result<(), futures::task::SpawnError> {
        spawn_local(future);
        Ok(())
    }
}
