use std::future::IntoFuture;

use futures::task::Spawn;

pub struct TokioSpawn {
    tokio: tokio::runtime::Handle,
}

impl TokioSpawn {
    pub fn new() -> Self {
        Self {
            tokio: tokio::runtime::Handle::current(),
        }
    }

    #[cfg(feature = "playground")]
    pub fn from_handle(handle: tokio::runtime::Handle) -> Self {
        Self { tokio: handle }
    }
}

impl Spawn for TokioSpawn {
    fn spawn_obj(
        &self,
        future: futures::task::FutureObj<'static, ()>,
    ) -> Result<(), futures::task::SpawnError> {
        drop(self.tokio.spawn(future.into_future()));
        Ok(())
    }
}
