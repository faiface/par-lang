use std::{sync::Arc};

use futures::{future::BoxFuture, FutureExt};
use tokio::sync::Mutex;

use crate::{
    location::Span,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
    runtime::Handle,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Share",
            Type::forall(
                "a",
                Type::function(
                    Type::var("a"),
                    Type::function(
                        Type::name(None, "Cell", vec![Type::var("a")]).dual(Span::None),
                        Type::var("a"),
                    ),
                ),
            ),
            |handle| Box::pin(cell_share(handle)),
        )],
    }
}

async fn cell_share(mut handle: Handle) {
    let initial_value = handle.receive().await;
    let sharing = handle.send().await;

    let mutex = Arc::new(Mutex::new(Cell {
        shared: Some(initial_value),
        finally: Some(handle),
    }));

    provide_cell(sharing, mutex).await
}

fn provide_cell(mut handle: Handle, mutex: Arc<Mutex<Cell>>) -> BoxFuture<'static, ()> {
    async move {
        loop {
            match handle.case().await.as_str() {
                "end" => break handle.continue_().await,

                "split" => {
                    let mutex = Arc::clone(&mutex);
                    handle
                        .receive()
                        .await
                        .concurrently(move |handle| provide_cell(handle, mutex));
                }

                "take" => {
                    let mut locked = mutex.lock().await;
                    let current_value = locked.shared.take().unwrap();
                    handle.send().await.link(current_value).await;
                    match handle.case().await.as_str() {
                        "put" => {
                            let new_value = handle.receive().await;
                            locked.shared = Some(new_value);
                            drop(locked)
                        }
                        _ => unreachable!(),
                    }
                }

                _ => unreachable!(),
            }
        }
    }
    .boxed()
}

struct Cell {
    shared: Option<Handle>,
    finally: Option<Handle>,
}

impl Drop for Cell {
    fn drop(&mut self) {
        let value = self.shared.take().unwrap();
        let handle = self.finally.take().unwrap();
        handle.concurrently(move |x| x.link(value))
    }
}
