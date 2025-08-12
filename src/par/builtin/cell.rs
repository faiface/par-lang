use std::{future::Future, sync::Arc};

use tokio::sync::Mutex;

use crate::{
    icombs::readback::Handle,
    location::Span,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
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
                    Type::name(None, "Cell", vec![Type::var("a")]).dual(Span::None),
                    Type::choice(vec![(
                        "put",
                        Type::function(Type::var("a"), Type::var("a")),
                    )]),
                ),
            ),
            |handle| Box::pin(cell_share(handle)),
        )],
    }
}

async fn cell_share(mut handle: Handle) {
    let sharing = handle.send();

    match handle.case().await.as_str() {
        "put" => {}
        _ => unreachable!(),
    }
    let initial_value = handle.receive();

    let mutex = Arc::new(Mutex::new(Cell {
        shared: Some(initial_value),
        finally: Some(handle),
    }));

    provide_cell(sharing, mutex).await
}

fn provide_cell(mut handle: Handle, mutex: Arc<Mutex<Cell>>) -> impl Send + Future<Output = ()> {
    async move {
        loop {
            match handle.case().await.as_str() {
                "end" => break handle.continue_(),

                "split" => {
                    let mutex = Arc::clone(&mutex);
                    handle
                        .receive()
                        .concurrently(move |handle| provide_cell(handle, mutex));
                }

                "take" => {
                    let mut locked = mutex.lock().await;
                    let current_value = locked.shared.take().unwrap();
                    handle.send().link(current_value);
                    match handle.case().await.as_str() {
                        "put" => {
                            let new_value = handle.receive();
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
}

struct Cell {
    shared: Option<Handle>,
    finally: Option<Handle>,
}

impl Drop for Cell {
    fn drop(&mut self) {
        let value = self.shared.take().unwrap();
        let handle = self.finally.take().unwrap();
        handle.link(value)
    }
}
