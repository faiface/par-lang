use std::{fs::Metadata, future::Future, path::PathBuf, sync::Arc};

use arcstr::{literal, Substr};
use num_bigint::BigInt;
use tokio::fs::{self, File, ReadDir};

use crate::{
    icombs::readback::Handle,
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
            "Open",
            Type::function(
                Type::name(None, "Path", vec![]),
                Type::name(None, "OpenResult", vec![]),
            ),
            |handle| Box::pin(storage_open(handle)),
        )],
    }
}

async fn storage_open(mut handle: Handle) {
    let path = PathBuf::from(handle.receive().string().await.as_str());
    let meta = match fs::metadata(&path).await {
        Ok(meta) => meta,
        Err(error) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(error.to_string()));
        }
    };
    handle_open_result(path, meta, handle).await
}

fn handle_open_result(
    path: PathBuf,
    meta: Metadata,
    mut handle: Handle,
) -> impl Send + Future<Output = ()> {
    async move {
        let path = fs::canonicalize(&path).await.unwrap_or(path);

        if meta.is_file() {
            let file = match File::open(&path).await {
                Ok(file) => file,
                Err(error) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(error.to_string()));
                }
            };
            handle.signal(literal!("file"));
            return handle_file_info(
                Substr::from(path.to_string_lossy()),
                BigInt::from(meta.len()),
                file,
                handle,
            )
            .await;
        }

        if meta.is_dir() {
            let dir = match fs::read_dir(&path).await {
                Ok(dir) => dir,
                Err(error) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(error.to_string()));
                }
            };
            handle.signal(literal!("directory"));
            return handle_directory_info(Substr::from(path.to_string_lossy()), dir, handle).await;
        }

        handle.signal(literal!("err"));
        handle.provide_string(Substr::from("unsupported storage item type"));
    }
}

async fn handle_file_info(path: Substr, size: BigInt, _file: File, mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                return;
            }
            "getPath" => {
                handle.send().provide_string(path.clone());
            }
            "getSize" => {
                handle.send().provide_nat(size.clone());
            }
            "readUTF8" => {
                todo!("implement")
            }
            _ => unreachable!(),
        }
    }
}

async fn handle_directory_info(path: Substr, mut dir: ReadDir, mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
            }
            "getPath" => {
                handle.send().provide_string(path.clone());
            }
            "list" => {
                while let Ok(Some(entry)) = dir.next_entry().await {
                    let Ok(meta) = entry.metadata().await else {
                        continue;
                    };

                    handle.signal(literal!("item"));
                    handle.send().concurrently(|mut handle| async move {
                        let path = Substr::from(entry.path().to_string_lossy());
                        handle.send().provide_string(path);
                        match handle.case().await.as_str() {
                            "open" => handle_open_result(entry.path(), meta, handle).await,
                            "skip" => {
                                handle.break_();
                                return;
                            }
                            _ => unreachable!(),
                        }
                    });
                }
                handle.signal(literal!("end"));
                handle.break_();
                return;
            }
            _ => unreachable!(),
        }
    }
}
