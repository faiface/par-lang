use std::{ffi::OsStr, fs::Metadata, path::PathBuf, sync::Arc};

use arcstr::{literal, Substr};
use num_bigint::BigInt;
use tokio::fs;

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
            "Get",
            Type::function(
                Type::string(),
                Type::name(
                    None,
                    "Result",
                    vec![Type::either(vec![
                        ("file", Type::name(None, "FileInfo", vec![])),
                        ("dir", Type::name(None, "DirInfo", vec![])),
                    ])],
                ),
            ),
            |handle| Box::pin(storage_open(handle)),
        )],
    }
}

async fn storage_open(mut handle: Handle) {
    let path = PathBuf::from(handle.receive().string().await.as_str());
    let metadata = match fs::metadata(path.clone()).await {
        Ok(metadata) => metadata,
        Err(err) => {
            handle.signal(literal!("err"));
            handle.provide_string(Substr::from(err.to_string()));
            return;
        }
    };
    if metadata.is_file() {
        handle.signal(literal!("ok"));
        handle.signal(literal!("file"));
        provide_file_info(
            handle,
            FileInfo {
                path: Arc::new(path),
                metadata,
            },
        );
    } else if metadata.is_dir() {
        handle.signal(literal!("ok"));
        handle.signal(literal!("dir"));
        provide_dir_info(
            handle,
            DirInfo {
                path: Arc::new(path),
            },
        );
    } else {
        handle.signal(literal!("err"));
        handle.provide_string(Substr::from("unsupported file type"));
    }
}

#[derive(Clone)]
struct DirInfo {
    path: Arc<PathBuf>,
}

#[derive(Clone)]
struct FileInfo {
    path: Arc<PathBuf>,
    metadata: Metadata,
}

fn provide_path_info(handle: Handle, path: Arc<PathBuf>) {
    let path = path.clone().canonicalize().map(Arc::new).unwrap_or(path);
    handle.provide_box(move |mut handle| {
        let path = Arc::clone(&path);
        async move {
            match handle.case().await.as_str() {
                "name" => handle.provide_string(Substr::from(
                    path.file_name()
                        .map(OsStr::to_string_lossy)
                        .unwrap_or(std::borrow::Cow::Borrowed("")),
                )),
                "absolute" => handle.provide_string(Substr::from(path.to_string_lossy())),
                _ => unreachable!(),
            }
        }
    })
}

fn provide_dir_info(handle: Handle, info: DirInfo) {
    handle.provide_box(move |mut handle| {
        let info = info.clone();
        async move {
            match handle.case().await.as_str() {
                "path" => provide_path_info(handle, info.path),

                "list" => {
                    let mut dir = match fs::read_dir(info.path.as_ref()).await {
                        Ok(dir) => dir,
                        Err(err) => {
                            handle.signal(literal!("err"));
                            handle.provide_string(Substr::from(err.to_string()));
                            return;
                        }
                    };
                    handle.signal(literal!("ok"));
                    while let Ok(Some(entry)) = dir.next_entry().await {
                        if let Ok(metadata) = entry.metadata().await {
                            if metadata.is_file() {
                                handle.signal(literal!("item"));
                                handle.send().concurrently(|mut handle| async move {
                                    handle.signal(literal!("file"));
                                    provide_file_info(
                                        handle,
                                        FileInfo {
                                            path: Arc::new(entry.path()),
                                            metadata,
                                        },
                                    )
                                })
                            } else if metadata.is_dir() {
                                handle.signal(literal!("item"));
                                handle.send().concurrently(|mut handle| async move {
                                    handle.signal(literal!("dir"));
                                    provide_dir_info(
                                        handle,
                                        DirInfo {
                                            path: Arc::new(entry.path()),
                                        },
                                    )
                                });
                            }
                        }
                    }
                    handle.signal(literal!("end"));
                    handle.break_();
                }

                _ => unreachable!(),
            }
        }
    })
}

fn provide_file_info(handle: Handle, info: FileInfo) {
    handle.provide_box(move |mut handle| {
        let info = info.clone();
        async move {
            match handle.case().await.as_str() {
                "path" => provide_path_info(handle, info.path),

                "size" => handle.provide_nat(BigInt::from(info.metadata.len())),

                _ => unreachable!(),
            }
        }
    })
}
