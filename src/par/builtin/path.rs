use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use arcstr::{literal, Substr};
use byteview::ByteView;
use tokio::{fs::File, io::AsyncReadExt};

pub fn external_module() -> Module<std::sync::Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "FromString",
                Type::function(Type::string(), Type::name(None, "Path", vec![])),
                |handle| Box::pin(path_from_string(handle)),
            ),
            Definition::external(
                "FromBytes",
                Type::function(Type::bytes(), Type::name(None, "Path", vec![])),
                |handle| Box::pin(path_from_bytes(handle)),
            ),
        ],
    }
}

async fn path_from_string(mut handle: Handle) {
    let s = handle.receive().string().await;
    let p = PathBuf::from(s.as_str());
    provide_path(handle, p);
}

async fn path_from_bytes(mut handle: Handle) {
    let b = handle.receive().bytes().await;
    // Unsafe: we accept arbitrary OS-encoded bytes without validation
    let os: &OsStr = unsafe { OsStr::from_encoded_bytes_unchecked(b.as_ref()) };
    let p = PathBuf::from(os);
    provide_path(handle, p);
}

pub fn provide_path(handle: Handle, path: PathBuf) {
    handle.provide_box(move |mut handle| {
        let path = path.clone();
        async move {
            match handle.case().await.as_str() {
                "stringName" => {
                    let s = path
                        .file_name()
                        .map(|n| n.to_string_lossy().to_string())
                        .unwrap_or_else(|| "".to_string());
                    handle.provide_string(Substr::from(s));
                }
                "bytesName" => {
                    let bytes = path
                        .file_name()
                        .map(|n| os_to_bytes(n))
                        .unwrap_or_else(|| ByteView::from(&b""[..]));
                    handle.provide_bytes(bytes);
                }
                "stringAbsolute" => {
                    let abs = absolute_path(&path);
                    handle.provide_string(Substr::from(abs.to_string_lossy()));
                }
                "bytesAbsolute" => {
                    let abs = absolute_path(&path);
                    let bytes = os_to_bytes(abs.as_os_str());
                    handle.provide_bytes(bytes);
                }
                "stringParts" => {
                    provide_string_parts(handle, &path);
                }
                "bytesParts" => {
                    provide_bytes_parts(handle, &path);
                }
                "parent" => match path.parent() {
                    Some(p) => {
                        handle.signal(arcstr::literal!("ok"));
                        provide_path(handle, p.to_path_buf());
                    }
                    None => {
                        handle.signal(arcstr::literal!("err"));
                        handle.break_();
                    }
                },
                "appendString" => {
                    let s = handle.receive().string().await;
                    let p2 = path.join(s.as_str());
                    provide_path(handle, p2);
                }
                "appendBytes" => {
                    let b = handle.receive().bytes().await;
                    let os: &OsStr = unsafe { OsStr::from_encoded_bytes_unchecked(b.as_ref()) };
                    let p2 = path.join(Path::new(os));
                    provide_path(handle, p2);
                }
                "openFile" => match File::open(&path).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        return provide_bytes_reader(handle, file).await;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },
                _ => unreachable!(),
            }
        }
    })
}

fn absolute_path(p: &Path) -> PathBuf {
    match p.canonicalize() {
        Ok(abs) => abs,
        Err(_) => {
            if p.is_absolute() {
                p.to_path_buf()
            } else {
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("."))
                    .join(p)
            }
        }
    }
}

fn provide_string_parts(mut handle: Handle, p: &Path) {
    for part in p.iter() {
        handle.signal(arcstr::literal!("item"));
        let s = part.to_string_lossy();
        handle.send().provide_string(Substr::from(s));
    }
    handle.signal(arcstr::literal!("end"));
    handle.break_();
}

fn provide_bytes_parts(mut handle: Handle, p: &Path) {
    for part in p.iter() {
        handle.signal(arcstr::literal!("item"));
        let bytes = os_to_bytes(part);
        handle.send().provide_bytes(bytes);
    }
    handle.signal(arcstr::literal!("end"));
    handle.break_();
}

#[cfg(unix)]
fn os_to_bytes(os: &OsStr) -> ByteView {
    use std::os::unix::ffi::OsStrExt;
    ByteView::from(os.as_bytes())
}

#[cfg(windows)]
fn os_to_bytes(os: &OsStr) -> ByteView {
    use std::os::windows::ffi::OsStrExt;
    let wide: Vec<u16> = os.encode_wide().collect();
    let mut bytes = Vec::with_capacity(wide.len() * 2);
    for w in wide {
        bytes.push((w & 0xFF) as u8);
        bytes.push((w >> 8) as u8);
    }
    ByteView::from(bytes)
}

#[cfg(not(any(unix, windows)))]
fn os_to_bytes(os: &OsStr) -> ByteView {
    ByteView::from(os.to_string_lossy().as_ref())
}

async fn provide_bytes_reader(mut handle: Handle, mut file: File) {
    let mut buf = vec![0u8; 512];
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.receive().concurrently(|mut handle| async move {
                    match handle.case().await.as_str() {
                        "ok" => handle.continue_(),
                        _ => unreachable!(),
                    }
                });
                handle.signal(literal!("ok"));
                return handle.break_();
            }
            "read" => match file.read(&mut buf[..]).await {
                Ok(n) => {
                    if n == 0 {
                        handle.signal(literal!("ok"));
                        handle.signal(literal!("end"));
                        return handle.break_();
                    }
                    handle.signal(literal!("ok"));
                    handle.signal(literal!("chunk"));
                    handle.send().provide_bytes(ByteView::from(&buf[..n]));
                    continue;
                }
                Err(err) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(err.to_string()));
                }
            },
            _ => unreachable!(),
        }
    }
}
