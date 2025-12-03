use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

use crate::{
    icombs::readback::Handle,
    par::{
        primitive::ParString,
        process,
        program::{Definition, Module},
        types::Type,
    },
};
use arcstr::literal;
use bytes::Bytes;
use futures::future::BoxFuture;
use num_bigint::BigInt;
use std::sync::Arc;
use tokio::sync::Mutex as TokioMutex;
use tokio::{
    fs::{self, DirEntry, File, OpenOptions, ReadDir},
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command},
};

pub fn external_module() -> Module<std::sync::Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Path",
                Type::function(Type::bytes(), Type::name(None, "Path", vec![])),
                |handle| Box::pin(path_from_bytes(handle)),
            ),
            Definition::external("Stdin", Type::name(None, "Reader", vec![]), |handle| {
                Box::pin(os_stdin(handle))
            }),
            Definition::external("Stdout", Type::name(None, "Writer", vec![]), |handle| {
                Box::pin(os_stdout(handle))
            }),
            Definition::external("Stderr", Type::name(None, "Writer", vec![]), |handle| {
                Box::pin(os_stderr(handle))
            }),
            Definition::external(
                "OpenFile",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Reader", vec![])),
                    ]),
                ),
                |handle| Box::pin(os_open_file(handle)),
            ),
            Definition::external(
                "CreateOrReplaceFile",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Writer", vec![])),
                    ]),
                ),
                |handle| Box::pin(os_create_or_replace_file(handle)),
            ),
            Definition::external(
                "CreateNewFile",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Writer", vec![])),
                    ]),
                ),
                |handle| Box::pin(os_create_new_file(handle)),
            ),
            Definition::external(
                "AppendToFile",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Writer", vec![])),
                    ]),
                ),
                |handle| Box::pin(os_append_to_file(handle)),
            ),
            Definition::external(
                "CreateOrAppendToFile",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::name(None, "Writer", vec![])),
                    ]),
                ),
                |handle| Box::pin(os_create_or_append_to_file(handle)),
            ),
            Definition::external(
                "CreateDir",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        ("ok", Type::break_()),
                    ]),
                ),
                |handle| Box::pin(os_create_dir(handle)),
            ),
            Definition::external(
                "ListDir",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        (
                            "ok",
                            Type::name(
                                Some("List"),
                                "List",
                                vec![Type::name(None, "Path", vec![])],
                            ),
                        ),
                    ]),
                ),
                |handle| Box::pin(os_list_dir(handle)),
            ),
            Definition::external(
                "TraverseDir",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::either(vec![
                        ("err", Type::name(None, "Error", vec![])),
                        (
                            "ok",
                            Type::recursive(
                                None,
                                Type::either(vec![
                                    ("end", Type::break_()),
                                    (
                                        "file",
                                        Type::pair(
                                            Type::name(None, "Path", vec![]),
                                            Type::self_(None),
                                        ),
                                    ),
                                    (
                                        "dir",
                                        Type::pair(
                                            Type::name(None, "Path", vec![]),
                                            Type::pair(Type::self_(None), Type::self_(None)),
                                        ),
                                    ),
                                ]),
                            ),
                        ),
                    ]),
                ),
                |handle| Box::pin(os_traverse_dir(handle)),
            ),
            Definition::external(
                "Exec",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::function(
                        Type::name(Some("List"), "List", vec![Type::string()]),
                        Type::either(vec![
                            ("err", Type::name(None, "Error", vec![])),
                            ("ok", Type::name(None, "ExecHandle", vec![])),
                        ]),
                    ),
                ),
                |handle| Box::pin(os_exec(handle)),
            ),
        ],
    }
}

async fn path_from_bytes(mut handle: Handle) {
    let b = handle.receive().bytes().await;
    // Use proper platform-specific conversion instead of unsafe unchecked
    // On Windows, bytes_to_os expects UTF-16, but strings from Par are UTF-8
    // So we need to try UTF-8 first, then fall back to bytes_to_os if that fails
    let os_string = if let Ok(utf8_str) = String::from_utf8(b.as_ref().to_vec()) {
        OsString::from(utf8_str)
    } else {
        bytes_to_os(b.as_ref())
            .unwrap_or_else(|_| OsString::from(String::from_utf8_lossy(b.as_ref()).into_owned()))
    };
    let p = PathBuf::from(os_string);
    provide_path(handle, p);
}

pub fn provide_path(handle: Handle, path: PathBuf) {
    handle.provide_box(move |mut handle| {
        let path = path.clone();
        async move {
            match handle.case().await.as_str() {
                "name" => {
                    let bytes = path
                        .file_name()
                        .map(|n| os_to_bytes(n))
                        .unwrap_or_else(|| Bytes::new());
                    handle.provide_bytes(bytes);
                }
                "absolute" => {
                    let abs = absolute_path(&path);
                    let bytes = os_to_bytes(abs.as_os_str());
                    handle.provide_bytes(bytes);
                }
                "parts" => {
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
                "append" => {
                    let b = handle.receive().bytes().await;
                    // Use proper platform-specific conversion instead of unsafe unchecked
                    let os_string = bytes_to_os(b.as_ref()).unwrap_or_else(|_| {
                        OsString::from(String::from_utf8_lossy(b.as_ref()).into_owned())
                    });
                    let os: &OsStr = os_string.as_os_str();
                    let p2 = path.join(Path::new(os));
                    provide_path(handle, p2);
                }
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
fn os_to_bytes(os: &OsStr) -> Bytes {
    use std::os::unix::ffi::OsStrExt;
    Bytes::copy_from_slice(os.as_bytes())
}

#[cfg(windows)]
fn os_to_bytes(os: &OsStr) -> Bytes {
    use std::os::windows::ffi::OsStrExt;
    let wide: Vec<u16> = os.encode_wide().collect();
    let mut bytes = Vec::with_capacity(wide.len() * 2);
    for w in wide {
        bytes.push((w & 0xFF) as u8);
        bytes.push((w >> 8) as u8);
    }
    Bytes::from(bytes)
}

#[cfg(not(any(unix, windows)))]
fn os_to_bytes(os: &OsStr) -> Bytes {
    Bytes::from(os.to_string_lossy().as_ref())
}

/// Convert bytes back to OsString, handling platform-specific encodings.
/// This is the inverse of `os_to_bytes`.
#[cfg(windows)]
fn bytes_to_os(bytes: &[u8]) -> Result<OsString, String> {
    use std::os::windows::ffi::OsStringExt;

    // Windows: bytes are UTF-16 little-endian (from os_to_bytes)
    if bytes.len() % 2 != 0 {
        return Err("Invalid UTF-16: odd number of bytes".to_string());
    }

    // Convert little-endian byte pairs to u16
    let wide: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
        .collect();

    // Remove trailing NUL if present (Windows paths shouldn't have embedded NULs)
    // This handles the case where os_to_bytes might have added a trailing NUL
    let wide: Vec<u16> = wide.into_iter().take_while(|&w| w != 0).collect();

    Ok(OsString::from_wide(&wide))
}

#[cfg(unix)]
fn bytes_to_os(bytes: &[u8]) -> Result<OsString, String> {
    use std::os::unix::ffi::OsStrExt;
    // Unix: bytes are already in the correct format (UTF-8 or arbitrary bytes)
    Ok(OsString::from_vec(bytes.to_vec()))
}

#[cfg(not(any(windows, unix)))]
fn bytes_to_os(bytes: &[u8]) -> Result<OsString, String> {
    // Fallback: lossy conversion for unknown platforms
    Ok(OsString::from(String::from_utf8_lossy(bytes)))
}

async fn provide_bytes_reader_from_async(mut handle: Handle, mut reader: impl AsyncRead + Unpin) {
    let mut buf = vec![0u8; 512];
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.signal(literal!("ok"));
                return handle.break_();
            }
            "read" => match reader.read(&mut buf[..]).await {
                Ok(n) => {
                    if n == 0 {
                        handle.signal(literal!("ok"));
                        handle.signal(literal!("end"));
                        return handle.break_();
                    }
                    handle.signal(literal!("ok"));
                    handle.signal(literal!("chunk"));
                    handle
                        .send()
                        .provide_bytes(Bytes::copy_from_slice(&buf[..n]));
                    continue;
                }
                Err(err) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(ParString::from(err.to_string()));
                }
            },
            _ => unreachable!(),
        }
    }
}

async fn provide_bytes_writer_from_async(mut handle: Handle, mut writer: impl AsyncWrite + Unpin) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                // Try to flush pending data before closing
                match writer.flush().await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(ParString::from(err.to_string()));
                    }
                }
            }
            "flush" => match writer.flush().await {
                Ok(()) => {
                    handle.signal(literal!("ok"));
                    continue;
                }
                Err(err) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(ParString::from(err.to_string()));
                }
            },
            "write" => {
                let bytes = handle.receive().bytes().await;
                match writer.write_all(bytes.as_ref()).await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        continue;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(ParString::from(err.to_string()));
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

// Provide List<Os.Path> for the directory entries of `base` using a pre-opened ReadDir.
async fn provide_list_dir(mut handle: Handle, base: &Path, rd: &mut ReadDir) {
    let mut entries: Vec<(Bytes, std::ffi::OsString)> = Vec::new();
    while let Ok(Some(entry)) = rd.next_entry().await {
        let name = entry.file_name();
        // Sort key: raw bytes if available, fallback to lossy string
        let key = os_to_bytes(&name);
        entries.push((key, name));
    }
    // Sort deterministically by the byte-representation of file name
    entries.sort_by(|(a, _), (b, _)| a.as_ref().cmp(b.as_ref()));

    for (_, name) in entries {
        let child = base.join(Path::new(&name));
        handle.signal(literal!("item"));
        provide_path(handle.send(), child);
    }
    handle.signal(literal!("end"));
    handle.break_();
}

// Directory tree node used for traverseDir
enum DirNode {
    File(PathBuf),
    Dir {
        path: PathBuf,
        children: Vec<DirNode>,
    },
}

// Recursively build the full directory tree. Returns an error message if any IO fails.
fn build_dir_tree(dir: PathBuf) -> BoxFuture<'static, Result<Vec<DirNode>, String>> {
    Box::pin(async move {
        let mut rd = fs::read_dir(&dir).await.map_err(|e| format!("{}", e))?;

        // Collect entries first to allow deterministic sorting
        let mut items: Vec<(Bytes, DirEntry)> = Vec::new();
        while let Ok(Some(entry)) = rd.next_entry().await {
            let name = entry.file_name();
            let key = os_to_bytes(&name);
            items.push((key, entry));
        }
        items.sort_by(|(a, _), (b, _)| a.as_ref().cmp(b.as_ref()));

        let mut result = Vec::new();
        for (_, entry) in items {
            let ty = entry.file_type().await.map_err(|e| format!("{}", e))?;
            let child_path = entry.path();
            if ty.is_dir() {
                let children = build_dir_tree(child_path.clone()).await?;
                result.push(DirNode::Dir {
                    path: child_path,
                    children,
                });
            } else {
                // Treat symlinks and others as files to avoid cycles
                result.push(DirNode::File(child_path));
            }
        }
        Ok(result)
    })
}

fn provide_dir_tree(mut handle: Handle, nodes: &[DirNode]) {
    match nodes.split_first() {
        None => {
            handle.signal(literal!("end"));
            handle.break_();
        }
        Some((node, tail)) => match node {
            DirNode::File(path) => {
                handle.signal(literal!("file"));
                provide_path(handle.send(), path.clone());
                provide_dir_tree(handle, tail);
            }
            DirNode::Dir { path, children } => {
                handle.signal(literal!("dir"));
                provide_path(handle.send(), path.clone());
                provide_dir_tree(handle.send(), children.as_slice());
                provide_dir_tree(handle, tail);
            }
        },
    }
}

async fn os_stdin(handle: Handle) {
    provide_bytes_reader_from_async(handle, tokio::io::stdin()).await;
}

async fn os_stdout(handle: Handle) {
    provide_bytes_writer_from_async(handle, tokio::io::stdout()).await;
}

async fn os_stderr(handle: Handle) {
    provide_bytes_writer_from_async(handle, tokio::io::stderr()).await;
}

async fn os_open_file(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match File::open(&path).await {
        Ok(file) => {
            handle.signal(literal!("ok"));
            return provide_bytes_reader_from_async(handle, file).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_create_or_replace_file(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
        .await
    {
        Ok(file) => {
            handle.signal(literal!("ok"));
            return provide_bytes_writer_from_async(handle, file).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_create_new_file(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&path)
        .await
    {
        Ok(file) => {
            handle.signal(literal!("ok"));
            return provide_bytes_writer_from_async(handle, file).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_append_to_file(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match OpenOptions::new()
        .write(true)
        .append(true)
        .open(&path)
        .await
    {
        Ok(file) => {
            handle.signal(literal!("ok"));
            return provide_bytes_writer_from_async(handle, file).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_create_or_append_to_file(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match OpenOptions::new()
        .create(true)
        .write(true)
        .append(true)
        .open(&path)
        .await
    {
        Ok(file) => {
            handle.signal(literal!("ok"));
            return provide_bytes_writer_from_async(handle, file).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_create_dir(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match fs::create_dir_all(&path).await {
        Ok(()) => {
            handle.signal(literal!("ok"));
            return handle.break_();
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_list_dir(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match fs::read_dir(&path).await {
        Ok(mut rd) => {
            handle.signal(literal!("ok"));
            return provide_list_dir(handle, &path, &mut rd).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

async fn os_traverse_dir(mut handle: Handle) {
    let path = pathbuf_from_os_path(handle.receive()).await;
    match build_dir_tree(path.clone()).await {
        Ok(nodes) => {
            handle.signal(literal!("ok"));
            return provide_dir_tree(handle, nodes.as_slice());
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(ParString::from(err));
        }
    }
}

async fn pathbuf_from_os_path(mut handle: Handle) -> PathBuf {
    handle.signal(literal!("absolute"));
    let path_bytes = handle.bytes().await;
    // Use proper platform-specific conversion instead of unsafe unchecked
    // The bytes from "absolute" are already in UTF-16 (Windows) or UTF-8 (Unix) format
    // from os_to_bytes, so we need to decode them correctly
    let os_string = bytes_to_os(&path_bytes)
        .unwrap_or_else(|_| OsString::from(String::from_utf8_lossy(&path_bytes).into_owned()));
    PathBuf::from(os_string)
}

// Helper to read List<String> from handle
async fn read_string_list(mut handle: Handle) -> Vec<String> {
    let mut args = Vec::new();
    loop {
        match handle.case().await.as_str() {
            "end" => break,
            "item" => {
                let arg = handle.receive().string().await;
                args.push(arg.as_str().to_string());
            }
            _ => unreachable!(),
        }
    }
    args
}

// Shared state for ExecHandle
struct ExecHandleState {
    child: Option<Child>,
    stdin: Option<ChildStdin>,
    stdout: Option<ChildStdout>,
    stderr: Option<ChildStderr>,
}

// Resolve executable path, searching PATH if needed
fn resolve_executable(path: &Path) -> PathBuf {
    // Check if path exists - if it does, use it as-is
    if path.exists() && path.is_file() {
        return path.to_path_buf();
    }

    // Extract just the filename (without directory)
    // If the path doesn't exist, try to find it in PATH
    if let Some(exe_name) = path.file_name() {
        // On Windows, try with .exe extension if not present
        #[cfg(windows)]
        let exe_names = {
            let exe_str = exe_name.to_string_lossy();
            if !exe_str.ends_with(".exe")
                && !exe_str.ends_with(".bat")
                && !exe_str.ends_with(".cmd")
            {
                vec![
                    OsString::from(format!("{}.exe", exe_str)),
                    exe_name.to_os_string(), // Also try without extension
                ]
            } else {
                vec![exe_name.to_os_string()]
            }
        };

        #[cfg(not(windows))]
        let exe_names = vec![exe_name.to_os_string()];

        // Search PATH environment variable
        // Use std::env::var_os to get OsString directly (handles non-UTF8 paths better)
        if let Some(path_env) = std::env::var_os("PATH") {
            for dir in std::env::split_paths(&path_env) {
                for exe_name in &exe_names {
                    let candidate = dir.join(exe_name);
                    if candidate.exists() && candidate.is_file() {
                        return candidate;
                    }
                }
            }
        }
    }

    // If not found in PATH, return original path (will fail with clear error)
    path.to_path_buf()
}

// Execute external process
async fn os_exec(mut handle: Handle) {
    // Receive path
    let path = pathbuf_from_os_path(handle.receive()).await;

    // Resolve executable path (search PATH if needed)
    let resolved_path = resolve_executable(&path);

    // Receive list of arguments
    let args = read_string_list(handle.receive()).await;

    // Build command
    let mut cmd = Command::new(&resolved_path);
    cmd.args(&args);
    // Set working directory to user's current directory (where par was run from)
    // This preserves relative path resolution in arguments, while the executable path
    // itself is already resolved to an absolute path via resolve_executable().
    if let Ok(current_dir) = std::env::current_dir() {
        cmd.current_dir(current_dir);
    }
    cmd.stdin(std::process::Stdio::piped());
    cmd.stdout(std::process::Stdio::piped());
    cmd.stderr(std::process::Stdio::piped());

    // Spawn process
    match cmd.spawn() {
        Ok(mut child) => {
            let state = Arc::new(TokioMutex::new(ExecHandleState {
                stdin: child.stdin.take(),
                stdout: child.stdout.take(),
                stderr: child.stderr.take(),
                child: Some(child),
            }));

            handle.signal(literal!("ok"));
            provide_exec_handle(handle, state).await;
        }
        Err(err) => {
            handle.signal(literal!("err"));
            handle.provide_string(ParString::from(err.to_string()));
        }
    }
}

// Provide ExecHandle operations
async fn provide_exec_handle(mut handle: Handle, state: Arc<TokioMutex<ExecHandleState>>) {
    loop {
        match handle.case().await.as_str() {
            "wait" => {
                let mut locked = state.lock().await;
                // Drop any remaining handles
                drop(locked.stdin.take());
                drop(locked.stdout.take());
                drop(locked.stderr.take());
                // Extract child from state
                if let Some(mut child) = locked.child.take() {
                    drop(locked);

                    match child.wait().await {
                        Ok(status) => {
                            let exit_code = status.code().unwrap_or(0);
                            handle.signal(literal!("ok"));
                            handle.provide_nat(BigInt::from(exit_code));
                            return;
                        }
                        Err(err) => {
                            handle.signal(literal!("err"));
                            handle.provide_string(ParString::from(err.to_string()));
                            return;
                        }
                    }
                } else {
                    drop(locked);
                    handle.signal(literal!("err"));
                    handle.provide_string(ParString::from("process already waited".to_string()));
                    return;
                }
            }
            "kill" => {
                let mut locked = state.lock().await;
                if let Some(mut child) = locked.child.take() {
                    drop(locked);

                    if let Err(err) = child.kill().await {
                        handle.signal(literal!("err"));
                        handle.provide_string(ParString::from(err.to_string()));
                        return;
                    }
                    handle.signal(literal!("ok"));
                    return handle.break_();
                } else {
                    drop(locked);
                    handle.signal(literal!("err"));
                    handle
                        .provide_string(ParString::from("process already terminated".to_string()));
                    return;
                }
            }
            "stdin" => {
                let mut locked = state.lock().await;
                if let Some(stdin) = locked.stdin.take() {
                    drop(locked);
                    // Provide writer directly - consume handle
                    provide_bytes_writer_from_async(handle, stdin).await;
                    // Note: Process continues running as long as stdin handle exists
                    return;
                } else {
                    drop(locked);
                    handle.signal(literal!("err"));
                    handle.provide_string(ParString::from("stdin already taken".to_string()));
                    return;
                }
            }
            "stdout" => {
                let mut locked = state.lock().await;
                if let Some(stdout) = locked.stdout.take() {
                    drop(locked);
                    // Provide reader directly - consume handle
                    provide_bytes_reader_from_async(handle, stdout).await;
                    // Note: Process continues running as long as stdout handle exists
                    return;
                } else {
                    drop(locked);
                    handle.signal(literal!("err"));
                    handle.provide_string(ParString::from("stdout already taken".to_string()));
                    return;
                }
            }
            "stderr" => {
                let mut locked = state.lock().await;
                if let Some(stderr) = locked.stderr.take() {
                    drop(locked);
                    // Provide reader directly - consume handle
                    provide_bytes_reader_from_async(handle, stderr).await;
                    // Note: Process continues running as long as stderr handle exists
                    return;
                } else {
                    drop(locked);
                    handle.signal(literal!("err"));
                    handle.provide_string(ParString::from("stderr already taken".to_string()));
                    return;
                }
            }
            // Lock feature reserved for future implementation (see Os.ExecLock in merge request docs)
            _ => unreachable!(),
        }
    }
}

// Provide ExecLock operations
// Reserved for future Os.ExecLock feature (see merge request docs for future work)
// This function is not currently exposed in the Par API but kept for future implementation
#[allow(dead_code)]
async fn provide_exec_lock(mut handle: Handle, lock_state: Arc<TokioMutex<bool>>) {
    // Acquire lock
    {
        let mut locked = lock_state.lock().await;
        if *locked {
            // Lock already held - this is an error in our simple implementation
            // In a more sophisticated version, we might wait or return an error
            handle.signal(literal!("err"));
            handle.provide_string(ParString::from("lock already held".to_string()));
            return;
        }
        *locked = true;
    }

    loop {
        match handle.case().await.as_str() {
            "release" => {
                let mut locked = lock_state.lock().await;
                *locked = false;
                return handle.break_();
            }
            "check" => {
                let locked = lock_state.lock().await;
                let is_held = *locked;
                drop(locked);
                if is_held {
                    handle.signal(literal!("true"));
                } else {
                    handle.signal(literal!("false"));
                }
                // Continue with same lock
                continue;
            }
            _ => unreachable!(),
        }
    }
}
