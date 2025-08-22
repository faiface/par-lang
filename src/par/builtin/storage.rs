use std::{collections::VecDeque, ffi::OsStr, fs::Metadata, path::PathBuf, sync::Arc};

use arcstr::{literal, Substr};
use byteview::ByteView;
use num_bigint::BigInt;
use tokio::{
    fs::{self, File, OpenOptions},
    io::{self, AsyncReadExt, AsyncWriteExt},
};

use crate::{
    icombs::readback::Handle,
    par::{
        builtin::reader::{
            provide_bytes_reader, provide_string_reader, AsyncByteIterator, AsyncCharIterator,
            BytesRemainder, CharsRemainder, Never,
        },
        process,
        program::{Definition, Module},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Get",
                Type::function(
                    Type::string(),
                    Type::name(
                        Some("Result"),
                        "Result",
                        vec![
                            Type::name(None, "Error", vec![]),
                            Type::either(vec![
                                ("file", Type::name(None, "FileInfo", vec![])),
                                ("dir", Type::name(None, "DirInfo", vec![])),
                            ]),
                        ],
                    ),
                ),
                |handle| Box::pin(storage_open(handle)),
            ),
            Definition::external(
                "CreateFile",
                Type::function(
                    Type::string(),
                    Type::name(
                        Some("Result"),
                        "Result",
                        vec![
                            Type::name(None, "Error", vec![]),
                            Type::name(None, "FileInfo", vec![]),
                        ],
                    ),
                ),
                |handle| Box::pin(storage_create_file(PathBuf::new(), handle)),
            ),
            Definition::external(
                "CreateDir",
                Type::function(
                    Type::string(),
                    Type::name(
                        Some("Result"),
                        "Result",
                        vec![
                            Type::name(None, "Error", vec![]),
                            Type::name(None, "DirInfo", vec![]),
                        ],
                    ),
                ),
                |handle| Box::pin(storage_create_dir(PathBuf::new(), handle)),
            ),
        ],
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
        handle.provide_string(Substr::from("Unsupported file type"));
    }
}

async fn storage_create_file(prefix: PathBuf, mut handle: Handle) {
    let mut path = prefix;
    let suffix = PathBuf::from(handle.receive().string().await.as_str());
    path.push(suffix);

    match File::create(&path).await {
        Ok(file) => match file.metadata().await {
            Ok(metadata) => {
                handle.signal(literal!("ok"));
                return provide_file_info(
                    handle,
                    FileInfo {
                        path: Arc::new(path),
                        metadata,
                    },
                );
            }
            Err(err) => {
                handle.signal(literal!("err"));
                return handle.provide_string(Substr::from(err.to_string()));
            }
        },
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(err.to_string()));
        }
    }
}

async fn storage_create_dir(prefix: PathBuf, mut handle: Handle) {
    let mut path = prefix;
    let suffix = PathBuf::from(handle.receive().string().await.as_str());
    path.push(suffix);

    match fs::create_dir(&path).await {
        Ok(()) => {
            handle.signal(literal!("ok"));
            return provide_dir_info(
                handle,
                DirInfo {
                    path: Arc::new(path),
                },
            );
        }
        Err(err) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(err.to_string()));
        }
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

                "createFile" => storage_create_file(info.path.as_ref().clone(), handle).await,
                //"createDir" => storage_create_dir(info.path.as_ref().clone(), handle).await,
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

                "readBytes" => match File::open(info.path.as_ref()).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        return provide_bytes_reader(handle, FileRemainder::new(file)).await;
                    }

                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                "readUTF8" => match File::open(info.path.as_ref()).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        return provide_string_reader(handle, FileRemainder::new(file)).await;
                    }

                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                "overwriteBytes" => match File::create(info.path.as_ref()).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        provide_bytes_writer_for_file(handle, file).await;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                "appendBytes" => match OpenOptions::new()
                    .append(true)
                    .open(info.path.as_ref())
                    .await
                {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        provide_bytes_writer_for_file(handle, file).await;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                "overwriteUTF8" => match File::create(info.path.as_ref()).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        provide_string_writer_for_file(handle, file).await;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                "appendUTF8" => match OpenOptions::new()
                    .append(true)
                    .open(info.path.as_ref())
                    .await
                {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        provide_string_writer_for_file(handle, file).await;
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

async fn provide_bytes_writer_for_file(mut handle: Handle, mut file: File) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.receive().concurrently(|mut handle| async {
                    match handle.case().await.as_str() {
                        "ok" => handle.continue_(),
                        _ => unreachable!(),
                    }
                });
                match file.flush().await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                }
            }

            "write" => {
                let bytes = handle.receive().bytes().await;
                match file.write(&bytes).await {
                    Ok(_) => {
                        handle.signal(literal!("ok"));
                        continue;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}

async fn provide_string_writer_for_file(mut handle: Handle, mut file: File) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.receive().concurrently(|mut handle| async {
                    match handle.case().await.as_str() {
                        "ok" => handle.continue_(),
                        _ => unreachable!(),
                    }
                });
                match file.flush().await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                }
            }

            "write" => {
                let string = handle.receive().string().await;
                match file.write(string.as_bytes()).await {
                    Ok(_) => {
                        handle.signal(literal!("ok"));
                        continue;
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return handle.provide_string(Substr::from(err.to_string()));
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}

struct FileRemainder {
    file: File,
    buffer: VecDeque<u8>,
}

impl FileRemainder {
    fn new(file: File) -> Self {
        Self {
            file,
            buffer: VecDeque::new(),
        }
    }
}

impl BytesRemainder for FileRemainder {
    type ErrIn = Never;
    type ErrOut = io::Error;
    type Iterator<'a>
        = FileRemainderByteIterator<'a>
    where
        Self: 'a;

    async fn read_error_in(_: Handle) -> Self::ErrIn {
        unreachable!()
    }

    async fn provide_error_out(handle: Handle, err_out: Self::ErrOut) {
        handle.provide_string(Substr::from(err_out.to_string()));
    }

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut> {
        match result_in {
            Ok(()) => {}
        }
        Ok(())
    }

    fn bytes(&mut self) -> Self::Iterator<'_> {
        FileRemainderByteIterator {
            remainder: self,
            index: 0,
            tmp: [0; 256],
        }
    }

    fn pop_bytes(&mut self, n: usize) -> ByteView {
        self.buffer.drain(..n).collect()
    }

    async fn remaining_bytes(&mut self) -> Result<ByteView, Self::ErrOut> {
        let mut result = Vec::new();
        let mut bytes = self.bytes();
        while let Some((_, b)) = bytes.next().await? {
            result.push(b);
        }
        Ok(ByteView::from(result))
    }
}

impl CharsRemainder for FileRemainder {
    type ErrIn = Never;
    type ErrOut = io::Error;
    type Iterator<'a>
        = FileRemainderCharIterator<'a>
    where
        Self: 'a;

    async fn read_error_in(_: Handle) -> Self::ErrIn {
        unreachable!()
    }

    async fn provide_error_out(handle: Handle, err_out: Self::ErrOut) {
        handle.provide_string(Substr::from(err_out.to_string()));
    }

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut> {
        match result_in {
            Ok(()) => {}
        }
        Ok(())
    }

    fn chars(&mut self) -> Self::Iterator<'_> {
        FileRemainderCharIterator {
            bytes: FileRemainderByteIterator {
                remainder: self,
                index: 0,
                tmp: [0; 256],
            },
            tmp: Vec::with_capacity(4),
        }
    }

    fn pop_chars(&mut self, n: usize) -> Substr {
        let popped = self.buffer.drain(..n).collect::<Vec<u8>>();
        let popped = String::from_utf8_lossy(&popped[..]);
        Substr::from(popped)
    }

    async fn remaining_chars(&mut self) -> Result<Substr, Self::ErrOut> {
        let mut result = String::new();
        let mut chars = self.chars();
        while let Some((_, _, ch)) = chars.next().await? {
            result.push(ch);
        }
        Ok(Substr::from(result))
    }
}

struct FileRemainderByteIterator<'a> {
    remainder: &'a mut FileRemainder,
    index: usize,
    tmp: [u8; 256],
}

impl<'a> AsyncByteIterator for FileRemainderByteIterator<'a> {
    type ErrOut = io::Error;

    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::ErrOut> {
        if let Some(b) = self.remainder.buffer.get(self.index) {
            self.index += 1;
            return Ok(Some((self.index - 1, *b)));
        }
        let n = self.remainder.file.read(&mut self.tmp[..]).await?;
        if n == 0 {
            return Ok(None);
        }
        self.remainder.buffer.extend(&self.tmp[..n]);
        let b = self.remainder.buffer.get(self.index).unwrap();
        self.index += 1;
        Ok(Some((self.index - 1, *b)))
    }
}

struct FileRemainderCharIterator<'a> {
    bytes: FileRemainderByteIterator<'a>,
    tmp: Vec<u8>,
}

impl<'a> AsyncCharIterator for FileRemainderCharIterator<'a> {
    type ErrOut = io::Error;

    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::ErrOut> {
        loop {
            while self.tmp.len() < 4 {
                match self.bytes.next().await? {
                    Some((_, b)) => self.tmp.push(b),
                    None => break,
                }
            }
            if self.tmp.is_empty() {
                return Ok(None);
            }
            let pos = self.bytes.index - self.tmp.len();
            for len in 1..=self.tmp.len() {
                if let Ok(s) = std::str::from_utf8(&self.tmp[..len]) {
                    if let Some(c) = s.chars().next() {
                        self.tmp.drain(..len);
                        return Ok(Some((pos, len, c)));
                    }
                }
            }
            self.tmp.remove(0);
            return Ok(Some((pos, 1, char::REPLACEMENT_CHARACTER)));
        }
    }
}
