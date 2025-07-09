use std::{collections::VecDeque, ffi::OsStr, fs::Metadata, path::PathBuf, sync::Arc};

use arcstr::{literal, Substr};
use num_bigint::BigInt;
use tokio::{
    fs::{self, File},
    io::{self, AsyncReadExt},
};

use crate::{
    icombs::readback::Handle,
    par::{
        builtin::string::{Machine, Pattern},
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
        handle.provide_string(Substr::from("Unsupported file type"));
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

                "readUTF8" => match File::open(info.path.as_ref()).await {
                    Ok(file) => {
                        handle.signal(literal!("ok"));
                        provide_string_reader(handle, file).await;
                    }

                    Err(err) => {
                        handle.signal(literal!("err"));
                        handle.provide_string(Substr::from(err.to_string()));
                    }
                },

                _ => unreachable!(),
            }
        }
    })
}

async fn provide_string_reader(mut handle: Handle, file: File) {
    let mut remainder = FileRemainder {
        file,
        buffer: VecDeque::new(),
    };

    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
            }

            "char" => match remainder.char_indices().next().await {
                Ok(Some((_, ch))) => {
                    handle.signal(literal!("char"));
                    handle.send().provide_char(ch);
                    remainder.pop(ch.len_utf8());
                }
                Ok(None) => {
                    handle.signal(literal!("end"));
                    handle.signal(literal!("ok"));
                    handle.break_();
                    return;
                }
                Err(err) => {
                    handle.signal(literal!("end"));
                    handle.signal(literal!("err"));
                    handle.provide_string(Substr::from(err.to_string()));
                    return;
                }
            },

            "match" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                match remainder.char_indices().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        handle.break_();
                        return;
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        handle.provide_string(Substr::from(err.to_string()));
                        return;
                    }
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                let mut best_match = None;
                let mut char_indices = remainder.char_indices();
                loop {
                    let (pos, ch) = match char_indices.next().await {
                        Ok(Some((pos, ch))) => (pos, ch),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            handle.provide_string(Substr::from(err.to_string()));
                            return;
                        }
                    };
                    match (m.leftmost_feasible_split(pos), best_match) {
                        (Some(fi), Some((bi, _))) if fi > bi => break,
                        (None, _) => break,
                        _ => {}
                    }
                    m.advance(pos, ch);
                    match (m.leftmost_accepting_split(), best_match) {
                        (Some(ai), Some((bi, _))) if ai <= bi => {
                            best_match = Some((ai, pos + ch.len_utf8()))
                        }
                        (Some(ai), None) => best_match = Some((ai, pos + ch.len_utf8())),
                        _ => {}
                    }
                }

                match best_match {
                    Some((i, j)) => {
                        handle.signal(literal!("match"));
                        let matched = remainder.pop(j);
                        handle.send().provide_string(matched.substr(..i));
                        handle.send().provide_string(matched.substr(i..));
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "matchEnd" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                match remainder.char_indices().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        handle.break_();
                        return;
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        handle.provide_string(Substr::from(err.to_string()));
                        return;
                    }
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                let mut char_indices = remainder.char_indices();
                loop {
                    let (pos, ch) = match char_indices.next().await {
                        Ok(Some((pos, ch))) => (pos, ch),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            handle.provide_string(Substr::from(err.to_string()));
                            return;
                        }
                    };
                    if m.accepts() == None {
                        break;
                    }
                    m.advance(pos, ch);
                }

                match m.leftmost_accepting_split() {
                    Some(i) => {
                        let left = remainder.pop(i);
                        let right = match remainder.all().await {
                            Ok(string) => string,
                            Err(err) => {
                                handle.signal(literal!("end"));
                                handle.signal(literal!("err"));
                                handle.provide_string(Substr::from(err.to_string()));
                                return;
                            }
                        };
                        handle.signal(literal!("match"));
                        handle.send().provide_string(left);
                        handle.send().provide_string(right);
                        handle.break_();
                        return;
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => {
                match remainder.all().await {
                    Ok(string) => {
                        handle.signal(literal!("ok"));
                        handle.provide_string(string);
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        handle.provide_string(Substr::from(err.to_string()));
                    }
                }
                return;
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
    fn char_indices(&mut self) -> FileRemainderChars<'_> {
        FileRemainderChars {
            bytes: FileRemainderBytes {
                remainder: self,
                index: 0,
                tmp: [0; 256],
            },
            tmp: Vec::with_capacity(4),
        }
    }

    fn pop(&mut self, n: usize) -> Substr {
        let popped = self.buffer.drain(..n).collect::<Vec<u8>>();
        let popped = String::from_utf8_lossy(&popped[..]);
        Substr::from(popped)
    }

    async fn all(&mut self) -> io::Result<Substr> {
        let mut string = String::new();
        let mut char_indices = self.char_indices();
        while let Some((_, ch)) = char_indices.next().await? {
            string.push(ch);
        }
        Ok(Substr::from(string))
    }
}

struct FileRemainderBytes<'a> {
    remainder: &'a mut FileRemainder,
    index: usize,
    tmp: [u8; 256],
}

impl<'a> FileRemainderBytes<'a> {
    async fn next(&mut self) -> io::Result<Option<u8>> {
        if let Some(b) = self.remainder.buffer.get(self.index) {
            self.index += 1;
            return Ok(Some(*b));
        }
        let n = self.remainder.file.read(&mut self.tmp[..]).await?;
        if n == 0 {
            return Ok(None);
        }
        self.remainder.buffer.extend(&self.tmp[..n]);
        let b = self.remainder.buffer.get(self.index).unwrap();
        self.index += 1;
        Ok(Some(*b))
    }
}

struct FileRemainderChars<'a> {
    bytes: FileRemainderBytes<'a>,
    tmp: Vec<u8>,
}

impl<'a> FileRemainderChars<'a> {
    async fn next(&mut self) -> io::Result<Option<(usize, char)>> {
        loop {
            while self.tmp.len() < 4 {
                match self.bytes.next().await? {
                    Some(b) => self.tmp.push(b),
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
                        return Ok(Some((pos, c)));
                    }
                }
            }
            self.tmp.remove(0);
            return Ok(Some((pos, char::REPLACEMENT_CHARACTER)));
        }
    }
}
