use arcstr::literal;
use bytes::Bytes;
use std::collections::VecDeque;

use crate::{
    par::{
        builtin::{
            bytes::{BytesMachine, BytesPattern},
            string::{StringMachine, StringPattern},
        },
        primitive::ParString,
    },
    runtime::Handle,
};

pub trait BytesRemainder {
    type Err;
    type Iterator<'a>: AsyncByteIterator<Err = Self::Err>
    where
        Self: 'a;

    async fn provide_err(handle: Handle, err: Self::Err);

    async fn close(self) -> Result<(), Self::Err>;
    fn bytes(&mut self) -> Self::Iterator<'_>;
    fn pop_bytes(&mut self, n: usize) -> Bytes;
    async fn remaining_bytes(&mut self) -> Result<Bytes, Self::Err>;
}

pub trait CharsRemainder {
    type Err;
    type Iterator<'a>: AsyncCharIterator<Err = Self::Err>
    where
        Self: 'a;

    async fn provide_err(handle: Handle, err: Self::Err);

    async fn close(self) -> Result<(), Self::Err>;
    fn chars(&mut self) -> Self::Iterator<'_>;
    fn pop_chars(&mut self, n: usize) -> ParString;
    async fn remaining_chars(&mut self) -> Result<ParString, Self::Err>;
}

pub trait AsyncByteIterator {
    type Err;
    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::Err>;
}

pub trait AsyncCharIterator {
    type Err;
    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::Err>;
}

pub enum Never {}

impl ToString for Never {
    fn to_string(&self) -> String {
        "Never".to_string()
    }
}

// A generic remainder that adapts a runtime `Bytes.Reader<e>` handle
// into the `BytesRemainder` and `CharsRemainder` traits. Errors are forwarded
// opaquely by passing handles through without interpretation.
pub struct ReaderRemainder {
    handle: Option<Handle>,
    buffer: VecDeque<u8>,
}

impl ReaderRemainder {
    pub fn new(handle: Handle) -> Self {
        Self {
            handle: Some(handle),
            buffer: VecDeque::new(),
        }
    }
}

pub struct ReaderRemainderByteIterator<'a> {
    remainder: &'a mut ReaderRemainder,
    index: usize,
}

impl<'a> AsyncByteIterator for ReaderRemainderByteIterator<'a> {
    // Forward opaque error values via raw Handle
    type Err = Handle;

    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::Err> {
        // Serve from buffer if available
        if let Some(b) = self.remainder.buffer.get(self.index) {
            self.index += 1;
            return Ok(Some((self.index - 1, *b)));
        }

        if self.remainder.handle.is_none() {
            return Ok(None);
        }

        // Request more bytes from the underlying reader
        loop {
            let handle = self.remainder.handle.as_mut().unwrap();
            handle.signal(literal!("read")).await;
            match handle.case().await.as_str() {
                "ok" => match handle.case().await.as_str() {
                    "chunk" => {
                        let chunk = handle.receive().await.bytes().await;
                        assert!(
                            !chunk.is_empty(),
                            "Bytes.Reader returned an empty chunk; implementation bug"
                        );
                        self.remainder.buffer.extend(chunk.as_ref());
                        // After extending by a non-empty chunk, the current index must be valid
                        let b = *self
                            .remainder
                            .buffer
                            .get(self.index)
                            .expect("buffer should contain data at current index");
                        self.index += 1;
                        return Ok(Some((self.index - 1, b)));
                    }
                    "end" => {
                        // Close the provider side of the 'end' branch
                        self.remainder.handle.take().unwrap().continue_().await;
                        return Ok(None);
                    }
                    _ => unreachable!(),
                },
                "err" => {
                    // Propagate the opaque error handle upward without receiving
                    let err = self.remainder.handle.take().unwrap();
                    return Err(err);
                }
                _ => unreachable!(),
            }
        }
    }
}

pub struct ReaderRemainderCharIterator<'a> {
    bytes: ReaderRemainderByteIterator<'a>,
    tmp: Vec<u8>,
}

impl<'a> AsyncCharIterator for ReaderRemainderCharIterator<'a> {
    // Forward opaque error values via raw Handle
    type Err = Handle;

    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::Err> {
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
            // Invalid UTF-8 prefix; consume one byte and return replacement char
            self.tmp.remove(0);
            return Ok(Some((pos, 1, char::REPLACEMENT_CHARACTER)));
        }
    }
}

impl BytesRemainder for ReaderRemainder {
    type Err = Handle;
    type Iterator<'a>
        = ReaderRemainderByteIterator<'a>
    where
        Self: 'a;

    async fn provide_err(handle: Handle, err: Self::Err) {
        // Forward the opaque error value
        handle.link(err).await;
    }

    async fn close(mut self) -> Result<(), Self::Err> {
        let handle = self.handle.as_mut().unwrap();
        handle.signal(literal!("close")).await;
        match handle.case().await.as_str() {
            "ok" => Ok(self.handle.take().unwrap().continue_().await),
            "err" => Err(self.handle.take().unwrap()),
            _ => unreachable!(),
        }
    }

    fn bytes(&mut self) -> Self::Iterator<'_> {
        ReaderRemainderByteIterator {
            remainder: self,
            index: 0,
        }
    }

    fn pop_bytes(&mut self, n: usize) -> Bytes {
        self.buffer.drain(..n).collect()
    }

    async fn remaining_bytes(&mut self) -> Result<Bytes, Self::Err> {
        let mut result = Vec::new();
        let mut iter = self.bytes();
        while let Some((_, b)) = iter.next().await? {
            result.push(b);
        }
        Ok(Bytes::from(result))
    }
}

impl CharsRemainder for ReaderRemainder {
    type Err = Handle;
    type Iterator<'a>
        = ReaderRemainderCharIterator<'a>
    where
        Self: 'a;

    async fn provide_err(handle: Handle, err: Self::Err) {
        // Forward the opaque error value
        handle.link(err).await;
    }

    async fn close(mut self) -> Result<(), Self::Err> {
        let handle = self.handle.as_mut().unwrap();
        handle.signal(literal!("close")).await;
        match handle.case().await.as_str() {
            "ok" => Ok(self.handle.take().unwrap().continue_().await),
            "err" => Err(self.handle.take().unwrap()),
            _ => unreachable!(),
        }
    }

    fn chars(&mut self) -> Self::Iterator<'_> {
        ReaderRemainderCharIterator {
            bytes: ReaderRemainderByteIterator {
                remainder: self,
                index: 0,
            },
            tmp: Vec::with_capacity(4),
        }
    }

    fn pop_chars(&mut self, n: usize) -> ParString {
        let popped = self.buffer.drain(..n).collect::<Vec<u8>>();
        let popped = String::from_utf8_lossy(&popped[..]);
        ParString::copy_from_slice(popped.as_bytes())
    }

    async fn remaining_chars(&mut self) -> Result<ParString, Self::Err> {
        let mut result = String::new();
        let mut iter = self.chars();
        while let Some((_, _, ch)) = iter.next().await? {
            result.push(ch);
        }
        Ok(ParString::from(result))
    }
}

impl BytesRemainder for Bytes {
    type Err = Never;
    type Iterator<'a>
        = (usize, &'a Bytes)
    where
        Self: 'a;

    async fn provide_err(_: Handle, err: Self::Err) {
        match err {}
    }

    async fn close(self) -> Result<(), Self::Err> {
        Ok(())
    }

    fn bytes(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_bytes(&mut self, n: usize) -> Bytes {
        let popped = self.slice(..n);
        *self = self.slice(n..);
        popped
    }

    async fn remaining_bytes(&mut self) -> Result<Bytes, Self::Err> {
        Ok(self.clone())
    }
}

impl<'a> AsyncByteIterator for (usize, &'a Bytes) {
    type Err = Never;

    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::Err> {
        let (index, bytes) = self;
        Ok(match bytes.get(*index) {
            Some(&byte) => Some((*index, {
                *index += 1;
                byte
            })),
            None => None,
        })
    }
}

impl CharsRemainder for ParString {
    type Err = Never;
    type Iterator<'a>
        = (usize, &'a ParString)
    where
        Self: 'a;

    async fn provide_err(_: Handle, err: Self::Err) {
        match err {}
    }

    async fn close(self) -> Result<(), Self::Err> {
        Ok(())
    }

    fn chars(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_chars(&mut self, n: usize) -> ParString {
        let popped = self.substr(..n);
        *self = self.substr(n..);
        popped
    }

    async fn remaining_chars(&mut self) -> Result<ParString, Self::Err> {
        Ok(ParString::from(self.clone()))
    }
}

impl<'a> AsyncCharIterator for (usize, &'a ParString) {
    type Err = Never;

    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::Err> {
        let (index, chars) = self;
        Ok(match chars.as_str()[*index..].chars().next() {
            Some(ch) => Some((*index, ch.len_utf8(), {
                *index += ch.len_utf8();
                ch
            })),
            None => None,
        })
    }
}

pub async fn provide_bytes_parser<R: BytesRemainder>(mut handle: Handle, mut remainder: R) {
    loop {
        match handle.case().await.as_str() {
            "close" => match remainder.close().await {
                Ok(()) => {
                    handle.signal(literal!("ok")).await;
                    return handle.break_().await;
                }
                Err(err) => {
                    handle.signal(literal!("err")).await;
                    return R::provide_err(handle, err).await;
                }
            },

            "byte" => {
                let mut bytes = remainder.bytes();
                match bytes.next().await {
                    Ok(Some((_, b))) => {
                        drop(bytes);
                        handle.signal(literal!("byte")).await;
                        handle.send().await.provide_byte(b).await;
                        remainder.pop_bytes(1);
                    }
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }
            }

            "match" => {
                let prefix = BytesPattern::readback(handle.receive().await).await;
                let suffix = BytesPattern::readback(handle.receive().await).await;
                match remainder.bytes().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }

                let mut m = BytesMachine::start(Box::new(BytesPattern::Concat(prefix, suffix)));

                let mut best_match = None;
                let mut bytes = remainder.bytes();
                loop {
                    let (pos, b) = match bytes.next().await {
                        Ok(Some((pos, b))) => (pos, b),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end")).await;
                            handle.signal(literal!("err")).await;
                            return R::provide_err(handle, err).await;
                        }
                    };
                    match (m.leftmost_feasible_split(pos), best_match) {
                        (Some(fi), Some((bi, _))) if fi > bi => break,
                        (None, _) => break,
                        _ => {}
                    }
                    m.advance(pos, b);
                    match (m.leftmost_accepting_split(), best_match) {
                        (Some(ai), Some((bi, _))) if ai <= bi => best_match = Some((ai, pos + 1)),
                        (Some(ai), None) => best_match = Some((ai, pos + 1)),
                        _ => {}
                    }
                }
                drop(bytes);

                match best_match {
                    Some((i, j)) => {
                        handle.signal(literal!("match")).await;
                        handle
                            .send()
                            .await
                            .provide_bytes(remainder.pop_bytes(i))
                            .await;
                        handle
                            .send()
                            .await
                            .provide_bytes(remainder.pop_bytes(j - i))
                            .await;
                    }
                    None => {
                        handle.signal(literal!("fail")).await;
                    }
                }
            }

            "matchEnd" => {
                let prefix = BytesPattern::readback(handle.receive().await).await;
                let suffix = BytesPattern::readback(handle.receive().await).await;
                match remainder.bytes().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }

                let mut m = BytesMachine::start(Box::new(BytesPattern::Concat(prefix, suffix)));

                let mut bytes = remainder.bytes();
                loop {
                    let (pos, b) = match bytes.next().await {
                        Ok(Some((pos, b))) => (pos, b),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end")).await;
                            handle.signal(literal!("err")).await;
                            return R::provide_err(handle, err).await;
                        }
                    };
                    if m.accepts() == None {
                        break;
                    }
                    m.advance(pos, b);
                }
                drop(bytes);

                match m.leftmost_accepting_split() {
                    Some(i) => {
                        let left = remainder.pop_bytes(i);
                        let right = match remainder.remaining_bytes().await {
                            Ok(bytes) => bytes,
                            Err(err) => {
                                handle.signal(literal!("end")).await;
                                handle.signal(literal!("err")).await;
                                return R::provide_err(handle, err).await;
                            }
                        };
                        handle.signal(literal!("match")).await;
                        handle.send().await.provide_bytes(left).await;
                        handle.send().await.provide_bytes(right).await;
                        return handle.break_().await;
                    }
                    None => {
                        handle.signal(literal!("fail")).await;
                    }
                }
            }
            "remainder" => match remainder.remaining_bytes().await {
                Ok(bytes) => {
                    handle.signal(literal!("ok")).await;
                    return handle.provide_bytes(bytes).await;
                }
                Err(err) => {
                    handle.signal(literal!("err")).await;
                    return R::provide_err(handle, err).await;
                }
            },
            _ => unreachable!(),
        }
    }
}

pub async fn provide_string_parser<R: CharsRemainder>(mut handle: Handle, mut remainder: R) {
    loop {
        match handle.case().await.as_str() {
            "close" => match remainder.close().await {
                Ok(()) => {
                    handle.signal(literal!("ok")).await;
                    return handle.break_().await;
                }
                Err(err) => {
                    handle.signal(literal!("err")).await;
                    return R::provide_err(handle, err).await;
                }
            },

            "char" => {
                let mut chars = remainder.chars();
                match chars.next().await {
                    Ok(Some((_, len, ch))) => {
                        drop(chars);
                        handle.signal(literal!("char")).await;
                        handle.send().await.provide_char(ch).await;
                        remainder.pop_chars(len);
                    }
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }
            }

            "match" => {
                let prefix = StringPattern::readback(handle.receive().await).await;
                let suffix = StringPattern::readback(handle.receive().await).await;
                match remainder.chars().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }

                let mut m = StringMachine::start(Box::new(StringPattern::Concat(prefix, suffix)));

                let mut best_match = None;
                let mut chars = remainder.chars();
                loop {
                    let (pos, len, ch) = match chars.next().await {
                        Ok(Some((pos, len, ch))) => (pos, len, ch),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end")).await;
                            handle.signal(literal!("err")).await;
                            return R::provide_err(handle, err).await;
                        }
                    };
                    match (m.leftmost_feasible_split(pos), best_match) {
                        (Some(fi), Some((bi, _))) if fi > bi => break,
                        (None, _) => break,
                        _ => {}
                    }
                    m.advance(pos, len, ch);
                    match (m.leftmost_accepting_split(), best_match) {
                        (Some(ai), Some((bi, _))) if ai <= bi => best_match = Some((ai, pos + len)),
                        (Some(ai), None) => best_match = Some((ai, pos + len)),
                        _ => {}
                    }
                }
                drop(chars);

                match best_match {
                    Some((i, j)) => {
                        handle.signal(literal!("match")).await;
                        handle
                            .send()
                            .await
                            .provide_string(remainder.pop_chars(i))
                            .await;
                        handle
                            .send()
                            .await
                            .provide_string(remainder.pop_chars(j - i))
                            .await;
                    }
                    None => {
                        handle.signal(literal!("fail")).await;
                    }
                }
            }

            "matchEnd" => {
                let prefix = StringPattern::readback(handle.receive().await).await;
                let suffix = StringPattern::readback(handle.receive().await).await;
                match remainder.chars().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("ok")).await;
                        return handle.break_().await;
                    }
                    Err(err) => {
                        handle.signal(literal!("end")).await;
                        handle.signal(literal!("err")).await;
                        return R::provide_err(handle, err).await;
                    }
                }

                let mut m = StringMachine::start(Box::new(StringPattern::Concat(prefix, suffix)));

                let mut chars = remainder.chars();
                loop {
                    let (pos, len, ch) = match chars.next().await {
                        Ok(Some((pos, len, ch))) => (pos, len, ch),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end")).await;
                            handle.signal(literal!("err")).await;
                            return R::provide_err(handle, err).await;
                        }
                    };
                    if m.accepts() == None {
                        break;
                    }
                    m.advance(pos, len, ch);
                }
                drop(chars);

                match m.leftmost_accepting_split() {
                    Some(i) => {
                        let left = remainder.pop_chars(i);
                        let right = match remainder.remaining_chars().await {
                            Ok(string) => string,
                            Err(err) => {
                                handle.signal(literal!("end")).await;
                                handle.signal(literal!("err")).await;
                                return R::provide_err(handle, err).await;
                            }
                        };
                        handle.signal(literal!("match")).await;
                        handle.send().await.provide_string(left).await;
                        handle.send().await.provide_string(right).await;
                        return handle.break_().await;
                    }
                    None => {
                        handle.signal(literal!("fail")).await;
                    }
                }
            }
            "remainder" => match remainder.remaining_chars().await {
                Ok(string) => {
                    handle.signal(literal!("ok")).await;
                    return handle.provide_string(string).await;
                }
                Err(err) => {
                    handle.signal(literal!("err")).await;
                    return R::provide_err(handle, err).await;
                }
            },
            _ => unreachable!(),
        }
    }
}
