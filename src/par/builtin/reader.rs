use arcstr::{literal, Substr};
use byteview::ByteView;

use crate::{
    icombs::readback::Handle,
    par::builtin::{
        bytes::{BytesMachine, BytesPattern},
        string::{StringMachine, StringPattern},
    },
};

pub trait BytesRemainder {
    type ErrIn;
    type ErrOut;
    type Iterator<'a>: AsyncByteIterator<ErrOut = Self::ErrOut>
    where
        Self: 'a;

    async fn read_error_in(handle: Handle) -> Self::ErrIn;
    async fn provide_error_out(handle: Handle, err_out: Self::ErrOut);

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut>;
    fn bytes(&mut self) -> Self::Iterator<'_>;
    fn pop_bytes(&mut self, n: usize) -> ByteView;
    async fn remaining_bytes(&mut self) -> Result<ByteView, Self::ErrOut>;
}

pub trait CharsRemainder {
    type ErrIn;
    type ErrOut;
    type Iterator<'a>: AsyncCharIterator<ErrOut = Self::ErrOut>
    where
        Self: 'a;

    async fn read_error_in(handle: Handle) -> Self::ErrIn;
    async fn provide_error_out(handle: Handle, err_out: Self::ErrOut);

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut>;
    fn chars(&mut self) -> Self::Iterator<'_>;
    fn pop_chars(&mut self, n: usize) -> Substr;
    async fn remaining_chars(&mut self) -> Result<Substr, Self::ErrOut>;
}

pub trait AsyncByteIterator {
    type ErrOut;
    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::ErrOut>;
}

pub trait AsyncCharIterator {
    type ErrOut;
    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::ErrOut>;
}

pub enum Never {}

impl ToString for Never {
    fn to_string(&self) -> String {
        "Never".to_string()
    }
}

impl BytesRemainder for ByteView {
    type ErrIn = Never;
    type ErrOut = Never;
    type Iterator<'a>
        = (usize, &'a ByteView)
    where
        Self: 'a;

    async fn read_error_in(_: Handle) -> Self::ErrIn {
        unreachable!()
    }

    async fn provide_error_out(_: Handle, err_out: Self::ErrOut) {
        match err_out {}
    }

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut> {
        match result_in {
            Ok(()) => Ok(()),
        }
    }

    fn bytes(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_bytes(&mut self, n: usize) -> ByteView {
        let popped = self.slice(..n);
        *self = self.slice(n..);
        popped
    }

    async fn remaining_bytes(&mut self) -> Result<ByteView, Self::ErrOut> {
        Ok(self.clone())
    }
}

impl<'a> AsyncByteIterator for (usize, &'a ByteView) {
    type ErrOut = Never;

    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::ErrOut> {
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

impl CharsRemainder for Substr {
    type ErrIn = Never;
    type ErrOut = Never;
    type Iterator<'a>
        = (usize, &'a Substr)
    where
        Self: 'a;

    async fn read_error_in(_: Handle) -> Self::ErrIn {
        unreachable!()
    }

    async fn provide_error_out(_: Handle, err_out: Self::ErrOut) {
        match err_out {}
    }

    async fn close(self, result_in: Result<(), Self::ErrIn>) -> Result<(), Self::ErrOut> {
        match result_in {
            Ok(()) => Ok(()),
        }
    }

    fn chars(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_chars(&mut self, n: usize) -> Substr {
        let popped = self.substr(..n);
        *self = self.substr(n..);
        popped
    }

    async fn remaining_chars(&mut self) -> Result<Substr, Self::ErrOut> {
        Ok(self.clone())
    }
}

impl<'a> AsyncCharIterator for (usize, &'a Substr) {
    type ErrOut = Never;

    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::ErrOut> {
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

pub async fn provide_bytes_reader<R: BytesRemainder>(mut handle: Handle, mut remainder: R) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                let result_in = {
                    let mut handle = handle.receive();
                    match handle.case().await.as_str() {
                        "ok" => {
                            handle.continue_();
                            Ok(())
                        }
                        "err" => Err(R::read_error_in(handle).await),
                        _ => unreachable!(),
                    }
                };
                match remainder.close(result_in).await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }
            }

            "byte" => {
                let mut bytes = remainder.bytes();
                match bytes.next().await {
                    Ok(Some((_, b))) => {
                        drop(bytes);
                        handle.signal(literal!("byte"));
                        handle.send().provide_byte(b);
                        remainder.pop_bytes(1);
                    }
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }
            }

            "match" => {
                let prefix = BytesPattern::readback(handle.receive()).await;
                let suffix = BytesPattern::readback(handle.receive()).await;
                match remainder.bytes().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
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
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            return R::provide_error_out(handle, err).await;
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
                        handle.signal(literal!("match"));
                        handle.send().provide_bytes(remainder.pop_bytes(i));
                        handle.send().provide_bytes(remainder.pop_bytes(j - i));
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }

            "matchEnd" => {
                let prefix = BytesPattern::readback(handle.receive()).await;
                let suffix = BytesPattern::readback(handle.receive()).await;
                match remainder.bytes().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }

                let mut m = BytesMachine::start(Box::new(BytesPattern::Concat(prefix, suffix)));

                let mut bytes = remainder.bytes();
                loop {
                    let (pos, b) = match bytes.next().await {
                        Ok(Some((pos, b))) => (pos, b),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            return R::provide_error_out(handle, err).await;
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
                                handle.signal(literal!("end"));
                                handle.signal(literal!("err"));
                                return R::provide_error_out(handle, err).await;
                            }
                        };
                        handle.signal(literal!("match"));
                        handle.send().provide_bytes(left);
                        handle.send().provide_bytes(right);
                        return handle.break_();
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => match remainder.remaining_bytes().await {
                Ok(bytes) => {
                    handle.signal(literal!("ok"));
                    return handle.provide_bytes(bytes);
                }
                Err(err) => {
                    handle.signal(literal!("err"));
                    return R::provide_error_out(handle, err).await;
                }
            },
            _ => unreachable!(),
        }
    }
}

pub async fn provide_string_reader<R: CharsRemainder>(mut handle: Handle, mut remainder: R) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                let result_in = {
                    let mut handle = handle.receive();
                    match handle.case().await.as_str() {
                        "ok" => {
                            handle.continue_();
                            Ok(())
                        }
                        "err" => Err(R::read_error_in(handle).await),
                        _ => unreachable!(),
                    }
                };
                match remainder.close(result_in).await {
                    Ok(()) => {
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }
            }

            "char" => {
                let mut chars = remainder.chars();
                match chars.next().await {
                    Ok(Some((_, len, ch))) => {
                        drop(chars);
                        handle.signal(literal!("char"));
                        handle.send().provide_char(ch);
                        remainder.pop_chars(len);
                    }
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }
            }

            "match" => {
                let prefix = StringPattern::readback(handle.receive()).await;
                let suffix = StringPattern::readback(handle.receive()).await;
                match remainder.chars().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
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
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            return R::provide_error_out(handle, err).await;
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
                        handle.signal(literal!("match"));
                        handle.send().provide_string(remainder.pop_chars(i));
                        handle.send().provide_string(remainder.pop_chars(j - i));
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }

            "matchEnd" => {
                let prefix = StringPattern::readback(handle.receive()).await;
                let suffix = StringPattern::readback(handle.receive()).await;
                match remainder.chars().next().await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("ok"));
                        return handle.break_();
                    }
                    Err(err) => {
                        handle.signal(literal!("end"));
                        handle.signal(literal!("err"));
                        return R::provide_error_out(handle, err).await;
                    }
                }

                let mut m = StringMachine::start(Box::new(StringPattern::Concat(prefix, suffix)));

                let mut chars = remainder.chars();
                loop {
                    let (pos, len, ch) = match chars.next().await {
                        Ok(Some((pos, len, ch))) => (pos, len, ch),
                        Ok(None) => break,
                        Err(err) => {
                            handle.signal(literal!("end"));
                            handle.signal(literal!("err"));
                            return R::provide_error_out(handle, err).await;
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
                                handle.signal(literal!("end"));
                                handle.signal(literal!("err"));
                                return R::provide_error_out(handle, err).await;
                            }
                        };
                        handle.signal(literal!("match"));
                        handle.send().provide_string(left);
                        handle.send().provide_string(right);
                        return handle.break_();
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => match remainder.remaining_chars().await {
                Ok(string) => {
                    handle.signal(literal!("ok"));
                    return handle.provide_string(string);
                }
                Err(err) => {
                    handle.signal(literal!("err"));
                    return R::provide_error_out(handle, err).await;
                }
            },
            _ => unreachable!(),
        }
    }
}
