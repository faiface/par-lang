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
    type Error;
    type Iterator<'a>: AsyncByteIterator<Error = Self::Error>
    where
        Self: 'a;

    fn bytes(&mut self) -> Self::Iterator<'_>;
    fn pop_bytes(&mut self, n: usize) -> ByteView;
    async fn remaining_bytes(&mut self) -> Result<ByteView, Self::Error>;
}

pub trait CharsRemainder {
    type Error;
    type Iterator<'a>: AsyncCharIterator<Error = Self::Error>
    where
        Self: 'a;

    fn chars(&mut self) -> Self::Iterator<'_>;
    fn pop_chars(&mut self, n: usize) -> Substr;
    async fn remaining_chars(&mut self) -> Result<Substr, Self::Error>;
}

pub trait AsyncByteIterator {
    type Error;
    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::Error>;
}

pub trait AsyncCharIterator {
    type Error;
    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::Error>;
}

pub enum Never {}

impl ToString for Never {
    fn to_string(&self) -> String {
        "Never".to_string()
    }
}

impl BytesRemainder for ByteView {
    type Error = Never;
    type Iterator<'a>
        = (usize, &'a ByteView)
    where
        Self: 'a;

    fn bytes(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_bytes(&mut self, n: usize) -> ByteView {
        self.slice(n..)
    }

    async fn remaining_bytes(&mut self) -> Result<ByteView, Self::Error> {
        Ok(self.clone())
    }
}

impl<'a> AsyncByteIterator for (usize, &'a ByteView) {
    type Error = Never;

    async fn next(&mut self) -> Result<Option<(usize, u8)>, Self::Error> {
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
    type Error = Never;
    type Iterator<'a>
        = (usize, &'a Substr)
    where
        Self: 'a;

    fn chars(&mut self) -> Self::Iterator<'_> {
        (0, self)
    }

    fn pop_chars(&mut self, n: usize) -> Substr {
        self.substr(n..)
    }

    async fn remaining_chars(&mut self) -> Result<Substr, Self::Error> {
        Ok(self.clone())
    }
}

impl<'a> AsyncCharIterator for (usize, &'a Substr) {
    type Error = Never;

    async fn next(&mut self) -> Result<Option<(usize, usize, char)>, Self::Error> {
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

pub async fn provide_bytes_reader(
    mut handle: Handle,
    mut remainder: impl BytesRemainder<Error: ToString>,
) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
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
            }

            "match" => {
                let prefix = BytesPattern::readback(handle.receive()).await;
                let suffix = BytesPattern::readback(handle.receive()).await;
                match remainder.bytes().next().await {
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
                            handle.provide_string(Substr::from(err.to_string()));
                            return;
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

                let mut m = BytesMachine::start(Box::new(BytesPattern::Concat(prefix, suffix)));

                let mut bytes = remainder.bytes();
                loop {
                    let (pos, b) = match bytes.next().await {
                        Ok(Some((pos, b))) => (pos, b),
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
                                handle.provide_string(Substr::from(err.to_string()));
                                return;
                            }
                        };
                        handle.signal(literal!("match"));
                        handle.send().provide_bytes(left);
                        handle.send().provide_bytes(right);
                        handle.break_();
                        return;
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => {
                match remainder.remaining_bytes().await {
                    Ok(bytes) => {
                        handle.signal(literal!("ok"));
                        handle.provide_bytes(bytes);
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

pub async fn provide_string_reader(
    mut handle: Handle,
    mut remainder: impl CharsRemainder<Error: ToString>,
) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
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
            }

            "match" => {
                let prefix = StringPattern::readback(handle.receive()).await;
                let suffix = StringPattern::readback(handle.receive()).await;
                match remainder.chars().next().await {
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
                            handle.provide_string(Substr::from(err.to_string()));
                            return;
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

                let mut m = StringMachine::start(Box::new(StringPattern::Concat(prefix, suffix)));

                let mut chars = remainder.chars();
                loop {
                    let (pos, len, ch) = match chars.next().await {
                        Ok(Some((pos, len, ch))) => (pos, len, ch),
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
                match remainder.remaining_chars().await {
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
