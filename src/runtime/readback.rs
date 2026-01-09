use crate::par::primitive::{ParString, Primitive};
use arcstr::ArcStr;
use bytes::Bytes;
use num_bigint::BigInt;

use std::future::Future;

pub enum Handle {
    Old(super::old::readback::Handle),
    New(super::new::readback::Handle),
}

impl Handle {
    pub fn erase(self) {
        match self {
            Handle::Old(handle) => handle.erase(),
            Handle::New(handle) => handle.erase(),
        }
    }

    pub fn break_(self) {
        match self {
            Handle::Old(handle) => handle.break_(),
            Handle::New(handle) => handle.break_(),
        }
    }

    pub fn continue_(self) {
        match self {
            Handle::Old(handle) => handle.continue_(),
            Handle::New(handle) => handle.continue_(),
        }
    }

    pub async fn case(&mut self) -> ArcStr {
        match self {
            Handle::Old(handle) => handle.case().await,
            Handle::New(handle) => handle.case().await,
        }
    }

    pub fn signal(&mut self, s: ArcStr) {
        match self {
            Handle::Old(handle) => handle.signal(s),
            Handle::New(handle) => handle.signal(s),
        }
    }

    pub fn send(&mut self) -> Handle {
        match self {
            Handle::Old(handle) => Handle::Old(handle.send()),
            Handle::New(handle) => Handle::New(handle.send()),
        }
    }

    pub async fn receive(&mut self) -> Handle {
        match self {
            Handle::Old(handle) => Handle::Old(handle.receive()),
            Handle::New(handle) => Handle::New(handle.receive().await),
        }
    }

    pub fn duplicate(&mut self) -> Handle {
        match self {
            Handle::Old(handle) => Handle::Old(handle.duplicate()),
            Handle::New(handle) => Handle::New(handle.duplicate()),
        }
    }

    pub fn provide_int(self, value: BigInt) {
        match self {
            Handle::Old(handle) => handle.provide_int(value),
            Handle::New(handle) => handle.provide_primitive(Primitive::Int(value)),
        }
    }

    pub fn provide_nat(self, value: BigInt) {
        match self {
            Handle::Old(handle) => handle.provide_nat(value),
            Handle::New(handle) => handle.provide_primitive(Primitive::Int(value)),
        }
    }

    pub fn provide_string(self, value: ParString) {
        match self {
            Handle::Old(handle) => handle.provide_string(value),
            Handle::New(handle) => handle.provide_primitive(Primitive::String(value)),
        }
    }

    pub fn provide_bytes(self, value: Bytes) {
        match self {
            Handle::Old(handle) => handle.provide_bytes(value),
            Handle::New(handle) => handle.provide_primitive(Primitive::Bytes(value)),
        }
    }

    pub fn provide_char(self, value: char) {
        match self {
            Handle::Old(handle) => handle.provide_char(value),
            Handle::New(handle) => handle.provide_primitive(Primitive::String(
                ParString::copy_from_slice(value.encode_utf8(&mut [0u8; 4]).as_bytes()),
            )),
        }
    }

    pub fn provide_byte(self, value: u8) {
        match self {
            Handle::Old(handle) => handle.provide_byte(value),
            Handle::New(handle) => {
                handle.provide_primitive(Primitive::Bytes(Bytes::copy_from_slice(&[value])))
            }
        }
    }

    pub async fn byte(self) -> u8 {
        match self {
            Handle::Old(handle) => handle.byte().await,
            Handle::New(handle) => {
                let primitive = handle.primitive().await.unwrap();
                let Primitive::Bytes(value) = primitive else {
                    panic!(
                        "Unexpected primitive in Handle! Expected Bytes, got {:?}",
                        primitive
                    )
                };
                assert!(value.len() == 1);
                value.as_ref()[0]
            }
        }
    }

    pub async fn char(self) -> char {
        match self {
            Handle::Old(handle) => handle.char().await,
            Handle::New(handle) => {
                let primitive = handle.primitive().await.unwrap();
                let Primitive::String(value) = primitive else {
                    panic!(
                        "Unexpected primitive in Handle! Expected String, got {:?}",
                        primitive
                    )
                };
                let value = value.as_str();
                assert!(value.len() == 1);
                value.chars().nth(0).unwrap()
            }
        }
    }

    pub async fn string(self) -> ParString {
        match self {
            Handle::Old(handle) => handle.string().await,
            Handle::New(handle) => {
                let primitive = handle.primitive().await.unwrap();
                let Primitive::String(value) = primitive else {
                    panic!(
                        "Unexpected primitive in Handle! Expected String, got {:?}",
                        primitive
                    )
                };
                value
            }
        }
    }

    pub async fn bytes(self) -> Bytes {
        match self {
            Handle::Old(handle) => handle.bytes().await,
            Handle::New(handle) => match handle.primitive().await.unwrap() {
                Primitive::String(e) => e.as_bytes(),
                Primitive::Bytes(e) => e,
                primitive => panic!(
                    "Unexpected primitive in Handle! Expected String or Bytes, got {:?}",
                    primitive
                ),
            },
        }
    }

    pub async fn int(self) -> BigInt {
        match self {
            Handle::Old(handle) => handle.int().await,
            Handle::New(handle) => {
                let primitive = handle.primitive().await.unwrap();
                let Primitive::Int(value) = primitive else {
                    panic!(
                        "Unexpected primitive in Handle! Expected Int, got {:?}",
                        primitive
                    )
                };
                value
            }
        }
    }

    pub async fn nat(self) -> BigInt {
        match self {
            Handle::Old(handle) => handle.nat().await,
            Handle::New(handle) => {
                let primitive = handle.primitive().await.unwrap();
                let Primitive::Int(value) = primitive else {
                    panic!(
                        "Unexpected primitive in Handle! Expected Int, got {:?}",
                        primitive
                    )
                };
                value
            }
        }
    }

    pub fn link(self, dual: Handle) {
        match (self, dual) {
            (Handle::Old(a), Handle::Old(b)) => a.link(b),
            (Handle::New(a), Handle::New(b)) => a.link_with(b),
            _ => todo!(),
        }
    }

    pub fn concurrently<F>(self, f: impl FnOnce(Self) -> F)
    where
        F: 'static + Send + Future<Output = ()>,
    {
        match self {
            Handle::Old(handle) => handle.concurrently(move |h| f(Handle::Old(h))),
            Handle::New(handle) => handle.concurrently(move |h| f(Handle::New(h))),
        }
    }

    pub fn provide_box<Fun, Fut>(self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        match self {
            Handle::Old(handle) => {
                handle.provide_box(move |x| f(Handle::Old(x)));
            }
            Handle::New(handle) => handle.provide_external_closure(move |x| f(Handle::New(x))),
        }
    }
}
