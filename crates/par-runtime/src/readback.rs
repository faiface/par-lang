use crate::primitive::{ParString, Primitive};
use arcstr::ArcStr;
use bytes::Bytes;
use num_bigint::BigInt;

use std::future::Future;

pub struct Handle {
    pub handle: super::flat::readback::Handle,
}

impl From<super::flat::readback::Handle> for Handle {
    fn from(value: super::flat::readback::Handle) -> Self {
        Self { handle: value }
    }
}

impl Handle {
    pub fn erase(self) {
        self.handle.erase()
    }

    pub fn break_(self) {
        self.handle.break_()
    }

    pub fn continue_(self) {
        self.handle.continue_()
    }

    pub async fn case(&mut self) -> ArcStr {
        self.handle.case().await
    }

    pub fn signal(&mut self, s: ArcStr) {
        self.handle.signal(s)
    }

    pub fn send(&mut self) -> Handle {
        Handle::from(self.handle.send())
    }

    pub fn receive(&mut self) -> Handle {
        Handle::from(self.handle.receive())
    }

    pub fn duplicate(&mut self) -> Handle {
        Handle::from(self.handle.duplicate())
    }

    pub fn provide_int(self, value: BigInt) {
        self.handle.provide_primitive(Primitive::Int(value))
    }

    pub fn provide_nat(self, value: BigInt) {
        self.handle.provide_primitive(Primitive::Int(value))
    }

    pub fn provide_string(self, value: ParString) {
        self.handle.provide_primitive(Primitive::String(value))
    }

    pub fn provide_bytes(self, value: Bytes) {
        self.handle.provide_primitive(Primitive::Bytes(value))
    }

    pub fn provide_char(self, value: char) {
        self.handle
            .provide_primitive(Primitive::String(ParString::copy_from_slice(
                value.encode_utf8(&mut [0u8; 4]).as_bytes(),
            )))
    }

    pub fn provide_byte(self, value: u8) {
        self.handle
            .provide_primitive(Primitive::Bytes(Bytes::copy_from_slice(&[value])))
    }

    pub async fn byte(self) -> u8 {
        let primitive = self.handle.primitive().await.unwrap();
        let Primitive::Bytes(value) = primitive else {
            panic!(
                "Unexpected primitive in Handle! Expected Bytes, got {:?}",
                primitive
            )
        };
        assert!(value.len() == 1);
        value.as_ref()[0]
    }

    pub async fn char(self) -> char {
        let primitive = self.handle.primitive().await.unwrap();
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

    pub async fn string(self) -> ParString {
        let primitive = self.handle.primitive().await.unwrap();
        let Primitive::String(value) = primitive else {
            panic!(
                "Unexpected primitive in Handle! Expected String, got {:?}",
                primitive
            )
        };
        value
    }

    pub async fn bytes(self) -> Bytes {
        match self.handle.primitive().await.unwrap() {
            Primitive::String(e) => e.as_bytes(),
            Primitive::Bytes(e) => e,
            primitive => panic!(
                "Unexpected primitive in Handle! Expected String or Bytes, got {:?}",
                primitive
            ),
        }
    }

    pub async fn int(self) -> BigInt {
        let primitive = self.handle.primitive().await.unwrap();
        let Primitive::Int(value) = primitive else {
            panic!(
                "Unexpected primitive in Handle! Expected Int, got {:?}",
                primitive
            )
        };
        value
    }

    pub async fn nat(self) -> BigInt {
        let primitive = self.handle.primitive().await.unwrap();
        let Primitive::Int(value) = primitive else {
            panic!(
                "Unexpected primitive in Handle! Expected Int, got {:?}",
                primitive
            )
        };
        value
    }

    pub fn link(self, dual: Handle) {
        self.handle.link_with(dual.handle)
    }

    pub fn concurrently<F>(self, f: impl FnOnce(Self) -> F)
    where
        F: 'static + Send + Future<Output = ()>,
    {
        self.handle.concurrently(move |handle| f(Handle { handle }))
    }

    pub fn provide_box<Fun, Fut>(self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        self.handle
            .provide_external_closure(move |handle| f(Handle { handle }))
    }
}
