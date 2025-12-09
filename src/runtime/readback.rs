use crate::par::primitive::{ParString, Primitive};
use arcstr::ArcStr;
use bytes::Bytes;
use num_bigint::BigInt;

use std::future::Future;

pub enum Handle {
    Old(super::old::readback::Handle),
    New(super::new::readback::Handle),
}

macro_rules! do_await {
    ($x:expr, await) => {
        $x.await
    };
    ($x:expr) => {
        $x
    };
}

// this macro dispatches various types of methods into either the new handle or the old handle
macro_rules! dispatch {
    (nilary_destructor, $name:ident, $pati:ident : $patt:ty, ( $($arg_pat:ident : $arg_ty:ty),* ), $ret:ty $(,$x:ident)?) => {
        pub async fn $name($pati : $patt , $($arg_pat : $arg_ty),*) -> $ret {
            match $pati {
                Handle::Old(handle) => do_await!(handle.$name($($arg_pat),*) $(,$x)?),
                Handle::New(handle) => handle.$name($($arg_pat),*).await,
            }
        }
    };
    (transformer, $name:ident  $(,$x:ident)?) => {
        pub async fn $name(&mut self) -> Handle {
            match self {
                Handle::Old(handle) => Handle::Old(do_await!(handle.$name() $(,$x)?)),
                Handle::New(handle) => Handle::New(handle.$name().await)
            }
        }
    };
    (provide_primitive, $name:ident, $var:ident : $prim_ty:ty, $expr:expr) => {
        pub async fn $name(self, $var: $prim_ty) {
            match self {
                Handle::Old(handle) => (handle.$name($var)),
                Handle::New(handle) => (handle.provide_primitive($expr).await)
            }
        }
    };
    (primitive, $name:ident, $var:ident, $prim_ty:ty, $variant:ident, $expr:expr) => {
        pub async fn $name(self) -> $prim_ty {
            match self {
                Handle::Old(handle) => handle.$name().await,
                Handle::New(handle) => {
                    let primitive = handle.primitive().await.unwrap();
                    let Primitive::$variant($var) = primitive else {
                        panic!("Unexpected primitive in Handle! Expected {}, got {:?}", stringify!($variant), primitive)
                    };
                    $expr
                }
            }
        }
    };
}

impl Handle {
    dispatch!(nilary_destructor, erase, self : Self, (), ());
    dispatch!(nilary_destructor, break_, self : Self, (), ());
    dispatch!(nilary_destructor, continue_, self : Self, (), ());
    dispatch!(nilary_destructor, case, self : &mut Self, (), ArcStr, await);
    dispatch!(nilary_destructor, signal, self : &mut Self, (s: ArcStr), ());
    dispatch!(transformer, send);
    dispatch!(transformer, receive);
    dispatch!(transformer, duplicate);
    dispatch!(
        provide_primitive,
        provide_int,
        value: BigInt,
        Primitive::Int(value)
    );
    dispatch!(
        provide_primitive,
        provide_nat,
        value: BigInt,
        Primitive::Int(value)
    );
    dispatch!(
        provide_primitive,
        provide_string,
        value : ParString,
        Primitive::String(value)
    );
    dispatch!(
        provide_primitive,
        provide_bytes,
        value : Bytes,
        Primitive::Bytes(value)
    );

    dispatch!(
        provide_primitive,
        provide_char,
        value : char,
        Primitive::String(ParString::copy_from_slice(
            value.encode_utf8(&mut [0u8; 4]).as_bytes(),
        ))
    );

    dispatch!(
        provide_primitive,
        provide_byte,
        value : u8,
        Primitive::Bytes(Bytes::copy_from_slice(&[value]))
    );
    dispatch!(primitive, byte, value, u8, Bytes, {
        assert!(value.len() == 1);
        value.as_ref()[0]
    });
    dispatch!(primitive, char, value, char, String, {
        let value = value.as_str();
        assert!(value.len() == 1);
        value.chars().nth(0).unwrap()
    });
    dispatch!(primitive, string, value, ParString, String, value);

    // this one is a one-off because it requires subtyping.
    pub async fn bytes(self) -> Bytes {
        match self {
            Handle::Old(handle) => handle.bytes().await,
            Handle::New(handle) => match handle.primitive().await.unwrap() {
                Primitive::String(e) => e.as_bytes(),
                Primitive::Bytes(e) => e,
                primitive => panic!(
                    "Unexpected primitive in Handle! Expected {}, got {:?}",
                    stringify!($variant),
                    primitive
                ),
            },
        }
    }
    dispatch!(primitive, int, value, BigInt, Int, value);
    dispatch!(primitive, nat, value, BigInt, Int, value);

    pub async fn link(self, dual: Handle) {
        match (self, dual) {
            (Handle::Old(a), Handle::Old(b)) => a.link(b),
            (Handle::New(a), Handle::New(b)) => a.link_with(b).await,
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

    pub async fn provide_box<Fun, Fut>(self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        match self {
            Handle::Old(handle) => {
                handle.provide_box(move |x| f(Handle::Old(x)));
            }
            Handle::New(handle) => {
                handle
                    .provide_external_closure(move |x| f(Handle::New(x)))
                    .await
            }
        }
    }
}
