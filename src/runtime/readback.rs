use crate::location::Span;
use crate::par::language::LocalName;
use crate::par::primitive::{ParString, Primitive};
use crate::par::types::{PrimitiveType, Type, TypeDefs};
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

    pub fn receive(&mut self) -> Handle {
        match self {
            Handle::Old(handle) => Handle::Old(handle.receive()),
            Handle::New(handle) => Handle::New(handle.receive()),
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

#[allow(dead_code)]
pub enum TypedReadback {
    Nat(BigInt),
    Int(BigInt),
    String(ParString),
    Char(char),
    Byte(u8),
    Bytes(Bytes),

    NatRequest(Box<dyn Send + FnOnce(BigInt)>),
    IntRequest(Box<dyn Send + FnOnce(BigInt)>),
    StringRequest(Box<dyn Send + FnOnce(ParString)>),
    CharRequest(Box<dyn Send + FnOnce(char)>),
    ByteRequest(Box<dyn Send + FnOnce(u8)>),
    BytesRequest(Box<dyn Send + FnOnce(Bytes)>),

    Times(TypedHandle, TypedHandle),
    Par(TypedHandle, TypedHandle),
    Either(ArcStr, TypedHandle),
    Choice(Vec<ArcStr>, Box<dyn Send + FnOnce(ArcStr) -> TypedHandle>),

    Break,
    Continue,
    Unreadable { typ: Type, handle: TypedHandle },
}

pub struct TypedHandle {
    type_defs: TypeDefs,
    typ: Type,
    handle: Handle,
}

impl TypedHandle {
    pub fn new(type_defs: TypeDefs, typ: Type, handle: Handle) -> Self {
        Self {
            type_defs,
            typ,
            handle,
        }
    }

    pub async fn readback(mut self) -> TypedReadback {
        self.prepare_for_readback();
        let typ = std::mem::replace(&mut self.typ, Type::Break(Span::None));
        match typ {
            Type::Primitive(_, PrimitiveType::Nat) => TypedReadback::Nat(self.handle.nat().await),
            Type::Primitive(_, PrimitiveType::Int) => TypedReadback::Int(self.handle.int().await),
            Type::Primitive(_, PrimitiveType::String) => {
                TypedReadback::String(self.handle.string().await)
            }
            Type::Primitive(_, PrimitiveType::Char) => {
                TypedReadback::Char(self.handle.char().await)
            }
            Type::Primitive(_, PrimitiveType::Byte) => {
                TypedReadback::Byte(self.handle.byte().await)
            }
            Type::Primitive(_, PrimitiveType::Bytes) => {
                TypedReadback::Bytes(self.handle.bytes().await)
            }

            Type::DualPrimitive(_, PrimitiveType::Nat) => {
                let handle = self.handle;
                TypedReadback::NatRequest(Box::new(move |value| handle.provide_nat(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Int) => {
                let handle = self.handle;
                TypedReadback::IntRequest(Box::new(move |value| handle.provide_int(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::String) => {
                let handle = self.handle;
                TypedReadback::StringRequest(Box::new(move |value| handle.provide_string(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Char) => {
                let handle = self.handle;
                TypedReadback::CharRequest(Box::new(move |value| handle.provide_char(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Byte) => {
                let handle = self.handle;
                TypedReadback::ByteRequest(Box::new(move |value| handle.provide_byte(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Bytes) => {
                let handle = self.handle;
                TypedReadback::BytesRequest(Box::new(move |value| handle.provide_bytes(value)))
            }

            Type::Pair(_, t, u, _) => {
                let mut handle = self.handle;
                let t_handle = handle.receive();
                let u_handle = handle;
                TypedReadback::Times(
                    TypedHandle::new(self.type_defs.clone(), *t, t_handle),
                    TypedHandle::new(self.type_defs, *u, u_handle),
                )
            }

            Type::Function(_, t, u, _) => {
                let mut handle = self.handle;
                let t_handle = handle.send();
                let u_handle = handle;
                TypedReadback::Par(
                    TypedHandle::new(self.type_defs.clone(), t.dual(Span::None), t_handle),
                    TypedHandle::new(self.type_defs, *u, u_handle),
                )
            }

            Type::Either(_, branches) => {
                let mut handle = self.handle;
                let chosen = handle.case().await;
                let typ = branches
                    .get(&LocalName::from(chosen.clone()))
                    .cloned()
                    .unwrap();
                TypedReadback::Either(chosen, TypedHandle::new(self.type_defs, typ, handle))
            }

            Type::Choice(_, branches) => {
                let handle = self.handle;
                let type_defs = self.type_defs;
                let signals = branches.keys().map(|k| k.string.clone()).collect();
                TypedReadback::Choice(
                    signals,
                    Box::new(move |chosen| {
                        let mut handle = handle;
                        let typ = branches
                            .get(&LocalName::from(chosen.clone()))
                            .cloned()
                            .unwrap();
                        handle.signal(chosen);
                        TypedHandle::new(type_defs, typ, handle)
                    }),
                )
            }

            Type::Break(_) => {
                self.handle.continue_();
                TypedReadback::Break
            }

            Type::Continue(_) => {
                self.handle.break_();
                TypedReadback::Continue
            }

            typ => TypedReadback::Unreadable { typ, handle: self },
        }
    }

    fn prepare_for_readback(&mut self) {
        self.typ = expand_type(
            std::mem::replace(&mut self.typ, Type::Break(Span::None)),
            &self.type_defs,
        );
    }
}

fn expand_type(typ: Type, type_defs: &TypeDefs) -> Type {
    let mut typ = typ;
    loop {
        typ = match typ {
            Type::Name(span, name, args) => type_defs.get(&span, &name, &args).unwrap(),
            Type::DualName(span, name, args) => type_defs.get_dual(&span, &name, &args).unwrap(),
            Type::Box(_, inner) => expand_type(*inner, type_defs),
            Type::DualBox(_, inner) if inner.is_positive(type_defs).unwrap() => {
                expand_type(inner.clone().dual(Span::None), type_defs)
            }
            Type::Recursive {
                span: _,
                asc,
                label,
                body,
            } => Type::expand_recursive(&asc, &label, &body).unwrap(),
            Type::Iterative {
                span,
                asc,
                label,
                body,
            } => {
                if asc.is_empty() {
                    Type::expand_iterative(&Span::None, &asc, &label, &body).unwrap()
                } else {
                    break Type::Iterative {
                        span,
                        asc,
                        label,
                        body,
                    };
                }
            }
            typ => break typ,
        };
    }
}
