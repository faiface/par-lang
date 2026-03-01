use crate::frontend::{PrimitiveType, Type, TypeDefs};
use crate::frontend_impl::language::LocalName;
use crate::location::Span;
use arcstr::ArcStr;
use bytes::Bytes;
use num_bigint::BigInt;
use par_runtime::primitive::ParString;
use par_runtime::readback::Handle;

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
