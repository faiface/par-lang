use std::{
    future::Future,
    sync::{Arc, Mutex},
};

use arcstr::Substr;
use futures::channel::oneshot;
use num_bigint::BigInt;

use crate::{
    location::Span,
    par::{
        primitive::Primitive,
        types::{PrimitiveType, Type, TypeDefs},
    },
};

use super::{compiler::TypedTree, Net, Tree};

pub struct Handle {
    net: Arc<Mutex<Net>>,
    tree: Option<Tree>,
}

pub struct TypedHandle {
    type_defs: TypeDefs,
    net: Arc<Mutex<Net>>,
    tree: TypedTree,
}

pub enum TypedReadback {
    Nat(BigInt),
    Int(BigInt),
    String(Substr),
    Char(char),

    NatRequest(Box<dyn Send + FnOnce(BigInt)>),
    IntRequest(Box<dyn Send + FnOnce(BigInt)>),
    StringRequest(Box<dyn Send + FnOnce(Substr)>),
    CharRequest(Box<dyn Send + FnOnce(char)>),

    Times(TypedHandle, TypedHandle),
    Par(TypedHandle, TypedHandle),
    Either(String, TypedHandle),
    Choice(Vec<String>, Box<dyn Send + FnOnce(&str) -> TypedHandle>),

    Break,
    Continue,
}

impl Handle {
    pub fn new(net: Arc<Mutex<Net>>, tree: Tree) -> Self {
        Self {
            net,
            tree: Some(tree),
        }
    }

    pub fn concurrently<F>(self, f: impl FnOnce(Self) -> F)
    where
        F: 'static + Send + Future<Output = ()>,
    {
        self.net
            .clone()
            .lock()
            .expect("lock failed")
            .spawn(Box::pin(f(self)));
    }

    pub async fn nat(self) -> BigInt {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::IntRequest(tx), self.tree.unwrap());
            locked.notify_reducer();
            rx
        };
        let value = rx.await.expect("sender dropped");
        assert!(value >= BigInt::ZERO);
        value
    }

    pub fn provide_nat(self, value: BigInt) {
        assert!(value >= BigInt::ZERO);
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Int(value)), self.tree.unwrap());
        locked.notify_reducer();
    }

    pub async fn int(self) -> BigInt {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::IntRequest(tx), self.tree.unwrap());
            locked.notify_reducer();
            rx
        };
        rx.await.expect("sender dropped")
    }

    pub fn provide_int(self, value: BigInt) {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Int(value)), self.tree.unwrap());
        locked.notify_reducer();
    }

    pub async fn string(self) -> Substr {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::StringRequest(tx), self.tree.unwrap());
            locked.notify_reducer();
            rx
        };
        rx.await.expect("sender dropped")
    }

    pub fn provide_string(self, value: Substr) {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(
            Tree::Primitive(Primitive::String(value)),
            self.tree.unwrap(),
        );
        locked.notify_reducer();
    }

    pub async fn char(self) -> char {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::CharRequest(tx), self.tree.unwrap());
            locked.notify_reducer();
            rx
        };
        rx.await.expect("sender dropped")
    }

    pub fn provide_char(self, value: char) {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Char(value)), self.tree.unwrap());
        locked.notify_reducer();
    }

    pub fn send(&mut self) -> Self {
        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(
            Tree::Con(Box::new(u1), Box::new(t1)),
            self.tree.take().unwrap(),
        );
        locked.notify_reducer();
        drop(locked);

        self.tree = Some(u0);
        Self {
            net: Arc::clone(&self.net),
            tree: Some(t0),
        }
    }

    pub fn receive(&mut self) -> Self {
        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(
            Tree::Con(Box::new(u1), Box::new(t1)),
            self.tree.take().unwrap(),
        );
        locked.notify_reducer();
        drop(locked);

        self.tree = Some(u0);
        Self {
            net: Arc::clone(&self.net),
            tree: Some(t0),
        }
    }

    pub fn signal(&mut self, index: u16, size: u16) {
        let mut locked = self.net.lock().expect("lock failed");
        let (a0, a1) = locked.create_wire();
        locked.link(
            Tree::Signal(index, size, Box::new(a1)),
            self.tree.take().unwrap(),
        );
        locked.notify_reducer();
        drop(locked);

        self.tree = Some(a0);
    }

    pub async fn case(&mut self, size: u16) -> u16 {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::SignalRequest(tx), self.tree.take().unwrap());
            locked.notify_reducer();
            rx
        };

        let (index, actual_size, tree) = rx.await.expect("sender dropped");
        assert_eq!(size, actual_size);

        self.tree = Some(*tree);
        index
    }

    pub fn break_(self) {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.unwrap());
        locked.notify_reducer();
    }

    pub fn continue_(self) {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.unwrap());
        locked.notify_reducer();
    }
}

impl TypedHandle {
    pub fn new(type_defs: TypeDefs, net: Arc<Mutex<Net>>, tree: TypedTree) -> Self {
        Self {
            type_defs,
            net,
            tree,
        }
    }

    pub fn net(&self) -> Arc<Mutex<Net>> {
        Arc::clone(&self.net)
    }

    pub async fn readback(mut self) -> TypedReadback {
        self.prepare_for_readback();

        match &self.tree.ty {
            Type::Primitive(_, PrimitiveType::Nat) => TypedReadback::Nat(self.nat().await),
            Type::Primitive(_, PrimitiveType::Int) => TypedReadback::Int(self.int().await),
            Type::Primitive(_, PrimitiveType::String) => TypedReadback::String(self.string().await),
            Type::Primitive(_, PrimitiveType::Char) => TypedReadback::Char(self.char().await),

            Type::DualPrimitive(_, PrimitiveType::Nat) => {
                TypedReadback::NatRequest(Box::new(move |value| self.provide_nat(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Int) => {
                TypedReadback::IntRequest(Box::new(move |value| self.provide_int(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::String) => {
                TypedReadback::StringRequest(Box::new(move |value| self.provide_string(value)))
            }
            Type::DualPrimitive(_, PrimitiveType::Char) => {
                TypedReadback::CharRequest(Box::new(move |value| self.provide_char(value)))
            }

            Type::Pair(_, _, _) => {
                let (t_handle, u_handle) = self.receive();
                TypedReadback::Times(t_handle, u_handle)
            }

            Type::Function(_, _, _) => {
                let (t_handle, u_handle) = self.send();
                TypedReadback::Par(t_handle, u_handle)
            }

            Type::Either(_, _) => {
                let (signal, handle) = self.case().await;
                TypedReadback::Either(signal, handle)
            }

            Type::Choice(_, branches) => TypedReadback::Choice(
                branches.keys().map(|k| k.string.clone()).collect(),
                Box::new(move |signal| self.signal(signal)),
            ),

            Type::Break(_) => {
                self.continue_();
                TypedReadback::Break
            }

            Type::Continue(_) => {
                self.break_();
                TypedReadback::Continue
            }

            typ => panic!("Unsupported type for readback: {:?}", typ),
        }
    }

    pub async fn nat(mut self) -> BigInt {
        self.prepare_for_readback();
        let Type::Primitive(_, PrimitiveType::Nat) = self.tree.ty else {
            panic!("Incorrect type for `nat`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::IntRequest(tx), self.tree.tree);
            locked.notify_reducer();
            rx
        };

        let value = rx.await.expect("sender dropped");
        assert!(value >= BigInt::ZERO);
        value
    }

    pub fn provide_nat(mut self, value: BigInt) {
        assert!(value >= BigInt::ZERO);

        self.prepare_for_readback();
        let Type::DualPrimitive(_, PrimitiveType::Nat | PrimitiveType::Int) = self.tree.ty else {
            panic!("Incorrect type for `provide_nat`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Int(value)), self.tree.tree);
        locked.notify_reducer();
    }

    pub async fn int(mut self) -> BigInt {
        self.prepare_for_readback();
        let Type::Primitive(_, PrimitiveType::Int | PrimitiveType::Nat) = self.tree.ty else {
            panic!("Incorrect type for `int`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::IntRequest(tx), self.tree.tree);
            locked.notify_reducer();
            rx
        };

        rx.await.expect("sender dropped")
    }

    pub fn provide_int(mut self, value: BigInt) {
        self.prepare_for_readback();
        let Type::DualPrimitive(_, PrimitiveType::Int) = self.tree.ty else {
            panic!("Incorrect type for `provide_int`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Int(value)), self.tree.tree);
        locked.notify_reducer();
    }

    pub async fn string(mut self) -> Substr {
        self.prepare_for_readback();
        let Type::Primitive(_, PrimitiveType::String) = self.tree.ty else {
            panic!("Incorrect type for `string`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::StringRequest(tx), self.tree.tree);
            locked.notify_reducer();
            rx
        };

        rx.await.expect("sender dropped")
    }

    pub fn provide_string(mut self, value: Substr) {
        self.prepare_for_readback();
        let Type::DualPrimitive(_, PrimitiveType::String) = self.tree.ty else {
            panic!("Incorrect type for `provide_string`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::String(value)), self.tree.tree);
        locked.notify_reducer();
    }

    pub async fn char(mut self) -> char {
        self.prepare_for_readback();
        let Type::Primitive(_, PrimitiveType::Char) = self.tree.ty else {
            panic!("Incorrect type for `char`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::CharRequest(tx), self.tree.tree);
            locked.notify_reducer();
            rx
        };

        rx.await.expect("sender dropped")
    }

    pub fn provide_char(mut self, value: char) {
        self.prepare_for_readback();
        let Type::DualPrimitive(_, PrimitiveType::Char) = self.tree.ty else {
            panic!("Incorrect type for `provide_char`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(Primitive::Char(value)), self.tree.tree);
        locked.notify_reducer();
    }

    pub fn send(mut self) -> (Self, Self) {
        self.prepare_for_readback();
        let Type::Function(_, t, u) = self.tree.ty else {
            panic!("Incorrect type for `send`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(Tree::Con(Box::new(u1), Box::new(t1)), self.tree.tree);
        locked.notify_reducer();
        drop(locked);

        let t_handle = Self {
            type_defs: self.type_defs.clone(),
            net: Arc::clone(&self.net),
            tree: t0.with_type(t.dual(Span::None)),
        };
        let u_handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: u0.with_type(*u),
        };

        (t_handle, u_handle)
    }

    pub fn receive(mut self) -> (Self, Self) {
        self.prepare_for_readback();
        let Type::Pair(_, t, u) = self.tree.ty else {
            panic!("Incorrect type for `receive`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(Tree::Con(Box::new(u1), Box::new(t1)), self.tree.tree);
        locked.notify_reducer();
        drop(locked);

        let t_handle = Self {
            type_defs: self.type_defs.clone(),
            net: Arc::clone(&self.net),
            tree: t0.with_type(*t),
        };
        let u_handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: u0.with_type(*u),
        };

        (t_handle, u_handle)
    }

    pub fn signal(mut self, chosen: &str) -> Self {
        self.prepare_for_readback();
        let Type::Choice(_, branches) = self.tree.ty else {
            panic!("Incorrect type for `signal`: {:?}", self.tree.ty);
        };
        let size = branches.len() as u16;
        let (index, (_, typ)) = branches
            .into_iter()
            .enumerate()
            .find(|(_, (k, _))| k.string == chosen)
            .unwrap();
        let index = index as u16;

        let mut locked = self.net.lock().expect("lock failed");
        let (a0, a1) = locked.create_wire();
        locked.link(Tree::Signal(index, size, Box::new(a1)), self.tree.tree);
        locked.notify_reducer();
        drop(locked);

        Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: a0.with_type(typ),
        }
    }

    pub async fn case(mut self) -> (String, Self) {
        self.prepare_for_readback();
        let Type::Either(_, branches) = self.tree.ty else {
            panic!("Incorrect type for `case`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::SignalRequest(tx), self.tree.tree);
            locked.notify_reducer();
            rx
        };

        let (index, size, tree) = rx.await.expect("sender dropped");
        assert_eq!(branches.len(), size as usize);
        let (name, typ) = branches.into_iter().skip(index as usize).next().unwrap();

        let handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: tree.with_type(typ),
        };

        (name.string, handle)
    }

    pub fn break_(mut self) {
        self.prepare_for_readback();
        let Type::Continue(_) = self.tree.ty else {
            panic!("Incorrect type for `break`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.tree);
        locked.notify_reducer();
    }

    pub fn continue_(mut self) {
        self.prepare_for_readback();
        let Type::Break(_) = self.tree.ty else {
            panic!("Incorrect type for `break`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.tree);
        locked.notify_reducer();
    }
}

impl TypedHandle {
    fn prepare_for_readback(&mut self) {
        self.tree.ty = expand_type(
            std::mem::replace(&mut self.tree.ty, Type::Break(Default::default())),
            &self.type_defs,
        );
    }
}

pub fn expand_type(typ: Type, type_defs: &TypeDefs) -> Type {
    let mut typ = typ;
    loop {
        typ = match typ {
            Type::Name(span, name, args) => type_defs.get(&span, &name, &args).unwrap(),
            Type::DualName(span, name, args) => type_defs.get_dual(&span, &name, &args).unwrap(),
            Type::Recursive {
                span: _,
                asc,
                label,
                body,
            } => Type::expand_recursive(&asc, &label, &body, &type_defs).unwrap(),
            Type::Iterative {
                span: _,
                asc,
                label,
                body,
            } => Type::expand_iterative(&asc, &label, &body, &type_defs).unwrap(),
            typ => break typ,
        };
    }
}
