use arcstr::{ArcStr, Substr};
use futures::channel::{mpsc, oneshot};
use num_bigint::BigInt;
use std::sync::atomic::AtomicUsize;
use std::{
    future::Future,
    sync::{Arc, Mutex},
};

use crate::{
    location::Span,
    par::{
        language::LocalName,
        primitive::Primitive,
        types::{PrimitiveType, Type, TypeDefs},
    },
};

use super::{compiler::TypedTree, Net, Tree};

pub(crate) mod private {
    use crate::icombs::net::ReducerMessage;
    use crate::icombs::Net;
    use futures::channel::mpsc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::{Arc, LockResult, Mutex};

    pub struct NetWrapper {
        net: Arc<Mutex<Net>>,
        notify: mpsc::UnboundedSender<crate::icombs::net::ReducerMessage>,
        handle_count: Arc<AtomicUsize>,
    }

    impl NetWrapper {
        pub(crate) fn new(
            net: Arc<Mutex<Net>>,
            notify: mpsc::UnboundedSender<ReducerMessage>,
            handle_count: Arc<AtomicUsize>,
        ) -> Self {
            handle_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            Self {
                net,
                notify,
                handle_count,
            }
        }
        pub(crate) fn lock(&self) -> LockResult<std::sync::MutexGuard<'_, Net>> {
            self.net.lock()
        }

        pub(crate) fn net(&self) -> Arc<Mutex<Net>> {
            Arc::clone(&self.net)
        }
    }

    impl Clone for NetWrapper {
        fn clone(&self) -> Self {
            self.handle_count
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);

            Self {
                net: Arc::clone(&self.net),
                notify: self.notify.clone(),
                handle_count: Arc::clone(&self.handle_count),
            }
        }
    }

    impl Drop for NetWrapper {
        fn drop(&mut self) {
            if self
                .handle_count
                .fetch_sub(1, std::sync::atomic::Ordering::SeqCst)
                == 1
            {
                let res = self.notify.unbounded_send(ReducerMessage::Ping);
                //.expect("Failed to notify reducer");
                if res.is_err() {
                    println!("Warning: Failed to notify reducer on drop of NetWrapper");
                }
            }
        }
    }
}

use crate::icombs::net::ReducerMessage;
use private::NetWrapper;

pub struct Handle {
    net: NetWrapper,
    tree: Option<Tree>,
}

pub struct TypedHandle {
    type_defs: TypeDefs,
    net: NetWrapper,
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
    Either(ArcStr, TypedHandle),
    Choice(Vec<ArcStr>, Box<dyn Send + FnOnce(ArcStr) -> TypedHandle>),

    Break,
    Continue,
}

impl Handle {
    pub fn new(
        net: Arc<Mutex<Net>>,
        notify: mpsc::UnboundedSender<ReducerMessage>,
        handle_count: Arc<AtomicUsize>,
        tree: Tree,
    ) -> Self {
        Self {
            net: NetWrapper::new(net, notify, handle_count),
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

    pub fn provide_box<Fun, Fut>(self, f: Fun)
    where
        Fun: 'static + Send + Sync + Fn(Handle) -> Fut,
        Fut: 'static + Send + Future<Output = ()>,
    {
        let mut locked = self.net.lock().expect("lock failed");
        locked.link(
            Tree::ExternalBox(Arc::new(move |handle| Box::pin(f(handle)))),
            self.tree.unwrap(),
        );
        locked.notify_reducer();
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
            net: self.net.clone(),
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
            net: self.net.clone(),
            tree: Some(t0),
        }
    }

    pub fn signal(&mut self, chosen: ArcStr) {
        let mut locked = self.net.lock().expect("lock failed");
        let (a0, a1) = locked.create_wire();
        locked.link(
            Tree::Signal(chosen, Box::new(a1)),
            self.tree.take().unwrap(),
        );
        locked.notify_reducer();
        drop(locked);

        self.tree = Some(a0);
    }

    pub async fn case(&mut self) -> ArcStr {
        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::SignalRequest(tx), self.tree.take().unwrap());
            locked.notify_reducer();
            rx
        };

        let (chosen, tree) = rx.await.expect("sender dropped");
        self.tree = Some(*tree);
        chosen
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

    pub fn link(self, handle: Handle) {
        let net = self.net.clone();
        let mut locked = net.lock().expect("lock failed");
        locked.link(handle.tree.unwrap(), self.tree.unwrap());
        locked.notify_reducer();
    }
}

impl TypedHandle {
    pub fn new(
        type_defs: TypeDefs,
        net: Arc<Mutex<Net>>,
        notify: mpsc::UnboundedSender<ReducerMessage>,
        handle_count: Arc<AtomicUsize>,
        tree: TypedTree,
    ) -> Self {
        Self {
            type_defs,
            net: NetWrapper::new(net, notify, handle_count),
            tree,
        }
    }

    pub fn from_wrapper(type_defs: TypeDefs, net: NetWrapper, tree: TypedTree) -> Self {
        Self {
            type_defs,
            net,
            tree,
        }
    }

    pub fn net(&self) -> Arc<Mutex<Net>> {
        self.net.net()
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
                let (chosen, handle) = self.case().await;
                TypedReadback::Either(chosen, handle)
            }

            Type::Choice(_, branches) => TypedReadback::Choice(
                branches.keys().map(|k| k.string.clone()).collect(),
                Box::new(move |chosen| self.signal(chosen)),
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
            net: self.net.clone(),
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
            net: self.net.clone(),
            tree: t0.with_type(*t),
        };
        let u_handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: u0.with_type(*u),
        };

        (t_handle, u_handle)
    }

    pub fn signal(mut self, chosen: ArcStr) -> Self {
        self.prepare_for_readback();
        let Type::Choice(_, branches) = self.tree.ty else {
            panic!("Incorrect type for `signal`: {:?}", self.tree.ty);
        };
        let typ = branches
            .get(&LocalName::from(chosen.clone()))
            .cloned()
            .unwrap();

        let mut locked = self.net.lock().expect("lock failed");
        let (a0, a1) = locked.create_wire();
        locked.link(Tree::Signal(chosen, Box::new(a1)), self.tree.tree);
        locked.notify_reducer();
        drop(locked);

        Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: a0.with_type(typ),
        }
    }

    pub async fn case(mut self) -> (ArcStr, Self) {
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

        let (chosen, tree) = rx.await.expect("sender dropped");
        let typ = branches
            .get(&LocalName::from(chosen.clone()))
            .cloned()
            .unwrap();

        let handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: tree.with_type(typ),
        };

        (chosen, handle)
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
            Type::Box(_, inner) => expand_type(*inner, type_defs),
            Type::DualBox(_, inner) if inner.is_positive(type_defs).unwrap() => {
                expand_type(inner.clone().dual(Span::None), type_defs)
            }
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
