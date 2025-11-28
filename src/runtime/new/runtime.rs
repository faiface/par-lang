use futures::io::FillBuf;

use super::arena::*;
use std::any::Any;
use std::sync::{Arc, Mutex};

pub type PackagePtr<'s> = Index<'s, Package<'s>>;
pub type GlobalPtr<'s> = Index<'s, Global<'s>>;
pub type Str<'s> = Arc<str>;

#[derive(Clone)]
struct Instance<'s> {
    vars: Arc<Mutex<Box<[Option<Value<'s>>]>>>,
}

type UserValue<'s> = Box<dyn Any>;

#[derive(Clone)]
pub struct Package<'s> {
    root: Global<'s>,
    captures: Global<'s>,
    num_vars: usize,
}

#[derive(Clone)]
pub enum Global<'s> {
    Variable(usize),
    LinearPackage(PackagePtr<'s>, GlobalPtr<'s>),
    GlobalPackage(PackagePtr<'s>, GlobalPtr<'s>),
    Destruct(GlobalCont<'s>),
    Value(GlobalValue<'s>),
    Fanout(IndexSlice<'s, Global<'s>>),
}

pub enum Value<'s> {
    User(Box<dyn Any>),
    ShareHole(Arc<Mutex<SharedHole<'s>>>),
    Shared(Shared<'s>),
    Global(Instance<'s>, Global<'s>),
}

#[derive(Clone)]
pub enum Shared<'s> {
    Async(Arc<Mutex<SharedHole<'s>>>),
    Sync(Arc<SyncShared<'s>>),
}

#[derive(Clone)]
pub enum SyncShared<'s> {
    Break,
    Pair(Shared<'s>, Shared<'s>),
    Either(Str<'s>, Shared<'s>),
    Package(PackagePtr<'s>, Shared<'s>),
}

#[derive(Clone)]
pub enum GlobalValue<'s> {
    Break,
    Pair(GlobalPtr<'s>, GlobalPtr<'s>),
    Either(Str<'s>, GlobalPtr<'s>),
}

#[derive(Clone)]
pub enum GlobalCont<'s> {
    Continue,
    Par(GlobalPtr<'s>, GlobalPtr<'s>),
}

enum SharedHole<'s> {
    Filled(SyncShared<'s>),
    Unfilled(Vec<Value<'s>>),
}
pub struct Net<'s> {
    arena: IndexedArena<'s>,
    redexes: Vec<(Value<'s>, Value<'s>)>,
}

macro_rules! sym {
    ($a: pat, $b: pat) => {
        ($a, $b) | ($b, $a)
    };
}

impl<'s> Net<'s> {
    fn set_var(&mut self, instance: Instance<'s>, index: usize, value: Value<'s>) {
        let mut lock = instance.vars.lock().unwrap();
        let slot = lock.get_mut(index).unwrap();
        match slot {
            Some(..) => {
                self.link(slot.take().unwrap(), value);
            }
            None => {
                *slot = Some(value);
            }
        }
    }
    fn link(&mut self, a: Value<'s>, b: Value<'s>) {
        self.redexes.push((a, b));
    }
    fn instantiate(&mut self, package: PackagePtr<'s>) -> (Value<'s>, Value<'s>) {
        let package = self.arena.get(package);
        let instance = Instance {
            vars: Arc::new(Mutex::new(
                Vec::from_iter(std::iter::from_fn(|| Some(None)).take(package.num_vars))
                    .into_boxed_slice(),
            )),
        };
        (
            Value::Global(instance.clone(), package.root.clone()),
            Value::Global(instance.clone(), package.captures.clone()),
        )
    }
    fn share(&mut self, a: Value<'s>) -> Option<Shared<'s>> {
        match a {
            Value::User(any) => None,
            Value::ShareHole(mutex) => None,
            Value::Shared(shared) => Some(shared),
            Value::Global(instance, Global::Destruct(..)) => None,
            Value::Global(instance, Global::LinearPackage(..)) => None,
            Value::Global(instance, Global::Fanout(..)) => None,
            Value::Global(instance, Global::GlobalPackage(package, captures)) => {
                let captures = Value::Global(instance, self.arena.get(captures).clone());
                let captures = self.share(captures)?;
                Some(Shared::Sync(Arc::new(SyncShared::Package(
                    package, captures,
                ))))
            }
            Value::Global(instance, Global::Value(value)) => Some(
                (Shared::Sync(Arc::new(match value {
                    GlobalValue::Break => (SyncShared::Break),
                    GlobalValue::Pair(a, b) => SyncShared::Pair(
                        self.share(Value::Global(instance.clone(), self.arena.get(a).clone()))?,
                        self.share(Value::Global(instance, self.arena.get(b).clone()))?,
                    ),
                    GlobalValue::Either(name, a) => {
                        (SyncShared::Either(
                            name,
                            self.share(Value::Global(instance, self.arena.get(a).clone()))?,
                        ))
                    }
                }))),
            ),
            Value::Global(instance, Global::Variable(id)) => {
                let mut lock = instance.vars.lock().unwrap();
                let slot = &mut lock[id];
                if let Some(slot) = slot.take() {
                    drop(lock);
                    self.share(slot)
                } else {
                    let state = Arc::new(Mutex::new(SharedHole::Unfilled(vec![])));
                    let hole = Value::ShareHole(state.clone());
                    slot.replace(hole);
                    Some(Shared::Async(state))
                }
            }
        }
    }
    pub fn reduce(&mut self) -> Option<(UserValue<'s>, Value<'s>)> {
        while let Some((a, b)) = self.redexes.pop() {
            if let Some(v) = self.interact(a, b) {
                return Some(v);
            }
        }
        None
    }
    fn link_with_hole(&mut self, hole: &mut SharedHole<'s>, value: Value<'s>) {
        match hole {
            SharedHole::Filled(sync_shared_value) => {
                self.link(
                    Value::Shared(Shared::Sync(Arc::new(sync_shared_value.clone()))),
                    value,
                );
            }
            SharedHole::Unfilled(values) => values.push(value),
        }
    }
    fn interact(&mut self, a: Value<'s>, b: Value<'s>) -> Option<(UserValue<'s>, Value<'s>)> {
        match (a, b) {
            sym!(Value::Global(instance, Global::Variable(index)), value) => {
                self.set_var(instance, index, value)
            }
            sym!(Value::User(user), other) => {
                return Some((user, other));
            }
            sym!(
                Value::Global(instance, Global::LinearPackage(package, captures_in)),
                other
            ) => {
                let (root, captures) = self.instantiate(package);
                let captures_in = self.arena.get(captures_in).clone();
                self.link(captures, Value::Global(instance, captures_in));
                self.link(root, other);
            }
            sym!(Value::Global(instance, Global::Fanout(destinations)), other) => {
                let other = self.share(other).unwrap();
                let destinations = self.arena.get_slice(destinations);
                for dest in destinations {
                    self.redexes.push((
                        Value::Global(instance.clone(), dest.clone()),
                        Value::Shared(other.clone()),
                    ));
                }
            }
            sym!(Value::ShareHole(state), other) => {
                let other = self.share(other).unwrap();
                match other {
                    Shared::Async(other) => {
                        self.link_with_hole(&mut other.lock().unwrap(), Value::ShareHole(state))
                    }
                    Shared::Sync(value) => {
                        let mut lock = state.lock().unwrap();
                        let SharedHole::Unfilled(values) =
                            core::mem::replace(&mut *lock, SharedHole::Filled((*value).clone()))
                        else {
                            unreachable!()
                        };
                        for i in values {
                            self.link(i, Value::Shared(Shared::Sync(value.clone())));
                        }
                    }
                }
            }
            sym!(
                Value::Global(instance, Global::GlobalPackage(package, captures_in)),
                other
            ) => {
                let (root, captures) = self.instantiate(package);
                let captures_in = self.arena.get(captures_in).clone();
                self.link(captures, Value::Global(instance, captures_in));
                self.link(root, other);
            }
            sym!(Value::Shared(Shared::Sync(x)), other)
                if matches!(&*x, SyncShared::Package(..)) =>
            {
                let SyncShared::Package(package, captures_in) = &*x else {
                    unreachable!()
                };
                let (root, captures) = self.instantiate(package.clone());
                self.link(captures, Value::Shared(captures_in.clone()));
                self.link(root, other);
            }
            sym!(Value::Shared(Shared::Async(state)), other) => {
                let mut lock = state.lock().unwrap();
                self.link_with_hole(&mut *lock, other);
            }
            sym!(Value::Shared(_), Value::Global(_, Global::Value(_)))
            | (Value::Shared(_), Value::Shared(_))
            | (Value::Global(_, Global::Value(_)), Value::Global(_, Global::Value(_))) => {
                unimplemented!("Value-value!");
            }
            _ => todo!(),
        };
        None
    }
}
