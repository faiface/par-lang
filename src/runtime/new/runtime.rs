use arcstr::ArcStr;
use futures::io::FillBuf;

use super::arena::*;
use std::any::Any;
use std::sync::{Arc, Mutex};

pub type PackagePtr = Index<Package>;
pub type GlobalPtr = Index<Global>;
pub type Str = ArcStr;

#[derive(Clone)]
struct Instance {
    vars: Arc<Mutex<Box<[Option<Value>]>>>,
}

type UserValue = Box<dyn Any>;

#[derive(Clone)]
pub struct Package {
    root: Global,
    captures: Global,
    num_vars: usize,
}

#[derive(Clone)]
pub enum Global {
    Variable(usize),
    LinearPackage(PackagePtr, GlobalPtr),
    GlobalPackage(PackagePtr, GlobalPtr),
    Destruct(GlobalCont),
    Value(GlobalValue),
    Fanout(Index<[Global]>),
}

pub enum Value {
    User(Box<dyn Any>),
    ShareHole(Arc<Mutex<SharedHole>>),
    Shared(Shared),
    Global(Instance, Global),
}

#[derive(Clone)]
pub enum Shared {
    Async(Arc<Mutex<SharedHole>>),
    Sync(Arc<SyncShared>),
}

#[derive(Clone)]
pub enum SyncShared {
    Break,
    Pair(Shared, Shared),
    Either(Str, Shared),
    Package(PackagePtr, Shared),
}

#[derive(Clone)]
pub enum GlobalValue {
    Break,
    Pair(GlobalPtr, GlobalPtr),
    Either(Str, GlobalPtr),
}

#[derive(Clone)]
pub enum GlobalCont {
    Continue,
    Par(GlobalPtr, GlobalPtr),
}

enum SharedHole {
    Filled(SyncShared),
    Unfilled(Vec<Value>),
}
pub struct Net {
    arena: Arena,
    redexes: Vec<(Value, Value)>,
}

macro_rules! sym {
    ($a: pat, $b: pat) => {
        ($a, $b) | ($b, $a)
    };
}

impl Net {
    fn set_var(&mut self, instance: Instance, index: usize, value: Value) {
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
    fn link(&mut self, a: Value, b: Value) {
        self.redexes.push((a, b));
    }
    fn instantiate(&mut self, package: PackagePtr) -> (Value, Value) {
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
    fn share(&mut self, a: Value) -> Option<Shared> {
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
    pub fn reduce(&mut self) -> Option<(UserValue, Value)> {
        while let Some((a, b)) = self.redexes.pop() {
            if let Some(v) = self.interact(a, b) {
                return Some(v);
            }
        }
        None
    }
    fn link_with_hole(&mut self, hole: &mut SharedHole, value: Value) {
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
    fn interact(&mut self, a: Value, b: Value) -> Option<(UserValue, Value)> {
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
                let destinations = self.arena.get(destinations);
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
