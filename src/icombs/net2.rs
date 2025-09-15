use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex, MutexGuard, RwLock},
};

use arcstr::ArcStr;

#[derive(Clone, Copy)]
pub struct PackageI(usize);

#[derive(Clone, Copy)]
pub struct CodeI(usize);

#[derive(Clone, Copy)]
pub struct VarI(usize);

#[derive(Clone)]
pub enum Code {
    Variable(VarI),
    Erase,
    Duplicate(CodeI),
    Distribute(CodeI),
    Package(PackageI),
    Logical(LogicalCode),
}

#[derive(Clone)]
pub enum LogicalCode {
    Unit,
    Continuation,
    Pair(CodeI),
    Par(CodeI),
    Either(ArcStr),
    Choice(Box<HashMap<ArcStr, PackageI>>),
}

#[derive(Clone)]
pub struct Allocation {
    variables: Arc<[Mutex<Option<Value>>]>,
}

pub enum Value {
    Code(Allocation, CodeI),
    Shared(SharedValue),
    Resolver(Arc<RwLock<SharedValueState>>),
}

#[derive(Clone)]
pub enum SharedValue {
    Sync(Arc<SyncSharedValue>),
    Async(Arc<RwLock<SharedValueState>>),
}

#[derive(Clone)]
pub enum SyncSharedValue {
    Unit,
    Pair(SharedValue, SharedValue),
    Either(ArcStr, SharedValue),
}

pub enum SharedValueState {
    Pending(Mutex<Vec<Value>>),
    Resolved(SyncSharedValue),
}

pub struct Package {
    number_of_variables: usize,
    code_index: CodeI,
}

pub struct Runtime {
    packages: Vec<Package>,
    codes: Vec<Code>,
    redexes: VecDeque<(Value, Value)>,
    scheduling: usize,
}

impl Runtime {
    fn interact(&mut self, left: Value, right: Value) {
        match (left, right) {
            (Value::Resolver(state), value) | (value, Value::Resolver(state)) => {
                self.resolve(state, value);
                return;
            }

            (Value::Shared(shared), value) | (value, Value::Shared(shared)) => {
                match shared {
                    SharedValue::Sync(sync) => todo!(),
                    SharedValue::Async(state) => match &*state.read().unwrap() {
                        SharedValueState::Pending(waiters) => {
                            waiters.lock().unwrap().push(value);
                            return;
                        }
                        SharedValueState::Resolved(sync) => todo!(),
                    }
                }
            }

            _ => panic!(),
        }
    }

    #[inline(always)]
    fn push(&mut self, left: Value, right: Value) {
        if self.scheduling > 0 {
            self.scheduling -= 1;
            self.redexes.push_front((left, right));
        } else {
            self.scheduling = 128;
            self.redexes.push_back((left, right));
        }
    }

    #[inline(always)]
    fn allocate(&self, package_index: PackageI) -> Value {
        let pkg = &self.packages[package_index.0];
        let mut variables = Vec::with_capacity(pkg.number_of_variables);
        for _ in 0..pkg.number_of_variables {
            variables.push(Mutex::new(None));
        }
        let variables = Arc::from(variables);
        Value::Code(Allocation { variables }, pkg.code_index)
    }

    #[inline(always)]
    fn resolve(&mut self, state: Arc<RwLock<SharedValueState>>, value: Value) {
        let sync = match self.into_shared(value) {
            IntoSharedResult::Created(sync) => sync,
            IntoSharedResult::Shared(SharedValue::Sync(sync)) => (*sync).clone(),

            IntoSharedResult::Shared(SharedValue::Async(other_state)) => {
                match &*other_state.read().unwrap() {
                    SharedValueState::Pending(other_waiters) => {
                        other_waiters.lock().unwrap().push(Value::Resolver(state));
                        return;
                    }
                    SharedValueState::Resolved(sync) => sync.clone(),
                }
            }
        };

        let mut locked_state = state.write().unwrap();
        let SharedValueState::Pending(waiters) = &mut *locked_state else {
            panic!("async shared value resolved twice");
        };

        for waiter in waiters.lock().unwrap().drain(..) {
            let state = state.clone();
            self.push(waiter, Value::Shared(SharedValue::Async(state)));
        }

        *locked_state = SharedValueState::Resolved(sync);
    }

    fn into_shared(&self, value: Value) -> IntoSharedResult {
        match value {
            Value::Code(alloc, CodeI(c)) => match &self.codes[c] {
                Code::Variable(VarI(v)) => {
                    let mut slot = alloc.variables[*v].lock().unwrap();
                    match slot.take() {
                        Some(value) => self.into_shared(value),
                        None => {
                            let state = Arc::new(RwLock::new(SharedValueState::Pending(
                                Mutex::new(Vec::new()),
                            )));
                            *slot = Some(Value::Resolver(state.clone()));
                            IntoSharedResult::Shared(SharedValue::Async(state))
                        }
                    }
                }

                Code::Erase => panic!("cannot share erase"),
                Code::Duplicate(_) => panic!("cannot share duplicate"),
                Code::Distribute(_) => panic!("cennot share distribute"),

                Code::Package(p) => self.into_shared(self.allocate(*p)),

                Code::Logical(code) => IntoSharedResult::Created(match code {
                    LogicalCode::Unit => SyncSharedValue::Unit,

                    LogicalCode::Continuation => panic!("cannot share continuation"),

                    LogicalCode::Pair(CodeI(d)) => {
                        let left = self.into_shared(Value::Code(alloc.clone(), CodeI(c + 1)));
                        let right = self.into_shared(Value::Code(alloc, CodeI(*d)));
                        SyncSharedValue::Pair(left.value(), right.value())
                    }

                    LogicalCode::Par(_) => panic!("cannot share par"),

                    LogicalCode::Either(signal) => {
                        let payload = self.into_shared(Value::Code(alloc, CodeI(c + 1)));
                        SyncSharedValue::Either(signal.clone(), payload.value())
                    }

                    LogicalCode::Choice(_) => panic!("cannot share choice"),
                }),
            },

            Value::Shared(shared) => IntoSharedResult::Shared(shared),

            Value::Resolver(_) => panic!("cannot share resolver"),
        }
    }
}

enum IntoSharedResult {
    Created(SyncSharedValue),
    Shared(SharedValue),
}

impl IntoSharedResult {
    #[inline(always)]
    fn value(self) -> SharedValue {
        match self {
            Self::Created(sync) => SharedValue::Sync(Arc::new(sync)),
            Self::Shared(shared) => shared,
        }
    }
}
