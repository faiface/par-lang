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
    Structural(StructuralCode),
    Logical(LogicalCode),
}

#[derive(Clone, Copy)]
pub enum StructuralCode {
    Package(PackageI),
    Variable(VarI),
    Erase,
    Duplicate(CodeI),
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
        let (pattern1, pattern2) = match (left, right) {
            (Value::Resolver(state), value) | (value, Value::Resolver(state)) => {
                self.resolve(state, value);
                return;
            }

            (Value::Code(alloc1, CodeI(c1)), Value::Code(alloc2, CodeI(c2))) => {
                match (&self.codes[c1], &self.codes[c2]) {
                    (Code::Structural(code1), _) => {
                        self.run_structural_code(
                            alloc1,
                            c1,
                            *code1,
                            Value::Code(alloc2, CodeI(c2)),
                        );
                        return;
                    }

                    (_, Code::Structural(code2)) => {
                        self.run_structural_code(
                            alloc2,
                            c2,
                            *code2,
                            Value::Code(alloc1, CodeI(c1)),
                        );
                        return;
                    }

                    (Code::Logical(code1), Code::Logical(code2)) => (
                        code1.into_pattern(alloc1, c1),
                        code2.into_pattern(alloc2, c2),
                    ),
                }
            }

            (Value::Code(alloc, CodeI(c)), Value::Shared(shared))
            | (Value::Shared(shared), Value::Code(alloc, CodeI(c))) => match &self.codes[c] {
                Code::Structural(code) => {
                    self.run_structural_code(alloc, c, *code, Value::Shared(shared));
                    return;
                }

                Code::Logical(code) => {
                    let sync = match shared {
                        SharedValue::Sync(sync) => (*sync).clone(),
                        SharedValue::Async(state) => match &*state.read().unwrap() {
                            SharedValueState::Pending(waiters) => {
                                waiters.lock().unwrap().push(Value::Code(alloc, CodeI(c)));
                                return;
                            }
                            SharedValueState::Resolved(sync) => sync.clone(),
                        },
                    };
                    (code.into_pattern(alloc, c), sync.into_pattern())
                }
            },

            (Value::Shared(_), Value::Shared(_)) => {
                panic!("two shared values cannot interact");
            }
        };

        match (pattern1, pattern2) {
            (Pattern::Unit, Pattern::Continuation) | (Pattern::Continuation, Pattern::Unit) => {
                return;
            }

            (Pattern::Pair(pair1, pair2), Pattern::Par(par1, par2))
            | (Pattern::Par(par1, par2), Pattern::Pair(pair1, pair2)) => {
                self.push(pair2, par2);
                self.push(pair1, par1);
                return;
            }

            (Pattern::Either(signal, payload), Pattern::Choice(table, context))
            | (Pattern::Choice(table, context), Pattern::Either(signal, payload)) => {
                todo!()
            }

            _ => panic!("invalid interaction"),
        }
    }

    #[inline(always)]
    fn run_structural_code(
        &mut self,
        alloc: Allocation,
        c: usize,
        code: StructuralCode,
        value: Value,
    ) {
        match code {
            StructuralCode::Package(p) => {
                self.push(self.allocate(p), value);
                return;
            }

            StructuralCode::Variable(VarI(v)) => {
                let mut slot = alloc.variables[v].lock().unwrap();
                match slot.take() {
                    Some(value1) => self.push(value1, value),
                    None => *slot = Some(value),
                }
                return;
            }

            StructuralCode::Erase => {
                let _ = self.into_shared(value);
                return;
            }

            StructuralCode::Duplicate(d) => {
                let shared = self.into_shared(value).value();
                self.push(Value::Code(alloc.clone(), d), Value::Shared(shared.clone()));
                self.push(Value::Code(alloc, CodeI(c + 1)), Value::Shared(shared));
                return;
            }
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
                Code::Structural(code) => match code {
                    StructuralCode::Variable(VarI(v)) => {
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

                    StructuralCode::Erase => panic!("cannot share erase"),
                    StructuralCode::Duplicate(_) => panic!("cannot share duplicate"),

                    StructuralCode::Package(p) => self.into_shared(self.allocate(*p)),
                },

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

enum Pattern<'a> {
    Unit,
    Continuation,
    Pair(Value, Value),
    Par(Value, Value),
    Either(ArcStr, Value),
    Choice(&'a HashMap<ArcStr, PackageI>, Value),
}

impl LogicalCode {
    #[inline(always)]
    fn into_pattern(&self, alloc: Allocation, c: usize) -> Pattern<'_> {
        match self {
            Self::Unit => Pattern::Unit,
            Self::Continuation => Pattern::Continuation,

            Self::Pair(CodeI(d)) => Pattern::Pair(
                Value::Code(alloc.clone(), CodeI(c + 1)),
                Value::Code(alloc, CodeI(*d)),
            ),

            Self::Par(CodeI(d)) => Pattern::Par(
                Value::Code(alloc.clone(), CodeI(c + 1)),
                Value::Code(alloc, CodeI(*d)),
            ),

            Self::Either(signal) => {
                Pattern::Either(signal.clone(), Value::Code(alloc, CodeI(c + 1)))
            }

            Self::Choice(table) => Pattern::Choice(table, Value::Code(alloc, CodeI(c + 1))),
        }
    }
}

impl SyncSharedValue {
    #[inline(always)]
    fn into_pattern(self) -> Pattern<'static> {
        match self {
            Self::Unit => Pattern::Unit,

            Self::Pair(shared1, shared2) => {
                Pattern::Pair(Value::Shared(shared1), Value::Shared(shared2))
            }

            Self::Either(signal, shared) => Pattern::Either(signal, Value::Shared(shared)),
        }
    }
}
