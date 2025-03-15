use std::{
    ops::BitAnd,
    sync::{Arc, Mutex},
};

use futures::{
    channel::oneshot::channel,
    future::{join, BoxFuture},
    task::{Spawn, SpawnExt},
    FutureExt,
};

use crate::{
    icombs::{
        readback::{ReadbackResult, SharedState},
        Net,
    },
    par::{
        language::Internal,
        parse::{Loc, Program},
        process::Expression,
        types::Type,
    },
    playground::{self, CheckedProgram},
    readback::{NameRequiredTraits, ReadbackState},
    spawn::TokioSpawn,
};

use crate::icombs::Name;

#[derive(Debug, Default, Clone)]
pub enum ReadbackPattern<Name: Clone> {
    #[default]
    Break,
    Continue,
    Send(Box<ReadbackPattern<Name>>, Box<ReadbackPattern<Name>>),
    Receive(Box<ReadbackPattern<Name>>, Box<ReadbackPattern<Name>>),
    Either(Name, Box<ReadbackPattern<Name>>),
    // After choosing this, the rest of the pattern should match this.
    Choice(Name, Box<ReadbackPattern<Name>>),
    Any,
}

#[derive(Debug)]
pub struct TestingResult {
    checked: bool,
}

impl From<bool> for TestingResult {
    fn from(checked: bool) -> Self {
        TestingResult { checked }
    }
}

impl BitAnd for TestingResult {
    type Output = TestingResult;
    fn bitand(self, rhs: Self) -> Self::Output {
        (self.checked & rhs.checked).into()
    }
}

impl FromIterator<TestingResult> for TestingResult {
    fn from_iter<T: IntoIterator<Item = TestingResult>>(iter: T) -> Self {
        iter.into_iter().fold(false.into(), |a, b| a & b)
    }
}

impl From<(TestingResult, TestingResult)> for TestingResult {
    fn from((a, b): (TestingResult, TestingResult)) -> Self {
        a & b
    }
}

// testing works by matching each readback result against a pattern.

pub struct TestingState {
    pub shared: SharedState,
    pub spawner: Arc<dyn Spawn + Send + Sync>,
    pub net: Arc<Mutex<Net>>,
    pub prog: Arc<CheckedProgram>,
}

impl TestingState {
    fn result_to_pattern(&self, result: ReadbackResult) -> BoxFuture<ReadbackPattern<Name>> {
        async move {
            match result {
                ReadbackResult::Break => ReadbackPattern::Break,
                ReadbackResult::Continue => ReadbackPattern::Continue,
                ReadbackResult::Send(left, right) => {
                    let (left, right) = join(
                        self.result_to_pattern(*left),
                        self.result_to_pattern(*right),
                    )
                    .await;
                    ReadbackPattern::Send(Box::new(left), Box::new(right))
                }
                ReadbackResult::Receive(left, right) => {
                    let (left, right) = join(
                        self.result_to_pattern(*left),
                        self.result_to_pattern(*right),
                    )
                    .await;
                    ReadbackPattern::Receive(Box::new(left), Box::new(right))
                }
                ReadbackResult::Either(name, payload) => {
                    ReadbackPattern::Either(name, Box::new(self.result_to_pattern(*payload).await))
                }
                ReadbackResult::Halted(mut tree) => {
                    tree.ty = crate::readback::prepare_type_for_readback(
                        &self.prog.type_defs,
                        tree.ty,
                        &Default::default(),
                    );
                    let res = self.shared.read_with_type(tree).await;
                    self.result_to_pattern(res).await
                }
                ReadbackResult::Expand(mut package) => {
                    let tree = self.shared.expand_once(package.tree).await;
                    self.result_to_pattern(ReadbackResult::Halted(tree.with_type(package.ty)))
                        .await
                }
                a => {
                    todo!("{a:?}")
                }
            }
        }
        .boxed()
    }
    fn matches(
        &self,
        result: ReadbackResult,
        pattern: ReadbackPattern<Name>,
    ) -> BoxFuture<TestingResult> {
        async move {
            match result {
                ReadbackResult::Break => matches!(pattern, ReadbackPattern::Break).into(),
                ReadbackResult::Continue => matches!(pattern, ReadbackPattern::Continue).into(),
                ReadbackResult::Send(left, right) => {
                    if let ReadbackPattern::Send(lp, rp) = pattern {
                        join(
                            self.clone().matches(*left, *lp),
                            self.clone().matches(*right, *rp),
                        )
                        .await
                        .into()
                    } else {
                        false.into()
                    }
                }
                ReadbackResult::Receive(left, right) => {
                    if let ReadbackPattern::Receive(lp, rp) = pattern {
                        join(
                            self.clone().matches(*left, *lp),
                            self.clone().matches(*right, *rp),
                        )
                        .await
                        .into()
                    } else {
                        false.into()
                    }
                }
                ReadbackResult::Either(name, payload) => {
                    let ReadbackPattern::Either(pat_name, pattern_payload) = pattern else {
                        return false.into();
                    };
                    if name != pat_name {
                        return false.into();
                    }
                    self.clone().matches(*payload, *pattern_payload).await
                }
                ReadbackResult::Choice(ctx, options) => {
                    let ReadbackPattern::Choice(name, pattern_payload) = pattern else {
                        return false.into();
                    };
                    let payload = self.shared.choose_choice(name, ctx, options, &self.spawner);
                    self.matches(ReadbackResult::Halted(payload), *pattern_payload)
                        .await
                }
                ReadbackResult::Halted(mut tree) => {
                    tree.ty = crate::readback::prepare_type_for_readback(
                        &self.prog.type_defs,
                        tree.ty,
                        &Default::default(),
                    );
                    let res = self.shared.read_with_type(tree).await;
                    self.matches(res, pattern).await
                }

                ReadbackResult::Expand(mut package) => {
                    let tree = self.shared.expand_once(package.tree).await;
                    self.matches(ReadbackResult::Halted(tree.with_type(package.ty)), pattern)
                        .await
                }
                ReadbackResult::Variable(_) => todo!(),
                _ => todo!(),
            }
        }
        .boxed()
    }
}

pub enum TestPattern {
    Pattern(ReadbackPattern<Name>),
    Code(String),
}

pub struct TestCase {
    code: String,
    ty: String,
    pattern: TestPattern,
}

pub struct TestFamily {
    name: String,
    prelude: String,
    tests: Vec<TestCase>,
}
/// Macro to generate a TestCase
macro_rules! test_case {
    ($code:expr, $ty:expr, code $pattern:expr) => {
        TestCase {
            code: $code.to_string(),
            ty: $ty.to_string(),
            pattern: TestPattern::Code($pattern.to_string()),
        }
    };
    ($code:expr, $ty:expr, pat $pattern:expr) => {
        TestCase {
            code: $code.to_string(),
            ty: $ty.to_string(),
            pattern: TestPattern::Pattern($pat.to_string()),
        }
    };
}

macro_rules! test_family {
    ($name:expr, $prelude:expr, [$($case:tt)*]) => {
        TestFamily {
            name: $name.to_string(),
            prelude: $prelude.to_string(),
            tests: vec![
                $(test_case! $case),*
            ],
        }
    };
}

#[tokio::test]
async fn test_whole_programs() -> Result<(), String> {
    let mut failed = false;
    let test_families = vec![test_family!(
        "hello world",
        include_str!("../examples/sample_ic.par"),
        [
            (".true!", "Bool", code ".true!")
            ("not(true)", "Bool", code "false")
            ("not(false)", "Bool", code "true")
            ("xor(true)(true)", "Bool", code "false")
            ("xor(true)(false)", "Bool", code "true")
            ("xor(false)(true)", "Bool", code "true")
            ("xor(true)(true)", "Bool", code "false")
        ]
    )];
    for i in test_families {
        eprintln!("testing family: {}", i.name);
        use core::fmt::Write;
        // First, build the source code
        let mut source = String::new();
        write!(&mut source, "{}", i.prelude).unwrap();
        for (idx, j) in i.tests.iter().enumerate() {
            write!(&mut source, "dec test_{}: {}\n", idx, j.ty).unwrap();
            write!(&mut source, "def test_{} = {}\n", idx, j.code).unwrap();
            if let TestPattern::Code(code) = &j.pattern {
                write!(&mut source, "dec test_pat_{}: {}\n", idx, j.ty).unwrap();
                write!(&mut source, "def test_pat_{} = {}\n", idx, code).unwrap();
            }
        }
        let compiled = playground::Compiled::from_string(&source).unwrap();
        let checked = compiled.checked.unwrap();
        let ic_compiled = checked.ic_compiled.unwrap();
        let prog = (checked.program.clone());
        let mut spawner = Arc::new(TokioSpawn);
        let mut net = Arc::new(Mutex::new(ic_compiled.create_net()));
        let mut shared = SharedState::with_net(net.clone(), prog.type_defs.clone());
        let (_tx, rx) = channel();
        spawner.spawn(shared.create_net_reducer(rx)).unwrap();
        let mut state = TestingState {
            shared,
            spawner,
            net: net.clone(),
            prog: prog.clone(),
        };
        for (idx, j) in i.tests.into_iter().enumerate() {
            let def_name = format!("test_{}", idx);
            let def_name = Internal::Original(def_name.into());
            let mut tree = ic_compiled.get_with_name(&def_name).unwrap();
            net.lock().unwrap().freshen_variables(&mut tree);
            let pattern = match j.pattern {
                TestPattern::Code(_) => {
                    let def_name = format!("test_pat_{}", idx);
                    let def_name = Internal::Original(def_name.into());
                    let mut tree = ic_compiled.get_with_name(&def_name).unwrap();
                    net.lock().unwrap().freshen_variables(&mut tree);
                    let res = ReadbackResult::Halted(
                        tree.with_type(
                            prog.declarations
                                .get(&def_name)
                                .unwrap()
                                .as_ref()
                                .unwrap()
                                .clone(),
                        ),
                    );
                    state.result_to_pattern(res).await
                }
                TestPattern::Pattern(pat) => pat,
            };
            let testing_result = state
                .matches(
                    ReadbackResult::Halted(
                        tree.with_type(
                            prog.declarations
                                .get(&def_name)
                                .unwrap()
                                .as_ref()
                                .unwrap()
                                .clone(),
                        ),
                    ),
                    pattern,
                )
                .await;
            if testing_result.checked {
                eprintln!("  ok : `{}`", j.code);
            } else {
                eprintln!("  err: `{}`", j.code);
                failed = true;
            }
        }
    }
    if failed {
        Err("Some tests failed!".to_string())
    } else {
        Ok(())
    }
}
