use std::sync::{mpsc, Arc};

use par_core::{
    frontend::{language::GlobalName, process, Module, Type, TypeDef},
    runtime::Handle,
    source::Span,
    testing::AssertionResult,
};

pub(crate) fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef {
            span: Span::None,
            name: GlobalName::external(Some("Test"), "Test"),
            params: vec![],
            typ: Type::iterative_box_choice(
                None,
                vec![
                    (
                        "assert",
                        Type::function(
                            Type::string(),
                            Type::function(
                                Type::either(vec![
                                    ("true", Type::break_()),
                                    ("false", Type::break_()),
                                ]),
                                Type::self_(None),
                            ),
                        ),
                    ),
                    (
                        "id",
                        Type::pair(
                            Type::forall("a", Type::function(Type::var("a"), Type::var("a"))),
                            Type::self_(None),
                        ),
                    ),
                    (
                        "leak",
                        Type::pair(
                            Type::forall("a", Type::function(Type::var("a"), Type::break_())),
                            Type::self_(None),
                        ),
                    ),
                    ("done", Type::break_()),
                ],
            ),
        }],
        declarations: vec![],
        definitions: vec![],
    }
}

pub async fn provide_test(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    provide_test_inner(handle, sender);
}

fn provide_test_inner(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    handle.provide_box(move |mut handle| {
        let sender = sender.clone();
        async move {
            match handle.case().await.as_str() {
                "assert" => {
                    let description = handle.receive().string().await.as_str().to_string();
                    println!("{}", description);
                    let mut bool_handle = handle.receive();

                    let passed = match bool_handle.case().await.as_str() {
                        "true" => {
                            bool_handle.continue_();
                            true
                        }
                        "false" => {
                            bool_handle.continue_();
                            false
                        }
                        variant => {
                            panic!(
                                "Test.assert expected Bool, got unexpected variant: {}",
                                variant
                            );
                        }
                    };

                    // Send the assertion result
                    let result = AssertionResult {
                        description,
                        passed,
                    };

                    let _ = sender.send(result);

                    // Continue to next assertion - provide a new box for the next iteration
                    provide_test_inner(handle, sender);
                }
                "done" => {
                    handle.break_();
                }
                "id" => {
                    let argument = handle.send();
                    argument.provide_box(|mut handle| async move {
                        let arg = handle.receive();
                        handle.link(arg);
                    });

                    provide_test_inner(handle, sender);
                }
                "leak" => {
                    let argument = handle.send();
                    argument.provide_box(|handle| async move {
                        drop(handle);
                    });

                    provide_test_inner(handle, sender);
                }
                other => {
                    panic!("Unexpected method call on Test: {}", other);
                }
            }
        }
    })
}
