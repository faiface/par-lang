use futures::{future::BoxFuture, FutureExt};

use crate::{par::primitive::ParString, runtime::new::runtime::ExternalFnRet};
use std::sync::{mpsc, Arc};

use crate::{
    location::Span,
    par::{
        language::GlobalName,
        process,
        program::{Module, TypeDef},
        types::Type,
    },
    runtime::Handle,
    test_assertion::AssertionResult,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
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
    provide_test_inner(handle, sender).await;
}

fn provide_test_inner(
    handle: Handle,
    sender: mpsc::Sender<AssertionResult>,
) -> BoxFuture<'static, ()> {
    async move {
        handle
            .provide_box(move |mut handle| {
                let sender = sender.clone();
                async move {
                    match handle.case().await.as_str() {
                        "assert" => {
                            let description =
                                handle.receive().await.string().await.as_str().to_string();
                            println!("{}", description);
                            let mut bool_handle = handle.receive().await;

                            let passed = match bool_handle.case().await.as_str() {
                                "true" => {
                                    bool_handle.continue_().await;
                                    true
                                }
                                "false" => {
                                    bool_handle.continue_().await;
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
                            provide_test_inner(handle, sender).await;
                        }
                        "done" => {
                            handle.break_().await;
                        }
                        "id" => {
                            fn identity(mut handle: Handle) -> ExternalFnRet {
                                Box::pin(async {
                                    let arg = handle.receive().await;
                                    handle.link(arg).await;
                                })
                            }
                            let argument = handle.send().await;
                            argument.provide_box(|x| identity(x)).await;

                            provide_test_inner(handle, sender).await;
                        }
                        "leak" => {
                            fn leak(mut handle: Handle) -> ExternalFnRet {
                                Box::pin(async {
                                    drop(handle);
                                })
                            }
                            let argument = handle.send().await;
                            argument.provide_box(|x| leak(x)).await;

                            provide_test_inner(handle, sender).await;
                        }
                        other => {
                            panic!("Unexpected method call on Test: {}", other);
                        }
                    }
                }
            })
            .await;
    }
    .boxed()
}
