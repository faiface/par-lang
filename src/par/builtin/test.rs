use std::sync::{mpsc, Arc};

use crate::{
    icombs::readback::Handle,
    location::FileSpan,
    par::{
        language::GlobalName,
        process,
        program::{Module, TypeDef},
        types::Type,
    },
    test_assertion::AssertionResult,
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef {
            span: FileSpan::NONE,
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
                                Type::name(Some("Bool"), "Bool", vec![]),
                                Type::self_(None),
                            ),
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

pub fn provide_test(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    provide_test_inner(handle, sender);
}

fn provide_test_inner(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    handle.provide_box(move |mut handle| {
        let sender = sender.clone();
        async move {
            match handle.case().await.as_str() {
                "assert" => {
                    let description = handle.receive().string().await.to_string();
                    let mut bool_handle = handle.receive();

                    let passed = match bool_handle.case().await.as_str() {
                        "true" => {
                            bool_handle.break_();
                            true
                        }
                        "false" => {
                            bool_handle.break_();
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
                other => {
                    panic!("Unexpected method call on Test: {}", other);
                }
            }
        }
    });
}
