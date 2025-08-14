use std::sync::{mpsc, Arc};

use crate::{
    icombs::readback::Handle,
    location::Span,
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
            span: Span::None,
            name: GlobalName::external(Some("Test"), "Test"),
            params: vec![],
            typ: Type::iterative(
                None,
                Type::box_(Type::choice(vec![(
                    "assert",
                    Type::function(
                        Type::string(),
                        Type::function(Type::name(Some("Bool"), "Bool", vec![]), Type::break_()),
                    ),
                )])),
            ),
        }],
        declarations: vec![],
        definitions: vec![],
    }
}

pub fn provide_test(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    handle.provide_box(move |mut handle| {
        let sender = sender.clone();
        async move {
            // for iterative box choice, we need to handle each method call
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
                }
                other => {
                    panic!("Unexpected method call on Test: {}", other);
                }
            }
        }
    });
}
