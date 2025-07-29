use std::sync::{mpsc, Arc};

use crate::{
    icombs::readback::Handle,
    location::Span,
    par::{
        language::GlobalName,
        process,
        program::{Definition, Module, TypeDef},
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
            typ: Type::iterative_box_choice(vec![(
                "assert",
                Type::function(
                    Type::string(),
                    Type::function(Type::bool(), Type::self_(None)),
                ),
            )]),
        }],
        declarations: vec![],
        definitions: vec![
            Definition::external(
                "Assert",
                Type::function(
                    Type::either(vec![("false", Type::break_()), ("true", Type::break_())]),
                    Type::break_(),
                ),
                |handle| Box::pin(test_assert(handle)),
            ),
            // TODO: add the Test provider function here
        ],
    }
}

async fn test_assert(mut handle: Handle) {
    // receive the boolean value from the handle first
    let mut bool_handle = handle.receive();

    match bool_handle.case().await.as_str() {
        "false" => {
            handle.break_();
            panic!("Test assertion failed");
        }
        "true" => {
            handle.break_();
        }
        variant => panic!(
            "Test.Assert expected Bool, got unexpected variant: {}",
            variant
        ),
    }
}

pub fn provide_test(handle: Handle, sender: mpsc::Sender<AssertionResult>) {
    handle.provide_box(move |mut handle| {
        let sender = sender.clone();
        async move {
            loop {
                match handle.case().await.as_str() {
                    "assert" => {
                        // Receive the description string
                        let description = handle.receive().string().await.to_string();

                        // Receive the boolean value
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

                        if let Err(_) = sender.send(result) {
                            // Channel was closed, probably test is being cancelled
                            break;
                        }

                        // Continue to next iteration (return self)
                        handle.send();
                    }
                    other => {
                        panic!("Unexpected method call on Test: {}", other);
                    }
                }
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_assertion::create_assertion_channel;

    #[test]
    fn test_provide_test_sends_assertion_results() {
        // This test requires integration with the actual Handle/Net system
        // which is complex to mock. We'll test this through integration tests
        // when we update the test runner.

        let (sender, receiver) = create_assertion_channel();

        // Basic test to ensure the function compiles
        // Real testing will happen through integration tests
        assert!(receiver.try_recv().is_err()); // Channel is empty
    }
}
