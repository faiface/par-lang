use std::sync::Arc;

use crate::{
    icombs::readback::Handle,
    location::Span,
    par::{
        language::GlobalName,
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![
            TypeDef {
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
            },
        ],
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
