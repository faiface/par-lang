use std::sync::Arc;

use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Assert",
            Type::function(
                Type::either(vec![("false", Type::break_()), ("true", Type::break_())]),
                Type::break_(),
            ),
            |handle| Box::pin(test_assert(handle)),
        )],
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
