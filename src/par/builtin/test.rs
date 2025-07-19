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

async fn test_assert(_handle: Handle) {
    todo!("test_assert not implemented yet");
}
