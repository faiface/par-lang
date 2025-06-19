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
            "Open",
            Type::name(None, "Console", vec![]),
            |handle| Box::pin(console_open(handle)),
        )],
    }
}

async fn console_open(mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                break;
            }
            "print" => {
                println!("{}", handle.receive().string().await);
            }
            _ => unreachable!(),
        }
    }
}
