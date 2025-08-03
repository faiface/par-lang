use byteview::ByteView;
use std::sync::Arc;

use crate::{
    icombs::readback::Handle,
    par::{
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef::external("Bytes", &[], Type::bytes())],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Builder",
            Type::name(None, "Builder", vec![]),
            |handle| Box::pin(bytes_builder(handle)),
        )],
    }
}

async fn bytes_builder(mut handle: Handle) {
    let mut buf = Vec::<u8>::new();
    loop {
        match handle.case().await.as_str() {
            "add" => {
                buf.extend(handle.receive().bytes().await.as_ref());
            }
            "build" => {
                handle.provide_bytes(ByteView::from(buf));
                break;
            }
            _ => unreachable!(),
        }
    }
}
