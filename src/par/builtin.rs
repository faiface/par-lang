pub mod char_;
pub mod console;
pub mod debug;
pub mod int;
pub mod list;
pub mod nat;
pub mod storage;
pub mod string;

use std::sync::Arc;

use super::{process, program::Module};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        "Bool",
        Module::parse_and_compile(include_str!("./builtin/Bool.par")).unwrap(),
    );
    module.import(
        "List",
        Module::parse_and_compile(include_str!("./builtin/List.par")).unwrap(),
    );
    module.import(
        "Ordering",
        Module::parse_and_compile(include_str!("./builtin/Ordering.par")).unwrap(),
    );
    module.import(
        "Char",
        Module::parse_and_compile(include_str!("./builtin/Char.par")).unwrap(),
    );
    module.import(
        "String",
        Module::parse_and_compile(include_str!("./builtin/String.par")).unwrap(),
    );
    module.import(
        "Console",
        Module::parse_and_compile(include_str!("./builtin/Console.par")).unwrap(),
    );
    module.import(
        "Storage",
        Module::parse_and_compile(include_str!("./builtin/Storage.par")).unwrap(),
    );

    module.import("Nat", nat::external_module());
    module.import("Int", int::external_module());
    module.import("Char", char_::external_module());
    module.import("String", string::external_module());
    module.import("Debug", debug::external_module());
    module.import("Console", console::external_module());
    module.import("Storage", storage::external_module());
}
