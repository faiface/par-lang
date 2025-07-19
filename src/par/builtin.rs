pub mod char_;
pub mod console;
pub mod debug;
pub mod int;
pub mod list;
pub mod nat;
pub mod storage;
pub mod string;
pub mod test;

use std::sync::Arc;

use super::{process, program::Module};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import_unqualified(
        Module::parse_and_compile(include_str!("./builtin/Builtin.par")).unwrap(),
    );

    module.import("Nat", nat::external_module());
    module.import("Int", int::external_module());
    module.import("Char", char_::external_module());
    module.import("String", string::external_module());
    module.import("Debug", debug::external_module());
    module.import("Console", console::external_module());
    module.import("Storage", storage::external_module());
    module.import("Test", test::external_module());
}
