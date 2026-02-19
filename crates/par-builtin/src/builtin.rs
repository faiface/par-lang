mod bench;
mod boxmap;
mod byte;
mod bytes;
mod char_;
mod console;
mod debug;
mod http;
mod int;
mod list;
mod map;
mod nat;
mod os;
mod parser;
mod string;
mod time;
mod url;

use std::sync::Arc;

use par_core::frontend::{Module, process};
use par_core::source::FileName;
use par_core::testing::import_test_module;

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        None,
        Module::parse_and_compile(include_str!("./builtin/Builtin.par"), FileName::BUILTIN)
            .unwrap(),
    );
    module.import(Some("Nat"), nat::external_module());
    module.import(Some("Int"), int::external_module());
    module.import(Some("Bench"), bench::external_module());
    module.import(Some("Char"), char_::external_module());
    module.import(Some("String"), string::external_module());
    module.import(Some("Byte"), byte::external_module());
    module.import(Some("Bytes"), bytes::external_module());
    module.import(Some("Debug"), debug::external_module());
    module.import(Some("Console"), console::external_module());
    module.import(Some("Os"), os::external_module());
    module.import(Some("Map"), map::external_module());
    module.import(Some("BoxMap"), boxmap::external_module());
    module.import(Some("Url"), url::external_module());
    module.import(Some("Http"), http::external_module());
    module.import(Some("Time"), time::external_module());
    import_test_module(module);
}
