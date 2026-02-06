pub mod bench;
pub mod boxmap;
pub mod byte;
pub mod bytes;
pub mod char_;
pub mod console;
pub mod debug;
pub mod http;
pub mod int;
pub mod list;
pub mod map;
pub mod nat;
pub mod os;
pub mod parser;
pub mod string;
pub mod test;
pub mod time;
pub mod url;

use std::sync::Arc;

use par_core::frontend::{Expression, Module};
use par_core::source::FileName;

pub fn import_builtins(module: &mut Module<Arc<Expression<()>>>) {
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
    module.import(Some("Test"), test::external_module());
}
