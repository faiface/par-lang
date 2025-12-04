pub mod new;
pub mod old;
pub use new::transpiler::Transpiled as Compiled;
pub use old::compiler::Error as RuntimeCompilerError;
pub use old::net::Net;
pub use old::readback::{TypedHandle, TypedReadback};
pub use new::readback::Handle;

use crate::par::program::CheckedModule;
