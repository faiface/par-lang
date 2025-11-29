pub mod new;
pub mod old;
pub use new::transpiler::Transpiled as Compiled;
pub use old::compiler::Error as RuntimeCompilerError;
pub use old::net::Net;
pub use old::readback::{Handle, TypedHandle, TypedReadback};

use crate::par::program::CheckedModule;
