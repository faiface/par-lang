pub(crate) mod flat;
pub(crate) mod tree;
pub(crate) mod compiler;
pub(crate) mod readback;

pub use compiler::Compiled;
pub use readback::{Handle, TypedHandle, TypedReadback};
pub use stats::Rewrites;
pub use tree::compiler::Error as RuntimeCompilerError;

mod stats {
    pub use super::flat::stats::Rewrites;
}
