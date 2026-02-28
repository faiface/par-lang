pub(crate) mod flat;
pub(crate) mod readback;
pub(crate) mod tree;

pub use crate::backend::compiler::Compiled;
pub use crate::backend::tree::compiler::Error as RuntimeCompilerError;
pub use readback::{Handle, TypedHandle, TypedReadback};
pub use stats::Rewrites;
pub(crate) mod executor;

mod stats {
    pub use super::flat::stats::Rewrites;
}
