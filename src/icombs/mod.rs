pub mod compiler;
mod equivalence;
pub mod net;
pub mod parse;
pub use compiler::{compile_file, IcCompiled};
pub use equivalence::are_equivalent;
pub use net::{Net, Tree, VarId};

use crate::par::language::Internal;
pub mod readback;

pub type Name = Internal<crate::par::parse::Name>;
