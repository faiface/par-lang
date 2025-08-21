pub mod core;
pub use core::{LoopId, Operation, PrimitiveType, Type};

pub mod error;
pub use error::TypeError;

pub mod definitions;
pub use definitions::TypeDefs;
pub mod assignability;
pub mod checking;
pub mod context;
pub mod dependencies;
pub mod duality;
pub mod substitution;
pub mod validation;
pub use context::Context;
pub mod display;
pub mod expansion;
pub mod tests;
pub mod visit;
