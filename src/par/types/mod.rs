pub mod core;
pub use core::{Operation, PrimitiveType, Type};

pub mod error;
pub use error::TypeError;

pub mod definitions;
pub use definitions::TypeDefs;
pub mod assignability;
pub mod dependencies;
pub mod duality;
pub mod validation;
pub mod checking;
pub mod context;
pub mod substitution;
pub use context::Context;
pub mod display;
pub mod expansion;
