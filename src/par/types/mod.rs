// Import core types from separate module
pub mod core;
pub use core::{PrimitiveType, Type};

// Import error types from separate module
pub mod error;
pub use error::TypeError;

// Import display functionality from separate module
pub mod display;

// Import validation functionality from separate module
pub mod validation;

// Import substitution functionality from separate module
pub mod substitution;

// Import assignability functionality from separate module
pub mod assignability;

// Import expansion functionality from separate module
pub mod expansion;

// Import duality functionality from separate module
pub mod duality;

// Import dependencies functionality from separate module
pub mod dependencies;

// Import definitions functionality from separate module
pub mod definitions;
pub use definitions::TypeDefs;

// Import context functionality from separate module
pub mod context;
pub use context::Context;

// Import checking functionality from separate module
pub mod checking;
