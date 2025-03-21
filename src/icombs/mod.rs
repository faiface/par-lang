//! # Interaction combinator runtime
//!
//! Par ships with a runtime based on Interaction Combinators. Interaction Combinators are a model of computation
//! based on graph-rewriting. They are a specific case of interaction nets.
//!
//! In an interaction net, each node has a principal port and an amount of auxiliary ports. Nodes interact by their principal port,
//! according to some pre-defined set of interaction rules
//!
//! The [`compiler`] module compiles Par definitions inta interaction combinator trees.
//! The definition must be in process syntax (desugared), and type-annotated.
//!
//! The [`readback`] module exposes an async API to progressively read back normal-form interaction combinator nets
//! back into Par expressions.

pub mod compiler;
mod equivalence;
pub mod net;
pub mod parse;
pub use compiler::{compile_file, IcCompiled};
pub use equivalence::are_equivalent;
pub use net::{Net, Tree, VarId};

use crate::par::language::Internal;
pub mod readback;

/// Names used internally by the interaction combinator compiler.
pub type Name = Internal<crate::par::parse::Name>;
