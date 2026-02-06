//! # Interaction combinator runtime
//!
//! Par ships with a runtime based on Interaction Combinators. Interaction Combinators are a model of computation
//! based on graph-rewriting. They are a specific case of interaction nets.
//!
//! In an interaction net, each node has a principal port and an amount of auxiliary ports. Nodes interact by their principal port,
//! according to some pre-defined set of interaction rules
//!
//! The [`net`] module contains definitions common to all modules, and the implementation for the current runtime
//!
//! The [`compiler`] module compiles Par definitions into interaction combinator trees.
//! The definition must be in process syntax (desugared), and type-annotated.
//!
//! The [`readback`] module exposes an async API to progressively read back normal-form interaction combinator nets
//! back into Par expressions.

pub mod compiler;
pub mod net;
pub use net::Net;
