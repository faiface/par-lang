//! This is a runtime based on *interaction nets*
//! [Interaction nets][https://en.wikipedia.org/wiki/Interaction_nets] (inets) are a model of computation where
//! programs are represented by graphs (a *net*). Each node corresponds to an operation or a value
//! Nodes are connected to other nodes with edges through ports, and they
//! have a *principal port* which determines what value it interacts with.
//!
//! Par is particularly well-suited for interaction nets because it makes no distinction between commands and values.
//! In the runtime, values and commands are represented by the same kind of thing.
//!
//! Other programming languages that have inet runtimes are [Vine][https://vine.dev/] and [HVM][https://github.com/HigherOrderCO/HVM3/]
//!
//! The runtime that preceded this one (let's call it the V2 runtime, if this one is V3) was also based on interaction nets.
//! It used an approach where the many different types of commands and values in Par were compiled to a
//! small set of nodes. This is the approach also used by other inet languages. This is very
//! simple to implement and work with but it suffers from poor cache locality. Additionally,
//! serialization and debugging is very hard.
//!
//! However, it turns out that in Par we can do much better. Linear logic heavily restricts
//! where and how duplication can take place. The older approach was "too general"; we can use Par's
//! restrictions to implement a new runtime.
//!
//! The new runtime stores the static program in an arena defined in `arena` module. The arena's
//! contents don't change while it's executed. The program state, which changes while it's running
//! is stored in different locations depending on how it's used. Some state is reference-counted while
//! state that is guaranteed to be used linearly is stored in a shared buffer that is allocated when
//! each definition is instantiated. This is described in [`runtime`]
//!
//! Currently, the new runtime is transpiled from the old runtime since the representations are very similar,
//! to save some effort into refactoring the compiler.
//! A few changes had to be made to the old runtime and compiler to allow for this. In the long term, this should
//! not be necessary and be replaced with a more direct approach.
//!
//! Hopefully, this runtime will work well enough for a long time.

pub mod arena;
pub mod runtime;
pub mod transpiler;
pub mod readback;
pub mod reducer;
