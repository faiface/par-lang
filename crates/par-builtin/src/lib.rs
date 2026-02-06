pub mod builtin;

pub use builtin::import_builtins;

// Convenience re-exports so the builtin implementations can keep using the same
// paths they historically had when they lived inside `par-core`.
pub mod location {
    pub use par_core::location::*;
}

pub mod par {
    pub use par_core::par::{
        language, primitive, process, program, types,
    };

    pub mod builtin {
        pub use crate::builtin::*;
    }
}

pub mod runtime {
    pub use par_core::runtime::*;
}

pub mod test_assertion {
    pub use par_core::{AssertionResult, create_assertion_channel};
}
