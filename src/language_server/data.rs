use lsp_types::{self as lsp};
use crate::location::{Point, Span};

impl Into<lsp::Range> for Span {
    fn into(self) -> lsp::Range {
        lsp::Range {
            start: self.start.into(),
            end: self.end.into(),
        }
    }
}

impl Into<lsp::Range> for &Span {
    fn into(self) -> lsp::Range {
        self.clone().into()
    }
}

impl Into<lsp::Position> for Point {
    fn into(self) -> lsp::Position {
        lsp::Position {
            line: self.row as u32,
            character: self.column as u32,
        }
    }
}

// todo: are these macros the best way to go?

macro_rules! running_constants {
    (linear, $($x:ident),*) => {
        running_constants!(linear, 0 => $($x),*);
    };
    (bitset, $($x:ident),*) => {
        running_constants!(bitset, 0 => $($x),*);
    };
    (linear, $index:expr => $first:ident, $($x:ident),*) => {
        pub const $first: u32 = $index;
        running_constants!(linear, $index + 1 => $($x),*);
    };
    (bitset, $index:expr => $first:ident, $($x:ident),*) => {
        pub const $first: u32 = 1 << $index;
        running_constants!(bitset, $index + 1 => $($x),*);
    };
    (linear, $index:expr => $last:ident) => {
        pub const $last: u32 = $index;
        #[allow(non_upper_case_globals)]
        pub const count: usize = $index + 1;
    };
    (bitset, $index:expr => $last:ident) => {
        pub const $last: u32 = 1 << $index;
        #[allow(non_upper_case_globals)]
        pub const count: usize = $index + 1;
    };
}

macro_rules! semantic_token_types {
    ($($x:ident),*) => {
        pub const SEMANTIC_TOKEN_TYPES: [
            lsp::SemanticTokenType;
            semantic_token_types::count
        ] = [
            $(lsp::SemanticTokenType::$x,)*
        ];

        pub mod semantic_token_types {
            running_constants!(linear, $($x),*);
        }
    };
}

macro_rules! semantic_token_modifiers {
    ($($x:ident),*) => {
        pub const SEMANTIC_TOKEN_MODIFIERS: [
            lsp::SemanticTokenModifier;
            semantic_token_modifiers::count
        ] = [
            $(lsp::SemanticTokenModifier::$x,)*
        ];

        pub mod semantic_token_modifiers {
            running_constants!(bitset, $($x),*);
        }
    };
}

semantic_token_types!(TYPE, FUNCTION);
semantic_token_modifiers!(DECLARATION, DEFINITION);