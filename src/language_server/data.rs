use crate::location::Point;
use lsp_types::{self as lsp};

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

/* types:

NAMESPACE: namespaces
TYPE: types
CLASS: choice types?
ENUM: either types?
INTERFACE: traits
STRUCT: records?
TYPE_PARAMETER: receive type name
PARAMETER: receive value name
VARIABLE: (let)-declared, non-receive value
PROPERTY: choice branch (not receive)?
ENUM_MEMBER: either variant
FUNCTION: receive type
METHOD: choice branch (receive)?
KEYWORD (not here)
MODIFIER (not here)
COMMENT (not here)
STRING (not here)
NUMBER (not here)
OPERATOR (not here)
 */

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

/* modifiers:

DECLARATION: "dec"
DEFINITION: "def"
READONLY: not reassigned
DEPRECATED: (future)
ABSTRACT: trait member without default
MODIFICATION: reassigned
DOCUMENTATION: (future)
DEFAULT_LIBRARY: ?
 */

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

semantic_token_types!(TYPE, FUNCTION, VARIABLE);
semantic_token_modifiers!(DECLARATION, DEFINITION, READONLY, MODIFICATION);
