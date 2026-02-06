use par_core::location::Point;
use lsp_types::{self as lsp};

pub(crate) trait ToLspPosition {
    fn to_lsp_position(self) -> lsp::Position;
}

impl ToLspPosition for Point {
    fn to_lsp_position(self) -> lsp::Position {
        lsp::Position {
            line: self.row as u32,
            character: self.column as u32,
        }
    }
}
