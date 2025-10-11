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
