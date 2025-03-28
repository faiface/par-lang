#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default, Hash)]
pub struct Point {
    // 0-based
    pub offset: usize,
    // 0-based
    pub row: usize,
    // 0-based
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default, Hash)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

pub trait Spanning {
    fn span(&self) -> Span;
}

impl Span {
    pub fn len(&self) -> usize {
        self.end.offset - self.start.offset
    }

    pub fn join(&self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

impl Point {
    pub fn point_span(&self) -> Span {
        Span {
            start: *self,
            end: *self,
        }
    }
}