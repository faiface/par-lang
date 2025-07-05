#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Point {
    // 0-based
    pub offset: usize,
    // 0-based
    pub row: usize,
    // 0-based
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Span {
    None,
    At { start: Point, end: Point },
}

impl Default for Span {
    fn default() -> Self {
        Self::None
    }
}

pub trait Spanning {
    fn span(&self) -> Span;
}

impl Span {
    pub fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::At { start, end } => end.offset - start.offset,
        }
    }

    pub fn points(&self) -> Option<(Point, Point)> {
        match self {
            Self::None => None,
            Self::At { start, end } => Some((*start, *end)),
        }
    }

    pub fn start(&self) -> Option<Point> {
        self.points().map(|(s, _)| s)
    }

    pub fn end(&self) -> Option<Point> {
        self.points().map(|(_, e)| e)
    }

    pub fn only_start(&self) -> Self {
        match self {
            Self::None => Self::None,
            Self::At { start, .. } => Self::At {
                start: *start,
                end: *start,
            },
        }
    }

    pub fn only_end(&self) -> Self {
        match self {
            Self::None => Self::None,
            Self::At { end, .. } => Self::At {
                start: *end,
                end: *end,
            },
        }
    }

    pub fn join(&self, other: Self) -> Self {
        match (self, other) {
            (Self::None, span) | (&span, Self::None) => span,
            (
                &Self::At {
                    start: start1,
                    end: end1,
                },
                Self::At {
                    start: start2,
                    end: end2,
                },
            ) => Self::At {
                start: if start1.offset < start2.offset {
                    start1
                } else {
                    start2
                },
                end: if end1.offset > end2.offset {
                    end1
                } else {
                    end2
                },
            },
        }
    }
}

impl Point {
    pub fn point_span(&self) -> Span {
        Span::At {
            start: *self,
            end: *self,
        }
    }
}
