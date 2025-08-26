use arcstr::ArcStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Point {
    // 0-based
    pub offset: u32,
    // 0-based
    pub row: u32,
    // 0-based
    pub column: u32,
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
    pub fn len(&self) -> u32 {
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

    pub fn with_file(self, file: FileName) -> FileSpan {
        FileSpan::new(self, file)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FileName {
    Builtin,
    Path(ArcStr),
}

impl From<&str> for FileName {
    fn from(path: &str) -> Self {
        FileName::Path(path.into())
    }
}

impl From<String> for FileName {
    fn from(path: String) -> Self {
        FileName::Path(path.into())
    }
}

impl From<&FileName> for FileName {
    fn from(file: &FileName) -> Self {
        file.clone()
    }
}

#[derive(Debug, Clone, Default)]
pub struct FileSpan {
    inner: Option<((Point, Point), FileName)>,
}

impl FileSpan {
    pub const NONE: Self = Self { inner: None };

    pub fn new(span: Span, file: FileName) -> Self {
        match span {
            Span::None => Self { inner: None },
            Span::At { start, end } => Self {
                inner: Some(((start, end), file)),
            },
        }
    }

    pub fn span(&self) -> Span {
        match self.inner {
            Some(((start, end), _)) => Span::At { start, end },
            None => Span::None,
        }
    }

    pub fn file(&self) -> Option<&FileName> {
        self.inner.as_ref().map(|(_, file)| file)
    }

    pub fn file_path(&self) -> Option<&ArcStr> {
        match &self.inner {
            Some((_, FileName::Path(path))) => Some(path),
            _ => None,
        }
    }

    pub fn with_span(&self, span: Span) -> Self {
        match &self.inner {
            Some((_, file)) => FileSpan::new(span, file.clone()),
            None => FileSpan::NONE,
        }
    }

    pub fn points(&self) -> Option<(Point, Point)> {
        self.inner.as_ref().map(|&(points, _)| points)
    }
}
