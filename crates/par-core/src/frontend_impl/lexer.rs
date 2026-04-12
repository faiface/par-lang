use crate::location::{FileName, Point, Span};
use core::str::FromStr;
use winnow::{
    Parser, Result,
    combinator::{alt, not, opt, peek, preceded, repeat},
    error::{EmptyError, ParserError},
    stream::{ParseSlice, TokenSlice},
    token::{any, literal, take_while},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBrack,
    RBrack,
    Lt,
    Gt,

    Slash,
    At,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Eq,
    FatArrow,
    ThinArrow,
    Bang,
    Quest,
    Star,
    Link,

    Float,
    Integer,
    String,

    InvalidString,
    InvalidChar,

    LowercaseIdentifier,
    UppercaseIdentifier,
    Begin,
    Box,
    Case,
    Catch,
    Chan,
    Choice,
    Dec,
    Def,
    Do,
    Dual,
    Either,
    Else,
    Export,
    If,
    Import,
    Is,
    In,
    Iterative,
    Let,
    And,
    As,
    Module,
    Or,
    Not,
    Loop,
    Poll,
    Repoll,
    Submit,
    Recursive,
    Self_,
    Throw,
    Try,
    Default,
    Type,
    Unfounded,
    External,

    Unknown,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub raw: &'i str,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum CommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Comment<'i> {
    pub kind: CommentKind,
    pub raw: &'i str,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Lexed<'i> {
    pub tokens: Vec<Token<'i>>,
    pub comments: Vec<Comment<'i>>,
}

impl Token<'_> {
    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

// More useful in winnow debug view
// impl core::fmt::Debug for Token<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         format!("{}", self.raw).fmt(f)
//         // f.debug_struct("Token").field("kind", &self.kind).field("raw", &self.raw).field("loc", &self.loc).field("span", &self.span).finish()
//     }
// }
impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

impl TokenKind {
    pub fn expected(&self) -> &'static str {
        match self {
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "{",
            TokenKind::RCurly => "}",
            TokenKind::LBrack => "[",
            TokenKind::RBrack => "]",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",

            TokenKind::Slash => "/",
            TokenKind::At => "@",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Eq => "=",
            TokenKind::FatArrow => "=>",
            TokenKind::ThinArrow => "->",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Star => "*",
            TokenKind::Link => "<>",

            TokenKind::Float => "float",
            TokenKind::Integer => "integer",
            TokenKind::String => "string",

            TokenKind::InvalidString => "invalid string",
            TokenKind::InvalidChar => "invalid char",

            TokenKind::LowercaseIdentifier => "lower-case identifier",
            TokenKind::UppercaseIdentifier => "upper-case identifier",
            TokenKind::Begin => "begin",
            TokenKind::Box => "box",
            TokenKind::Case => "case",
            TokenKind::Catch => "catch",
            TokenKind::Chan => "chan",
            TokenKind::Choice => "choice",
            TokenKind::Dec => "dec",
            TokenKind::Def => "def",
            TokenKind::Do => "do",
            TokenKind::Dual => "dual",
            TokenKind::Either => "either",
            TokenKind::Else => "else",
            TokenKind::Export => "export",
            TokenKind::If => "if",
            TokenKind::Import => "import",
            TokenKind::Is => "is",
            TokenKind::In => "in",
            TokenKind::Iterative => "iterative",
            TokenKind::Let => "let",
            TokenKind::And => "and",
            TokenKind::As => "as",
            TokenKind::Module => "module",
            TokenKind::Or => "or",
            TokenKind::Not => "not",
            TokenKind::Loop => "loop",
            TokenKind::Poll => "poll",
            TokenKind::Repoll => "repoll",
            TokenKind::Submit => "submit",
            TokenKind::Recursive => "recursive",
            TokenKind::Self_ => "self",
            TokenKind::Throw => "throw",
            TokenKind::Try => "try",
            TokenKind::Default => "default",
            TokenKind::Type => "type",
            TokenKind::Unfounded => "unfounded",
            TokenKind::External => "external",

            TokenKind::Unknown => "???",
        }
    }
}

impl PartialEq<str> for Token<'_> {
    fn eq(&self, other: &str) -> bool {
        self.raw == other
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, &other: &&str) -> bool {
        self.raw == other
    }
}

impl<'i, E> Parser<Tokens<'i>, &'i Token<'i>, E> for TokenKind
where
    E: ParserError<Tokens<'i>>,
{
    fn parse_next(&mut self, input: &mut Tokens<'i>) -> Result<&'i Token<'i>, E> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

impl<'a, T: FromStr> ParseSlice<T> for Token<'a> {
    fn parse_slice(&self) -> Option<T> {
        self.raw.parse().ok()
    }
}
impl<'a, T: FromStr> ParseSlice<T> for &Token<'a> {
    fn parse_slice(&self) -> Option<T> {
        self.raw.parse().ok()
    }
}

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;
pub(crate) type Input<'a> = Tokens<'a>;

pub(crate) fn lex<'s>(input: &'s str, file: &FileName) -> Vec<Token<'s>> {
    lex_with_comments(input, file).tokens
}

fn scan_digit_run(input: &str, start: usize) -> Option<usize> {
    let bytes = input.as_bytes();
    if !matches!(bytes.get(start), Some(b'0'..=b'9')) {
        return None;
    }

    let mut idx = start + 1;
    while let Some(&byte) = bytes.get(idx) {
        match byte {
            b'0'..=b'9' => idx += 1,
            b'_' if matches!(bytes.get(idx + 1), Some(b'0'..=b'9')) => idx += 1,
            _ => break,
        }
    }

    Some(idx)
}

fn scan_number_token(input: &str) -> Option<(&str, TokenKind, usize)> {
    let bytes = input.as_bytes();
    let mut idx = 0;

    if matches!(bytes.get(idx), Some(b'+' | b'-')) {
        idx += 1;
    }

    idx = scan_digit_run(input, idx)?;

    if matches!(bytes.get(idx), Some(b'.')) {
        let frac_start = idx + 1;
        if let Some(frac_end) = scan_digit_run(input, frac_start) {
            idx = frac_end;
            if matches!(bytes.get(idx), Some(b'e' | b'E')) {
                let exp_start = idx;
                idx += 1;
                if matches!(bytes.get(idx), Some(b'+' | b'-')) {
                    idx += 1;
                }
                if let Some(exp_end) = scan_digit_run(input, idx) {
                    idx = exp_end;
                } else {
                    idx = exp_start;
                }
            }
            let raw = &input[..idx];
            return Some((raw, TokenKind::Float, idx));
        }
    }

    let raw = &input[..idx];
    Some((raw, TokenKind::Integer, idx))
}

pub(crate) fn lex_with_comments<'s>(input: &'s str, file: &FileName) -> Lexed<'s> {
    type Error = EmptyError;
    (|input: &'s str| -> Result<Lexed<'s>, Error> {
        let mut input = input;
        let input = &mut input;
        let mut row = 0;
        let mut last_newline = input.len();
        let mut tokens = Vec::new();
        let mut comments = Vec::new();
        let mut idx = 0;
        while let Ok(c) = peek(any::<&str, Error>).parse_next(input) {
            let column = last_newline - input.len(); // starting column
            let Some((raw, kind, len)) = (match c {
                '-' => {
                    let source = *input;
                    if let Some(rest) = source.strip_prefix("->") {
                        *input = rest;
                        Some(("->", TokenKind::ThinArrow, 2))
                    } else if let Some((raw, kind, len)) = scan_number_token(source) {
                        *input = &source[len..];
                        Some((raw, kind, len))
                    } else {
                        let raw = any::<&str, Error>.take().parse_next(input)?;
                        Some((raw, TokenKind::Unknown, raw.len()))
                    }
                }
                '0'..='9' | '+' => {
                    let source = *input;
                    if let Some((raw, kind, len)) = scan_number_token(source) {
                        *input = &source[len..];
                        Some((raw, kind, len))
                    } else {
                        let raw = any::<&str, Error>.take().parse_next(input)?;
                        Some((raw, TokenKind::Unknown, raw.len()))
                    }
                }
                '"' => {
                    any.parse_next(input)?;
                    let raw = (
                        repeat(0.., alt((preceded('\\', any), any.verify(|c| *c != '"'))))
                            .map(|()| ()),
                    )
                        .take()
                        .parse_next(input)?;
                    let is_closed = opt('"').parse_next(input)?.is_some();
                    let is_valid = unescaper::unescape(raw).is_ok();
                    let len = raw.len() + 1 + usize::from(is_closed);
                    Some((
                        raw,
                        if is_closed && is_valid {
                            TokenKind::String
                        } else {
                            TokenKind::InvalidString
                        },
                        len,
                    ))
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    /* a /* b */ */
                    let raw = take_while(
                        0..,
                        |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
                    )
                    .take()
                    .parse_next(input)?;
                    let kind = match raw {
                        "begin" => TokenKind::Begin,
                        "box" => TokenKind::Box,
                        "case" => TokenKind::Case,
                        "catch" => TokenKind::Catch,
                        "chan" => TokenKind::Chan,
                        "choice" => TokenKind::Choice,
                        "dec" => TokenKind::Dec,
                        "def" => TokenKind::Def,
                        "do" => TokenKind::Do,
                        "dual" => TokenKind::Dual,
                        "either" => TokenKind::Either,
                        "else" => TokenKind::Else,
                        "export" => TokenKind::Export,
                        "if" => TokenKind::If,
                        "import" => TokenKind::Import,
                        "is" => TokenKind::Is,
                        "in" => TokenKind::In,
                        "iterative" => TokenKind::Iterative,
                        "let" => TokenKind::Let,
                        "and" => TokenKind::And,
                        "as" => TokenKind::As,
                        "module" => TokenKind::Module,
                        "or" => TokenKind::Or,
                        "not" => TokenKind::Not,
                        "loop" => TokenKind::Loop,
                        "poll" => TokenKind::Poll,
                        "repoll" => TokenKind::Repoll,
                        "submit" => TokenKind::Submit,
                        "recursive" => TokenKind::Recursive,
                        "self" => TokenKind::Self_,
                        "throw" => TokenKind::Throw,
                        "try" => TokenKind::Try,
                        "default" => TokenKind::Default,
                        "type" => TokenKind::Type,
                        "unfounded" => TokenKind::Unfounded,
                        "external" => TokenKind::External,
                        raw => {
                            if raw.starts_with(char::is_uppercase) {
                                TokenKind::UppercaseIdentifier
                            } else {
                                TokenKind::LowercaseIdentifier
                            }
                        }
                    };
                    Some((raw, kind, raw.len()))
                }
                '\n' => {
                    let _ = any::<&str, Error>.parse_next(input);
                    row += 1;
                    last_newline = input.len();
                    idx += 1;
                    None
                }
                ' ' | '\t' | '\r' => {
                    let _ = any::<&str, Error>.parse_next(input);
                    idx += 1;
                    None
                }
                ':' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Colon, raw.len()))
                }
                ';' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Semicolon, raw.len()))
                }
                '[' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LBrack, raw.len()))
                }
                ']' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RBrack, raw.len()))
                }
                '(' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LParen, raw.len()))
                }
                ')' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RParen, raw.len()))
                }
                '{' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LCurly, raw.len()))
                }
                '}' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RCurly, raw.len()))
                }
                '<' => {
                    let (raw, kind) = alt((
                        "<>".map(|raw| (raw, TokenKind::Link)),
                        "<".map(|raw| (raw, TokenKind::Lt)),
                    ))
                    .parse_next(input)?;
                    Some((raw, kind, raw.len()))
                }
                '>' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Gt, raw.len()))
                }
                '/' => {
                    let (comment_kind, raw) = alt((
                        line_comment().map(|raw| (Some(CommentKind::Line), raw)),
                        block_comment().map(|raw| (Some(CommentKind::Block), raw)),
                        any.take().map(|x| (None, x)),
                    ))
                    .parse_next(input)?;
                    if let Some(comment_kind) = comment_kind {
                        let start = Point {
                            offset: idx as u32,
                            row: row as u32,
                            column: column as u32,
                        };
                        let end = end_point_for_raw(start, raw);
                        comments.push(Comment {
                            kind: comment_kind,
                            raw,
                            span: Span::At {
                                start,
                                end,
                                file: file.clone(),
                            },
                        });
                        if let Some(extra_len) = raw.chars().rev().position(|x| x == '\n') {
                            last_newline = input.len() + extra_len;
                        }
                        row += raw.chars().filter(|&x| x == '\n').count();
                        idx += raw.len();
                        None
                    } else {
                        Some((raw, TokenKind::Slash, raw.len()))
                    }
                }
                '@' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::At, raw.len()))
                }
                ',' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Comma, raw.len()))
                }
                '.' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Dot, raw.len()))
                }
                '=' => {
                    let (raw, kind) = alt((
                        ("=>").map(|raw| (raw, TokenKind::FatArrow)),
                        ("=").map(|raw| (raw, TokenKind::Eq)),
                    ))
                    .parse_next(input)?;
                    Some((raw, kind, raw.len()))
                }
                '!' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Bang, raw.len()))
                }
                '?' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Quest, raw.len()))
                }
                '*' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Star, raw.len()))
                }
                _ => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Unknown, raw.len()))
                }
            }) else {
                continue;
            };
            let start = Point {
                offset: idx as u32,
                row: row as u32,
                column: column as u32,
            };
            idx += len;
            let end = Point {
                offset: idx.try_into().expect("position too large"),
                row: row as u32,
                column: (column + len) as u32,
            };
            tokens.push(Token {
                kind,
                raw,
                span: Span::At {
                    start,
                    end,
                    file: file.clone(),
                },
            });
        }
        Ok(Lexed { tokens, comments })
    })(input)
    .expect("lexing failed")
}

fn end_point_for_raw(start: Point, raw: &str) -> Point {
    let newline_count = raw.bytes().filter(|&byte| byte == b'\n').count() as u32;
    let end_column = match raw.as_bytes().iter().rposition(|&byte| byte == b'\n') {
        Some(last_newline) => (raw.len() - last_newline - 1) as u32,
        None => start.column + raw.len() as u32,
    };
    Point {
        offset: start.offset + raw.len() as u32,
        row: start.row + newline_count,
        column: end_column,
    }
}

fn line_comment<'s, E>() -> impl Parser<&'s str, &'s str, E>
where
    E: ParserError<&'s str>,
{
    preceded("//", repeat(0.., (not("\n"), any)).map(|()| ())).take()
}

fn block_comment<'s, E>() -> impl Parser<&'s str, &'s str, E>
where
    E: ParserError<&'s str>,
{
    // below should be a valid block comment
    /* /* */ */
    // So have to consider nested comments
    let comment_block_rest = move |input: &mut &'s str| -> core::result::Result<(), E> {
        let mut nesting = 0;
        loop {
            let next_2 = match input.len() {
                0 => break Ok(()),
                1 => break Err(ParserError::from_input(input)),
                _ => &input.as_bytes()[..2],
            };
            match next_2 {
                s @ b"/*" => {
                    nesting += 1;
                    *input = &input[s.len()..];
                }
                s @ b"*/" if nesting > 0 => {
                    nesting -= 1;
                    *input = &input[s.len()..];
                }
                s @ b"*/" => {
                    *input = &input[s.len()..];
                    break Ok(());
                }
                _ => {
                    let mut it = input.chars();
                    it.next(); // skip a char
                    *input = it.as_str();
                }
            }
        }
    };
    preceded("/*", comment_block_rest).map(|()| ()).take()
}

#[cfg(test)]
mod lexer_test {
    use super::*;

    const FILE: FileName = FileName(arcstr::literal!("Test.par"));

    #[test]
    fn tok() {
        let tokens = lex(&mut "({[< ><>]}):Abc:abc_123: A\n_Ab", &FILE);
        assert_eq!(
            tokens.iter().map(|x| x.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::LParen,
                TokenKind::LCurly,
                TokenKind::LBrack,
                TokenKind::Lt,
                TokenKind::Gt,
                TokenKind::Link,
                TokenKind::RBrack,
                TokenKind::RCurly,
                TokenKind::RParen,
                TokenKind::Colon,
                TokenKind::UppercaseIdentifier,
                TokenKind::Colon,
                TokenKind::LowercaseIdentifier,
                TokenKind::Colon,
                TokenKind::UppercaseIdentifier,
                TokenKind::LowercaseIdentifier,
            ]
        );
        eprintln!("{:#?}", tokens);
    }

    #[test]
    fn block_comment() {
        let tokens = lex(
            &mut "abc/*\n\n/* comment \n*///\n*/ not_a_comment /*  */ /",
            &FILE,
        );
        eprintln!("{:#?}", tokens);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::LowercaseIdentifier,
                    raw: "abc",
                    span: Span::At {
                        start: Point {
                            offset: 0,
                            row: 0,
                            column: 0
                        },
                        end: Point {
                            offset: 3,
                            row: 0,
                            column: 3
                        },
                        file: FILE
                    },
                },
                Token {
                    kind: TokenKind::LowercaseIdentifier,
                    raw: "not_a_comment",
                    span: Span::At {
                        start: Point {
                            offset: 27,
                            row: 4,
                            column: 3
                        },
                        end: Point {
                            offset: 40,
                            row: 4,
                            column: 16
                        },
                        file: FILE
                    },
                },
                Token {
                    kind: TokenKind::Slash,
                    raw: "/",
                    span: Span::At {
                        start: Point {
                            offset: 48,
                            row: 4,
                            column: 24
                        },
                        end: Point {
                            offset: 49,
                            row: 4,
                            column: 25
                        },
                        file: FILE
                    },
                }
            ]
        )
    }

    #[test]
    fn float_literals_tokenize_as_single_tokens() {
        let tokens = lex("1.0 -0.5 +3.25 1_000.25 6.02e23 1.0e-6", &FILE);
        assert_eq!(
            tokens.iter().map(|token| token.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::Float,
                TokenKind::Float,
                TokenKind::Float,
                TokenKind::Float,
                TokenKind::Float,
                TokenKind::Float,
            ]
        );
        assert_eq!(
            tokens.iter().map(|token| token.raw).collect::<Vec<_>>(),
            vec!["1.0", "-0.5", "+3.25", "1_000.25", "6.02e23", "1.0e-6"]
        );
    }
}
