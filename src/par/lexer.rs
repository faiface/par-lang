use crate::location::{Point, Span};
use core::str::FromStr;
use winnow::{
    combinator::{alt, not, opt, peek, preceded, repeat},
    error::{EmptyError, ParserError},
    stream::{ParseSlice, TokenSlice},
    token::{any, literal, take, take_while},
    Parser, Result,
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
    Colon,
    Comma,
    Dot,
    Eq,
    Arrow,
    Bang,
    Quest,
    Star,
    Link,

    Integer,
    String,

    InvalidString,
    InvalidChar,

    LowercaseIdentifier,
    UppercaseIdentifier,
    Begin,
    Box,
    Case,
    Chan,
    Choice,
    Dec,
    Def,
    Do,
    Dual,
    Either,
    In,
    Iterative,
    Let,
    Loop,
    Recursive,
    Self_,
    Telltypes,
    Type,
    Unfounded,

    Unknown,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub raw: &'i str,
    pub span: Span,
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
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Eq => "=",
            TokenKind::Arrow => "=>",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Star => "*",
            TokenKind::Link => "<>",

            TokenKind::Integer => "integer",
            TokenKind::String => "string",

            TokenKind::InvalidString => "invalid string",
            TokenKind::InvalidChar => "invalid char",

            TokenKind::LowercaseIdentifier => "lower-case identifier",
            TokenKind::UppercaseIdentifier => "upper-case identifier",
            TokenKind::Begin => "begin",
            TokenKind::Box => "box",
            TokenKind::Case => "case",
            TokenKind::Chan => "chan",
            TokenKind::Choice => "choice",
            TokenKind::Dec => "dec",
            TokenKind::Def => "def",
            TokenKind::Do => "do",
            TokenKind::Dual => "dual",
            TokenKind::Either => "either",
            TokenKind::In => "in",
            TokenKind::Iterative => "iterative",
            TokenKind::Let => "let",
            TokenKind::Loop => "loop",
            TokenKind::Recursive => "recursive",
            TokenKind::Self_ => "self",
            TokenKind::Telltypes => "telltypes",
            TokenKind::Type => "type",
            TokenKind::Unfounded => "unfounded",

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

pub type Tokens<'i> = TokenSlice<'i, Token<'i>>;
pub type Input<'a> = Tokens<'a>;

pub fn lex<'s>(input: &'s str) -> Vec<Token<'s>> {
    type Error = EmptyError;
    (|input: &'s str| -> Result<Vec<Token<'s>>, Error> {
        let mut input = input;
        let input = &mut input;
        let mut row = 0;
        let mut last_newline = input.len();
        let mut tokens = Vec::new();
        let mut idx = 0;
        while let Ok(c) = peek(any::<&str, Error>).parse_next(input) {
            let column = last_newline - input.len(); // starting column
            let Some((raw, kind)) = (match c {
                '0'..='9' | '-' | '+' => {
                    let raw = (
                        take(1 as usize),
                        take_while(0.., |c| matches!(c, '0'..='9' | '_')),
                    )
                        .take()
                        .parse_next(input)?;
                    Some((raw, TokenKind::Integer))
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
                    Some((
                        raw,
                        if is_closed && is_valid {
                            TokenKind::String
                        } else {
                            TokenKind::InvalidString
                        },
                    ))
                }
                'a'..='z' | 'A'..='Z' | '_' => {
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
                        "chan" => TokenKind::Chan,
                        "choice" => TokenKind::Choice,
                        "dec" => TokenKind::Dec,
                        "def" => TokenKind::Def,
                        "do" => TokenKind::Do,
                        "dual" => TokenKind::Dual,
                        "either" => TokenKind::Either,
                        "in" => TokenKind::In,
                        "iterative" => TokenKind::Iterative,
                        "let" => TokenKind::Let,
                        "loop" => TokenKind::Loop,
                        "recursive" => TokenKind::Recursive,
                        "self" => TokenKind::Self_,
                        "telltypes" => TokenKind::Telltypes,
                        "type" => TokenKind::Type,
                        "unfounded" => TokenKind::Unfounded,
                        raw => {
                            if raw.starts_with(char::is_uppercase) {
                                TokenKind::UppercaseIdentifier
                            } else {
                                TokenKind::LowercaseIdentifier
                            }
                        }
                    };
                    Some((raw, kind))
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
                    Some((raw, TokenKind::Colon))
                }
                '[' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LBrack))
                }
                ']' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RBrack))
                }
                '(' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LParen))
                }
                ')' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RParen))
                }
                '{' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::LCurly))
                }
                '}' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RCurly))
                }
                '<' => Some(
                    alt((
                        "<>".map(|raw| (raw, TokenKind::Link)),
                        "<".map(|raw| (raw, TokenKind::Lt)),
                    ))
                    .parse_next(input)?,
                ),
                '>' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Gt))
                }
                '/' => {
                    let (is_comment, raw) = alt((
                        comment().take().map(|x| (true, x)),
                        any.take().map(|x| (false, x)),
                    ))
                    .parse_next(input)?;
                    if is_comment {
                        if let Some(extra_len) = raw.chars().rev().position(|x| x == '\n') {
                            last_newline = input.len() + extra_len;
                        }
                        row += raw.chars().filter(|&x| x == '\n').count();
                        idx += raw.len();
                        None
                    } else {
                        Some((raw, TokenKind::Slash))
                    }
                }
                ',' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Comma))
                }
                '.' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Dot))
                }
                '=' => Some(
                    alt((
                        ("=>").map(|raw| (raw, TokenKind::Arrow)),
                        ("=").map(|raw| (raw, TokenKind::Eq)),
                    ))
                    .parse_next(input)?,
                ),
                '!' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Bang))
                }
                '?' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Quest))
                }
                '*' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Star))
                }
                _ => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Unknown))
                }
            }) else {
                continue;
            };
            let start = Point {
                offset: idx,
                row,
                column,
            };
            idx += raw.len();
            let end = Point {
                offset: idx,
                row,
                column: column + raw.len(),
            };
            tokens.push(Token {
                kind,
                raw,
                span: Span::At { start, end },
            });
        }
        Ok(tokens)
    })(input)
    .expect("lexing failed")
}

pub fn comment<'s, E>() -> impl Parser<&'s str, &'s str, E>
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
    alt((
        preceded("//", repeat(0.., (not("\n"), any)).map(|()| ())),
        preceded("/*", comment_block_rest).map(|()| ()),
    ))
    //.context(StrContext::Label("comment"))
    .take()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tok() {
        let tokens = lex(&mut "({[< ><>]}):Abc:abc_123: A\n_Ab");
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
        let tokens = lex(&mut "abc/*\n\n/* comment \n*///\n*/ not_a_comment /*  */ /");
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
                        }
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
                        }
                    },
                },
                Token {
                    kind: TokenKind::Unknown,
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
                        }
                    },
                }
            ]
        )
    }
}
