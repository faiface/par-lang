use super::{parse::Loc, parser::comment};
use core::{ops::Range, str::FromStr};
use winnow::{
    combinator::{alt, peek},
    error::{EmptyError, ParserError},
    stream::{ParseSlice, TokenSlice},
    token::{any, literal, take_while},
    Parser, Result,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    LAngle,
    RAngle,
    LBrack,
    RBrack,
    Colon,
    Ident,
    Comma,
    Dot,
    Arrow,
    Equal,
    Bang,
    Quest,
    Link,
    Unknown,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub raw: &'i str,
    pub loc: super::parse::Loc,
    pub span: Range<usize>,
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
impl From<TokenKind> for &'static str {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "{",
            TokenKind::RCurly => "}",
            TokenKind::LBrack => "[",
            TokenKind::RBrack => "]",
            TokenKind::LAngle => "<",
            TokenKind::RAngle => ">",
            TokenKind::Colon => ":",
            TokenKind::Ident => "",
            TokenKind::Unknown => "",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Arrow => "=>",
            TokenKind::Equal => "=",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Link => "<>",
        }
    }
}

impl PartialEq<str> for Token<'_> {
    fn eq(&self, other: &str) -> bool {
        <&str>::from(self.kind) == other
            || (matches!(self.kind, TokenKind::Ident) && self.raw == other)
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, &other: &&str) -> bool {
        <&str>::from(self.kind) == other
            || (matches!(self.kind, TokenKind::Ident) && self.raw == other)
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
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    let ident = take_while(
                        0..,
                        |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
                    )
                    .take()
                    .parse_next(input)?;
                    Some((ident, TokenKind::Ident))
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
                        "<".map(|raw| (raw, TokenKind::LAngle)),
                    ))
                    .parse_next(input)?,
                ),
                '>' => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::RAngle))
                }
                '/' => {
                    let (is_comment, raw) = alt((
                        comment().take().map(|x| (true, x)),
                        any.take().map(|x| (false, x)),
                    ))
                    .parse_next(input)?;
                    if is_comment {
                        idx += raw.len();
                        None
                    } else {
                        Some((raw, TokenKind::Unknown))
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
                        ("=").map(|raw| (raw, TokenKind::Equal)),
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
                _ => {
                    let raw = any::<&str, Error>.take().parse_next(input)?;
                    Some((raw, TokenKind::Unknown))
                }
            }) else {
                continue;
            };
            tokens.push(Token {
                kind,
                raw,
                // one-based row/column
                loc: Loc::Code {
                    line: row + 1,
                    column: column + 1,
                },
                span: idx..idx + raw.len(),
            });
            idx += raw.len();
        }
        Ok(tokens)
    })(input)
    .expect("lexing failed")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tok() {
        let tokens = lex(&mut "({[< ><>]}):abc_123: a\nab");
        assert_eq!(
            tokens.iter().map(|x| x.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::LParen,
                TokenKind::LCurly,
                TokenKind::LBrack,
                TokenKind::LAngle,
                TokenKind::RAngle,
                TokenKind::Link,
                TokenKind::RBrack,
                TokenKind::RCurly,
                TokenKind::RParen,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Ident,
            ]
        );
        eprintln!("{:#?}", tokens);
    }
}
