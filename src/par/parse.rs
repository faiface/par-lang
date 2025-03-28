// why not rename this file to parser.rs?

use super::{
    language::{
        Apply, ApplyBranch, ApplyBranches, Command, CommandBranch, CommandBranches, Construct,
        ConstructBranch, ConstructBranches, Expression, Pattern, Process,
    },
    lexer::{lex, Input, Token, TokenKind},
    types::Type,
};
use crate::location::{Point, Span, Spanning};
use crate::par::language::{Declaration, Definition, Name, Program, TypeDef};
use core::fmt::Display;
use indexmap::IndexMap;
use miette::{SourceOffset, SourceSpan};
use winnow::token::literal;
use winnow::{
    combinator::{
        alt, cut_err, not, opt, preceded, repeat, separated, terminated,
        trace,
    },
    error::{
        AddContext, ContextError, ErrMode, ModalError, ParserError, StrContext, StrContextValue,
    },
    stream::{Accumulate, Compare, Stream, StreamIsPartial},
    token::any,
    Parser,
};

impl From<&Token<'_>> for Name {
    fn from(token: &Token) -> Self {
        Self {
            span: token.span,
            string: token.raw.to_owned()
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct MyError<C = StrContext> {
    context: Vec<(usize, ContextError<C>)>,
}
// impl<C> MyError<C> {
//     fn eof_offset(&self) -> usize {
//         self.context.iter().map(|x| x.0).min().unwrap_or(usize::MAX)
//     }
// }
pub type Error_ = MyError;
pub type Error = ErrMode<Error_>;
impl<I: Stream, C: core::fmt::Debug> ParserError<I> for MyError<C> {
    type Inner = Self;

    fn from_input(input: &I) -> Self {
        Self {
            context: vec![(input.eof_offset(), ContextError::from_input(input))],
        }
    }
    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
    fn append(self, _input: &I, _token_start: &<I as Stream>::Checkpoint) -> Self {
        self
    }
    fn or(mut self, other: Self) -> Self {
        self.context.extend(other.context);
        self
    }
}
impl<I: Stream, C> AddContext<I, C> for MyError<C> {
    fn add_context(
        mut self,
        input: &I,
        token_start: &<I as Stream>::Checkpoint,
        context: C,
    ) -> Self {
        let new_context = |context| {
            (
                input.eof_offset(),
                ContextError::new().add_context(input, token_start, context),
            )
        };
        if self.context.is_empty() {
            self.context.push(new_context(context));
            return self;
        }
        let last = self.context.pop().unwrap();
        if last.0 != input.eof_offset() {
            self.context.push(new_context(context));
            return self;
        }
        let last = (
            last.0.min(input.eof_offset()),
            last.1.add_context(input, token_start, context),
        );
        self.context.push(last);
        self
    }
}

pub type Result<O, E = MyError> = core::result::Result<O, ErrMode<E>>;

/// Token with additional context of expecting the `token` value
fn t<'i, E>(kind: TokenKind) -> impl Parser<Input<'i>, &'i Token<'i>, E>
where
    E: AddContext<Input<'i>, StrContext> + ParserError<Input<'i>>,
{
    literal(kind)
        .context(StrContext::Expected(StrContextValue::StringLiteral(kind.expected())))
        .map(|t: &[Token]| &t[0])
}

/// Like `t` for but for `n` tokens.
macro_rules! tn {
    ($s:literal: $($t:expr),+) => {
        ($($t),+).context(StrContext::Expected(StrContextValue::Description($s)))
    };
}

fn list<'i, P, O>(item: P) -> impl Parser<Input<'i>, Vec<O>, Error> + use<'i, P, O>
where
    P: Parser<Input<'i>, O, Error>,
    Vec<O>: Accumulate<O>,
{
    terminated(separated(1.., item, t(TokenKind::Comma)), opt(t(TokenKind::Comma)))
}

fn commit_after<Input, Prefix, Output, Error, PrefixParser, ParseNext>(
    prefix: PrefixParser,
    parser: ParseNext,
) -> impl Parser<Input, (Prefix, Output), Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    PrefixParser: Parser<Input, Prefix, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    trace("commit_after", (
        prefix,
        cut_err(parser),
    ))
}

/// Like `commit_after` but the prefix is optional and only cuts if the prefix is `Some`.
/// Also returns the prefix.
fn opt_commit_after<Input, Prefix, Output, Error, PrefixParser, ParseNext>(
    prefix: PrefixParser,
    mut parser: ParseNext,
) -> impl Parser<Input, (Option<Prefix>, Output), Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    PrefixParser: Parser<Input, Prefix, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    let mut prefix = opt(prefix);
    trace("opt_commit_after", move |input: &mut Input| {
        let prefix = prefix.parse_next(input)?;
        if prefix.is_some() {
            let n = cut_err(parser.by_ref()).parse_next(input)?;
            Ok((prefix, n))
        } else {
            let n = parser.parse_next(input)?;
            Ok((prefix, n))
        }
    })
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
    // .context(StrContext::Label("comment"))
    .take()
}

fn keyword<I>() -> impl Parser<I, I::Slice, Error>
where
    I: Stream + StreamIsPartial + for<'s> Compare<&'s str>,
{
    alt((
        "type",
        "dec",
        "def",
        "chan",
        "let",
        "do",
        "in",
        "begin",
        "unfounded",
        "loop",
        "telltypes",
        "either",
        "recursive",
        "iterative",
        "self",
    ))
    .context(StrContext::Label("keyword"))
}
/*
fn with_loc<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, Loc), E>
where
    E: ParserError<Input<'a>> + ModalError,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, Loc), E> {
        let loc = match peek(any::<_, E>).parse_next(input) {
            Ok(x) => x.loc.clone(),
            Err(e) => {
                let checkpoint = input.checkpoint();
                input.reset_to_start();
                let Some(last) = input.last() else {
                    return Err(e);
                };
                let res = match last.loc {
                    Loc::Code { line, column } => Loc::Code {
                        line,
                        column: column + 1,
                    },
                    Loc::External => Loc::External,
                };
                input.reset(&checkpoint);
                res
            }
        };
        let out = parser.parse_next(input)?;
        Ok((out, loc))
    }
}
#[allow(dead_code)]
fn with_span<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, core::ops::Range<usize>), E>
where
    E: ParserError<Input<'a>>,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, core::ops::Range<usize>), E> {
        let last = input.last().cloned();
        let start = peek(any).parse_next(input)?.span.start;
        let out = parser.parse_next(input)?;
        let end = peek::<_, &Token, E, _>(any)
            .parse_next(input)
            .unwrap_or(&last.unwrap()) // if input now empty, use that last token.
            .span
            .end;
        Ok((out, start..end))
    }
}
*/

fn name(input: &mut Input) -> Result<Name> {
    t(TokenKind::Identifier)
        .map(Name::from)
        .context(StrContext::Expected(StrContextValue::CharLiteral('_')))
        .context(StrContext::Expected(StrContextValue::Description("alphabetic")))
        .context(StrContext::Label("name"))
        .parse_next(input)
}

struct ProgramParseError {
    offset: usize,
    error: Error_,
}
impl ProgramParseError {
    fn offset(&self) -> usize {
        self.offset
    }
    fn inner(&self) -> &Error_ {
        &self.error
    }
}

fn program(
    mut input: Input,
) -> std::result::Result<Program<Name, Expression<Name>>, ProgramParseError> {
    pub enum Item<Name, Expr> {
        TypeDef(TypeDef<Name>),
        Declaration(Declaration<Name>),
        Definition(Definition<Name, Expr>, Option<Type<Name>>),
    }

    let parser = repeat(
        0..,
        alt((
            type_def.map(Item::TypeDef),
            declaration.map(Item::Declaration),
            definition.map(|(def, typ)| Item::Definition(def, typ)),
        ))
            .context(StrContext::Label("item")),
    )
        .fold(Program::default, |mut acc, item| {
            match item {
                Item::TypeDef(type_def) => {
                    acc.type_defs.push(type_def);
                }
                Item::Declaration(dec) => {
                    acc.declarations.push(dec);
                }
                Item::Definition(Definition { span, name, expression }, annotation) => {
                    if let Some(typ) = annotation {
                        acc.declarations.push(Declaration { span: span.clone(), name: name.clone(), typ });
                    }
                    acc.definitions.push(Definition { span, name, expression });
                }
            };
            acc
        });

    let start = input.checkpoint();
    (
        parser,
        winnow::combinator::eof
            .context(StrContext::Expected(StrContextValue::StringLiteral("type")))
            .context(StrContext::Expected(StrContextValue::StringLiteral("dec")))
            .context(StrContext::Expected(StrContextValue::StringLiteral("def")))
            .context(StrContext::Expected(StrContextValue::Description(
                "end of file",
            ))),
    )
        .parse_next(&mut input)
        .map(|(x, _eof)| x)
        .map_err(|e| {
            let e = e.into_inner().unwrap_or_else(|_err| {
                panic!("complete parsers should not report `ErrMode::Incomplete(_)`")
            });

            ProgramParseError {
                offset: winnow::stream::Offset::offset_from(&input, &start),
                error: ParserError::append(e, &input, &start),
            }
        })
}

#[derive(Debug, Clone, miette::Diagnostic)]
#[diagnostic(severity(Error))]
pub struct SyntaxError {
    #[label]
    source_span: SourceSpan,
    // Generate these with the miette! macro.
    // #[related]
    // related: Arc<[miette::ErrReport]>,
    #[help]
    help: String,

    span: Span
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "Syntax error.".fmt(f)
    }
}
impl core::error::Error for SyntaxError {}

impl Spanning for SyntaxError {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl SyntaxError {
    pub fn message(&self) -> &str {
        "Syntax error."
    }
}

pub fn set_miette_hook() {
    _ = miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .color(false)
                // .context_lines(1)
                // .with_cause_chain()
                .build(),
        )
    }));
}

pub fn parse_program(
    input: &str
) -> std::result::Result<Program<Name, Expression<Name>>, SyntaxError> {
    let tokens = lex(&input);
    let e = match program(Input::new(&tokens)) {
        Ok(x) => return Ok(x),
        Err(e) => e,
    };
    // Empty input doesn't error so this won't panic.
    let error_tok = tokens.get(e.offset()).unwrap_or(tokens.last().unwrap()).clone();
    let error_tok_span = error_tok.span.clone();
    Err(SyntaxError {
        span: error_tok_span,
        source_span: SourceSpan::new(SourceOffset::from(error_tok_span.start.offset), {
            match error_tok_span.len() {
                // miette unicode format for 1 length span is a hard-to-notice line, so don't set length to 1.
                x if x == 1 => 0,
                x => x,
            }
        }),
        help: e
            .inner()
            .context
            .iter()
            .map(|x| x.1.to_string().chars().chain(['\n']).collect::<String>())
            .collect::<String>(),
    })
}

fn type_def(input: &mut Input) -> Result<TypeDef<Name>> {
    commit_after(t(TokenKind::Type), (name, type_params, t(TokenKind::Eq), typ))
        .map(|(pre, (name, type_params, _, typ))| TypeDef {
            span: pre.span.join(typ.span()),
            name,
            params: type_params.map_or_else(Vec::new, |(_, params)| params),
            typ,
        })
        .context(StrContext::Label("type definition"))
        .parse_next(input)
}

fn declaration(input: &mut Input) -> Result<Declaration<Name>> {
    commit_after(t(TokenKind::Dec), (name, t(TokenKind::Colon), typ))
        .map(|(pre, (name, _, typ))| Declaration {
            span: pre.span.join(typ.span()),
            name,
            typ,
        })
        .context(StrContext::Label("declaration"))
        .parse_next(input)
}

fn definition(
    input: &mut Input,
) -> Result<(Definition<Name, Expression<Name>>, Option<Type<Name>>)> {
    commit_after(t(TokenKind::Def), (name, annotation, t(TokenKind::Eq), expression))
        .map(|(pre, (name, annotation, _, expression))| (
            Definition {
                span: pre.span.join(expression.span()),
                name,
                expression,
            },
            annotation,
        ))
        .context(StrContext::Label("definition"))
        .parse_next(input)
}

fn branches_body<'i, P, O>(
    branch: P,
) -> impl Parser<Input<'i>, (Span, IndexMap<Name, O>), Error> + use<'i, P, O>
where
    P: Parser<Input<'i>, O, Error>,
{
    commit_after(
        t(TokenKind::LCrl),
        (
            repeat(0.., (
                t(TokenKind::Dot), name, cut_err(branch), opt(t(TokenKind::Comma))
            )).fold(
                || IndexMap::new(),
                |mut branches, (_, name, branch, _)| {
                    branches.insert(name, branch);
                    branches
                },
            ),
            t(TokenKind::RCrl),
        ),
    )
        .map(|(open, (branches, close))| (
            open.span.join(close.span),
            branches,
        ))
        .context(StrContext::Label("either/choice branches"))
}

fn typ(input: &mut Input) -> Result<Type<Name>> {
    alt((
        typ_name,
        typ_chan,
        typ_either,
        typ_choice,
        typ_break,
        typ_continue,
        typ_recursive,
        typ_iterative,
        typ_self,
        typ_send_type,
        typ_send, // try after send_type so matching `(` is unambiguous
        typ_recv_type,
        typ_receive, // try after recv_type so matching `[` is unambiguous
    ))
    .context(StrContext::Label("type"))
    .parse_next(input)
}

fn typ_name(input: &mut Input) -> Result<Type<Name>> {
    trace(
        "typ_name",
        (name, type_args)
            .map(|(name, type_args)| match type_args {
                Some((type_args_span, type_args)) => Type::Name(
                    name.span.join(type_args_span),
                    name,
                    type_args,
                ),
                None => Type::Name(name.span.clone(), name, vec![]),
            })
    )
    .parse_next(input)
}

fn typ_chan(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Chan),
        typ.context(StrContext::Label("chan type")),
    )
        .map(|(pre, typ)| Type::Chan(
            pre.span.join(typ.span()),
            Box::new(typ)
        ))
        .parse_next(input)
}

fn typ_send(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::LPar), (list(typ), t(TokenKind::RPar), typ))
        .map(|(open, (args, _, then))| Type::Send(
            open.span.join(then.span()),
            args,
            Box::new(then),
        ))
        .parse_next(input)
}

fn typ_receive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::LBkt), (list(typ), t(TokenKind::RBkt), typ))
        .map(|(open, (args, _, then))| Type::Receive(
            open.span.join(then.span()),
            args,
            Box::new(then),
        ))
        .parse_next(input)
}

fn typ_either(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::Either), branches_body(typ))
        .map(|(pre, (branches_span, branches))| Type::Either(
            pre.span.join(branches_span),
            branches,
        ))
        .parse_next(input)
}

fn typ_choice(input: &mut Input) -> Result<Type<Name>> {
    branches_body(typ_branch)
        .map(|(span, branches)| Type::Choice(span, branches))
        .parse_next(input)
}

fn typ_break(input: &mut Input) -> Result<Type<Name>> {
    t(TokenKind::Bang)
        .map(|token| Type::Break(token.span))
        .parse_next(input)
}

fn typ_continue(input: &mut Input) -> Result<Type<Name>> {
    t(TokenKind::Quest)
        .map(|token| Type::Continue(token.span))
        .parse_next(input)
}

fn typ_recursive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::Recursive), (loop_label, typ))
        .map(|(pre, (label, typ))| Type::Recursive {
            span: pre.span.join(typ.span()),
            asc: Default::default(),
            label,
            body: Box::new(typ),
        })
        .parse_next(input)
}

fn typ_iterative(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Iterative),
        (loop_label, typ).context(StrContext::Label("iterative type body")),
    )
    .map(|(pre, (label, typ))| Type::Iterative {
        span: pre.span.join(typ.span()),
        asc: Default::default(),
        label,
        body: Box::new(typ),
    })
    .parse_next(input)
}

fn typ_self(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Self_),
        loop_label.context(StrContext::Label("self type loop label")),
    )
    .map(|(token, label)| Type::Self_(
        match &label {
            Some(label) => token.span.join(label.span),
            None => token.span.clone(),
        },
        label
    ))
    .parse_next(input)
}

fn typ_send_type<'s>(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (
            list(name).context(StrContext::Label("list of type names to send")),
            t(TokenKind::RPar),
            typ,
        ),
    )
    .map(|((open, _), (names, _, typ))| Type::SendTypes(
        open.span.join(typ.span()),
        names,
        Box::new(typ)
    ))
    .parse_next(input)
}

fn typ_recv_type(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        tn!("[type": TokenKind::LBkt, TokenKind::Type),
        (
            list(name).context(StrContext::Label("list of type names to receive")),
            t(TokenKind::RBkt),
            typ,
        ),
    )
    .map(|((open, _), (names, _, typ))| Type::ReceiveTypes(
        open.span.join(typ.span()),
        names,
        Box::new(typ)
    ))
    .parse_next(input)
}

fn type_params(input: &mut Input) -> Result<Option<(Span, Vec<Name>)>> {
    // TODO should be able to use `<` to improve error message
    opt((t(TokenKind::Lt), list(name), t(TokenKind::Gt)))
        .map(|opt| opt.map(|(open, names, close)| (
            open.span.join(close.span),
            names
        )))
        .parse_next(input)
}

fn type_args<'s>(input: &mut Input) -> Result<Option<(Span, Vec<Type<Name>>)>> {
    // TODO should be able to use `<` to improve error message
    opt((t(TokenKind::Lt), list(typ), t(TokenKind::Gt)))
        .map(|opt| opt.map(|(open, types, close)| (
            open.span.join(close.span),
            types
        )))
        .parse_next(input)
}

fn typ_branch(input: &mut Input) -> Result<Type<Name>> {
    // try recv_type first so `(` is unambiguous on `typ_branch_received`
    alt((typ_branch_then, typ_branch_recv_type, typ_branch_receive)).parse_next(input)
}

fn typ_branch_then(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::Arrow), typ)
        .map(|(_, typ)| typ)
        .parse_next(input)
}

fn typ_branch_receive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::LPar), (list(typ), t(TokenKind::RPar), typ_branch))
        .map(|(open, (args, _, then))| Type::Receive(
            open.span.join(then.span()),
            args,
            Box::new(then),
        ))
        .parse_next(input)
}

fn typ_branch_recv_type(input: &mut Input) -> Result<Type<Name>> {
    (
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        cut_err((list(name), t(TokenKind::RPar), typ_branch)),
    )
    .map(|((open, _), (names, _, body))| Type::ReceiveTypes(
        open.span.join(body.span()),
        names,
        Box::new(body),
    ))
    .parse_next(input)
}

fn annotation(input: &mut Input) -> Result<Option<Type<Name>>> {
    opt(commit_after(t(TokenKind::Colon), typ))
        .map(|opt| opt.map(|(_, typ)| typ))
        .parse_next(input)
}

// pattern           = { pattern_name | pattern_receive | pattern_continue | pattern_recv_type }
fn pattern(input: &mut Input) -> Result<Pattern<Name>> {
    alt((
        pattern_name,
        pattern_receive_type,
        pattern_receive,
        pattern_continue,
    ))
    .parse_next(input)
}

fn pattern_name(input: &mut Input) -> Result<Pattern<Name>> {
    (name, annotation)
        .map(|(name, annotation)| Pattern::Name(
            match &annotation {
                Some(typ) => name.span.join(typ.span()),
                None => name.span.clone(),
            },
            name,
            annotation,
        ))
        .parse_next(input)
}

fn pattern_receive(input: &mut Input) -> Result<Pattern<Name>> {
    commit_after(t(TokenKind::LPar), (list(pattern), t(TokenKind::RPar), pattern))
        .map(|(open, (patterns, _, rest))| Pattern::Receive(
            open.span.join(rest.span()),
            patterns,
            Box::new(rest),
        ))
        .parse_next(input)
}

fn pattern_continue(input: &mut Input) -> Result<Pattern<Name>> {
    t(TokenKind::Bang)
        .map(|token| Pattern::Continue(token.span))
        .parse_next(input)
}

fn pattern_receive_type(input: &mut Input) -> Result<Pattern<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), pattern),
    )
    .map(|((open, _), (names, _, rest))| Pattern::ReceiveTypes(
        open.span.join(rest.span()),
        names,
        Box::new(rest),
    ))
    .parse_next(input)
}

fn expression(input: &mut Input) -> Result<Expression<Name>> {
    alt((
        expr_let,
        expr_do,
        expr_fork,
        application,
        construction.map(Expression::Construction),
        expr_grouped,
    ))
    .context(StrContext::Label("expression"))
    .parse_next(input)
}

fn expr_grouped(input: &mut Input) -> Result<Expression<Name>> {
    (t(TokenKind::LCrl), expression, t(TokenKind::RCrl))
        .map(|(open, expr, close)| Expression::Grouped(
            open.span.join(close.span),
            Box::new(expr),
        ))
        .parse_next(input)
}

fn expr_let(input: &mut Input) -> Result<Expression<Name>> {
    commit_after(
        t(TokenKind::Let),
        (pattern, t(TokenKind::Eq), expression, t(TokenKind::In), expression),
    )
    .map(|(pre, (pattern, _, expression, _, body))| Expression::Let {
        span: pre.span.join(body.span()),
        pattern,
        expression: Box::new(expression),
        then: Box::new(body),
    })
    .parse_next(input)
}

fn expr_do(input: &mut Input) -> Result<Expression<Name>> {
    commit_after(
        t(TokenKind::Do),
        (t(TokenKind::LCrl), opt(process), (t(TokenKind::RCrl), t(TokenKind::In)), expression),
    )
    .map(|(pre, (open, process, _, expression))| Expression::Do {
        span: pre.span.join(expression.span()),
        process: match process {
            Some(process) => Box::new(process),
            None => Box::new(Process::Noop(open.span.end)),
        },
        then: Box::new(expression),
    })
    .parse_next(input)
}

fn expr_fork(input: &mut Input) -> Result<Expression<Name>> {
    commit_after(
        t(TokenKind::Chan),
        (name, annotation, t(TokenKind::LCrl), opt(process), t(TokenKind::RCrl)),
    )
    .map(|(pre, (channel, annotation, open, process, close))| Expression::Fork {
        span: pre.span.join(close.span),
        channel,
        annotation,
        process: match process {
            Some(process) => Box::new(process),
            None => Box::new(Process::Noop(open.span.end)),
        },
    })
    .parse_next(input)
}

fn construction(input: &mut Input) -> Result<Construct<Name>> {
    alt((
        cons_begin,
        cons_loop,
        cons_then,
        cons_choose,
        cons_either,
        cons_break,
        cons_send_type,
        cons_send,
        cons_recv_type,
        cons_receive,
    ))
    .context(StrContext::Label("construction"))
    .parse_next(input)
}

fn cons_then(input: &mut Input) -> Result<Construct<Name>> {
    alt((
        expr_fork,
        expr_let,
        expr_do,
        application,
        expr_grouped,
    ))
        .map(Box::new)
        .map(Construct::Then)
        .parse_next(input)
}

fn cons_send(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (list(expression), t(TokenKind::RPar), construction),
    )
    .map(|(open, (arguments, _, construct))| Construct::Send(
        open.span.join(construct.span()),
        arguments,
        Box::new(construct),
    ))
    .parse_next(input)
}

fn cons_receive(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(t(TokenKind::LBkt), (list(pattern), t(TokenKind::RBkt), construction))
        .map(|(open, (patterns, _, construct))| Construct::Receive(
            open.span.join(construct.span()),
            patterns,
            Box::new(construct),
        ))
        .parse_next(input)
}

fn cons_choose(input: &mut Input) -> Result<Construct<Name>> {
    // Note this can't be a commit_after because its possible that this is not a choose construction, and instead a branch of an either.
    (t(TokenKind::Dot), (name, construction))
        .map(|(pre, (chosen, construct))| Construct::Choose(
            pre.span.join(construct.span()),
            chosen,
            Box::new(construct)
        ))
        .parse_next(input)
}

fn cons_either(input: &mut Input) -> Result<Construct<Name>> {
    branches_body(cons_branch)
        .map(|(span, branches)| Construct::Either(
            span,
            ConstructBranches(branches)
        ))
        .parse_next(input)
}

fn cons_break(input: &mut Input) -> Result<Construct<Name>> {
    t(TokenKind::Bang)
        .map(|token| Construct::Break(token.span))
        .parse_next(input)
}

fn cons_begin(input: &mut Input) -> Result<Construct<Name>> {
    opt_commit_after(
        t(TokenKind::Unfounded),
        commit_after(t(TokenKind::Begin), (loop_label, construction)),
    )
    .map(|(unfounded, (begin, (label, construct)))| Construct::Begin {
        span: match unfounded {
            Some(unfounded) => unfounded.span.join(construct.span()),
            None => begin.span.join(construct.span()),
        },
        unfounded: unfounded.is_some(),
        label,
        then: Box::new(construct)
    })
    .parse_next(input)
}

fn cons_loop(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(t(TokenKind::Loop), loop_label)
        .map(|(token, label)| Construct::Loop(
            match &label {
                Some(label) => token.span.join(label.span),
                None => token.span.clone(),
            },
            label
        ))
        .parse_next(input)
}

fn cons_send_type(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(typ), t(TokenKind::RPar), construction),
    )
    .map(|((open, _), (names, _, construct))| Construct::SendTypes(
        open.span.join(construct.span()),
        names,
        Box::new(construct),
    ))
    .parse_next(input)
}

fn cons_recv_type(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        tn!("[type": TokenKind::LBkt, TokenKind::Type),
        (list(name), t(TokenKind::RBkt), construction),
    )
    .map(|((open, _), (names, _, construct))| Construct::ReceiveTypes(
        open.span.join(construct.span()),
        names,
        Box::new(construct),
    ))
    .parse_next(input)
}

fn cons_branch(input: &mut Input) -> Result<ConstructBranch<Name>> {
    alt((cons_branch_then, cons_branch_recv_type, cons_branch_receive)).parse_next(input)
}

fn cons_branch_then(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(t(TokenKind::Arrow), expression)
        .map(|(pre, expression)| ConstructBranch::Then(
            pre.span.join(expression.span()),
            expression,
        ))
        .parse_next(input)
}

fn cons_branch_receive(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(t(TokenKind::LPar), (list(pattern), t(TokenKind::RPar), cons_branch))
        .map(|(open, (patterns, _, branch))| ConstructBranch::Receive(
            open.span.join(branch.span()),
            patterns,
            Box::new(branch),
        ))
        .parse_next(input)
}

fn cons_branch_recv_type(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), cons_branch),
    )
    .map(|((open, _), (names, _, branch))| ConstructBranch::ReceiveTypes(
        open.span.join(branch.span()),
        names,
        Box::new(branch),
    ))
    .parse_next(input)
}

fn application(input: &mut Input) -> Result<Expression<Name>> {
    (
        alt((
            name.map(|name| Expression::Reference(name.span, name)),
            expr_grouped,
        )),
        apply,
    )
    .map(|(expr, apply)| match apply {
        Some(apply) => Expression::Application(
            expr.span().join(apply.span()),
            Box::new(expr),
            apply,
        ),
        None => expr,
    })
    .context(StrContext::Label("application"))
    .parse_next(input)
}

fn apply(input: &mut Input) -> Result<Option<Apply<Name>>> {
    opt(alt((
        apply_begin,
        apply_loop,
        apply_choose,
        apply_either,
        apply_send_type,
        apply_send,
    )))
        .parse_next(input)
}

fn apply_send(input: &mut Input) -> Result<Apply<Name>> {
    commit_after(t(TokenKind::LPar), (list(expression), t(TokenKind::RPar), apply))
        .map(|(open, (arguments, close, apply))| {
            let apply = match apply {
                Some(apply) => apply,
                None => Apply::Noop(close.span.end)
            };
            Apply::Send(
                open.span.join(apply.span()),
                arguments,
                Box::new(apply),
            )
        })
        .parse_next(input)
}

fn apply_choose(input: &mut Input) -> Result<Apply<Name>> {
    commit_after(t(TokenKind::Dot), (name, apply))
        .map(|(pre, (chosen, then))| {
            let then = match then {
                Some(then) => then,
                None => Apply::Noop(chosen.span.end)
            };
            Apply::Choose(
                pre.span.join(then.span()),
                chosen,
                Box::new(then)
            )
        })
        .parse_next(input)
}

fn apply_either(input: &mut Input) -> Result<Apply<Name>> {
    branches_body(apply_branch)
        .map(|(span, branches)| Apply::Either(span, ApplyBranches(branches)))
        .parse_next(input)
}

fn apply_begin(input: &mut Input) -> Result<Apply<Name>> {
    opt_commit_after(
        t(TokenKind::Unfounded),
        commit_after(t(TokenKind::Begin), (loop_label, apply)),
    )
    .map(|(unfounded, (begin, (label, then)))| {
        let then = match (&label, then) {
            (_, Some(then)) => then,
            (Some(label), None) => Apply::Noop(label.span.end),
            (None, None) => Apply::Noop(begin.span.end),
        };
        Apply::Begin {
            span: match unfounded {
                Some(unfounded) => unfounded.span.join(then.span()),
                None => begin.span.join(then.span()),
            },
            unfounded: unfounded.is_some(),
            label,
            then: Box::new(then),
        }
    })
    .parse_next(input)
}

fn apply_loop(input: &mut Input) -> Result<Apply<Name>> {
    commit_after(t(TokenKind::Loop), loop_label)
        .map(|(token, label)| Apply::Loop(
            match &label {
                Some(label) => token.span.join(label.span),
                None => token.span.clone(),
            },
            label
        ))
        .parse_next(input)
}

fn apply_send_type(input: &mut Input) -> Result<Apply<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(typ), t(TokenKind::RPar), apply)
    )
        .map(|((open, _), (types, close, apply))| {
            let apply = match apply {
                Some(apply) => apply,
                None => Apply::Noop(close.span.end)
            };
            Apply::SendTypes(
                open.span.join(apply.span()),
                types,
                Box::new(apply),
            )
        })
        .parse_next(input)
}

fn apply_branch(input: &mut Input) -> Result<ApplyBranch<Name>> {
    alt((
        apply_branch_then,
        apply_branch_recv_type,
        apply_branch_receive,
        apply_branch_continue,
    ))
    .parse_next(input)
}

fn apply_branch_then(input: &mut Input) -> Result<ApplyBranch<Name>> {
    (name, cut_err((t(TokenKind::Arrow), expression)))
        .map(|(name, (_, expression))| ApplyBranch::Then(
            name.span.join(expression.span()),
            name,
            expression
        ))
        .parse_next(input)
}

fn apply_branch_receive(input: &mut Input) -> Result<ApplyBranch<Name>> {
    commit_after(t(TokenKind::LPar), (list(pattern), t(TokenKind::RPar), apply_branch))
        .map(|(open, (patterns, _, branch))| ApplyBranch::Receive(
            open.span.join(branch.span()),
            patterns,
            Box::new(branch)
        ))
        .parse_next(input)
}

fn apply_branch_continue(input: &mut Input) -> Result<ApplyBranch<Name>> {
    commit_after(t(TokenKind::Bang), (t(TokenKind::Arrow), expression))
        .map(|(token, (_, expression))| ApplyBranch::Continue(
            token.span.join(expression.span()),
            expression
        ))
        .parse_next(input)
}

fn apply_branch_recv_type(input: &mut Input) -> Result<ApplyBranch<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), apply_branch),
    )
    .map(|((open, _), (names, _, branch))| ApplyBranch::ReceiveTypes(
        open.span.join(branch.span()),
        names,
        Box::new(branch),
    ))
    .parse_next(input)
}

fn process(input: &mut Input) -> Result<Process<Name>> {
    alt((proc_let, proc_telltypes, command))
        .context(StrContext::Label("process"))
        .parse_next(input)
}

fn proc_let(input: &mut Input) -> Result<Process<Name>> {
    commit_after(
        t(TokenKind::Let),
        (pattern, t(TokenKind::Eq), expression, opt(process)),
    )
    .map(|(pre, (pattern, _, expression, process))| Process::Let {
        span: pre.span.join(expression.span()),
        pattern,
        then: match process {
            Some(process) => Box::new(process),
            None => Box::new(Process::Noop(expression.span().end))
        },
        value: Box::new(expression),
    })
    .parse_next(input)
}

fn proc_telltypes(input: &mut Input) -> Result<Process<Name>> {
    commit_after(t(TokenKind::Telltypes), opt(process))
        .map(|(token, process)| Process::Telltypes(
            token.span,
            match process {
                Some(process) => Box::new(process),
                None => Box::new(Process::Noop(token.span.end))
            }
        ))
        .parse_next(input)
}

fn command(input: &mut Input) -> Result<Process<Name>> {
    (name, cmd)
        .map(|(name, cmd)| match cmd {
            Some(cmd) => Process::Command(name, cmd),
            None => {
                let noop_span = name.span.end;
                Process::Command(name, noop_cmd(noop_span))
            }
        })
        .parse_next(input)
}

fn noop_cmd<Name>(point: Point) -> Command<Name> {
    Command::Then(Box::new(Process::Noop(point)))
}

fn cmd(input: &mut Input) -> Result<Option<Command<Name>>> {
    alt((
        alt((
            cmd_link,
            cmd_choose,
            cmd_either,
            cmd_break,
            cmd_continue,
            cmd_begin,
            cmd_loop,
            cmd_send_type,
            cmd_send,
            cmd_recv_type,
            cmd_receive,
        )).map(Some),
        cmd_then
    ))
        .context(StrContext::Label("command"))
        .parse_next(input)
}

fn cmd_then(input: &mut Input) -> Result<Option<Command<Name>>> {
    opt(process)
        .map(|opt| opt.map(|process| Command::Then(Box::new(process))))
        .parse_next(input)
}

fn cmd_link(input: &mut Input) -> Result<Command<Name>> {
    commit_after(t(TokenKind::Link), expression)
        .map(|(token, expression)| Command::Link(
            token.span.join(expression.span()),
            Box::new(expression)
        ))
        .parse_next(input)
}

fn cmd_send(input: &mut Input) -> Result<Command<Name>> {
    commit_after(t(TokenKind::LPar), (list(expression), t(TokenKind::RPar), cmd))
        .map(|(open, (expressions, close, cmd))| {
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => noop_cmd(close.span.end)
            };
            Command::Send(
                open.span.join(cmd.span()),
                expressions,
                Box::new(cmd),
            )
        })
        .parse_next(input)
}

fn cmd_receive(input: &mut Input) -> Result<Command<Name>> {
    commit_after(t(TokenKind::LBkt), (list(pattern), t(TokenKind::RBkt), cmd))
        .map(|(open, (patterns, close, cmd))| {
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => noop_cmd(close.span.end)
            };
            Command::Receive(
                open.span.join(cmd.span()),
                patterns,
                Box::new(cmd),
            )
        })
        .parse_next(input)
}

fn cmd_choose(input: &mut Input) -> Result<Command<Name>> {
    commit_after(t(TokenKind::Dot), (name, cmd))
        .map(|(pre, (name, cmd))| {
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => noop_cmd(name.span.end)
            };
            Command::Choose(
                pre.span.join(cmd.span()),
                name,
                Box::new(cmd)
            )
        })
        .parse_next(input)
}

fn cmd_either(input: &mut Input) -> Result<Command<Name>> {
    (branches_body(cmd_branch), opt(pass_process))
        .map(|((span, branches), pass_process)| Command::Either(
            span,
            CommandBranches(branches),
            pass_process.map(Box::new),
        ))
        .parse_next(input)
}

fn cmd_break(input: &mut Input) -> Result<Command<Name>> {
    t(TokenKind::Bang)
        .map(|token| Command::Break(token.span))
        .parse_next(input)
}

fn cmd_continue(input: &mut Input) -> Result<Command<Name>> {
    (t(TokenKind::Quest), opt(process))
        .map(|(token, process)| match process {
            Some(process) => Command::Continue(
                token.span.join(process.span()),
                Box::new(process)
            ),
            None => Command::Continue(
                token.span.clone(),
                Box::new(Process::Noop(token.span.end))
            ),
        })
        .parse_next(input)
}

fn cmd_begin(input: &mut Input) -> Result<Command<Name>> {
    opt_commit_after(
        t(TokenKind::Unfounded),
        commit_after(t(TokenKind::Begin), (loop_label, cmd)),
    )
    .map(|(unfounded, (begin, (label, cmd)))| {
        let cmd = match (&label, cmd) {
            (_, Some(cmd)) => cmd,
            (Some(label), None) => noop_cmd(label.span.end),
            (None, None) => noop_cmd(begin.span.end),
        };
        Command::Begin {
            span: match unfounded {
                Some(unfounded) => unfounded.span.join(cmd.span()),
                None => begin.span.join(cmd.span()),
            },
            unfounded: unfounded.is_some(),
            label,
            then: Box::new(cmd)
        }
    })
    .parse_next(input)
}

fn cmd_loop(input: &mut Input) -> Result<Command<Name>> {
    commit_after(t(TokenKind::Loop), loop_label)
        .map(|(token, label)| Command::Loop(
            match &label {
                Some(label) => token.span.join(label.span),
                None => token.span.clone(),
            },
            label
        ))
        .parse_next(input)
}

fn cmd_send_type(input: &mut Input) -> Result<Command<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(typ), t(TokenKind::RPar), cmd)
    )
        .map(|((open, _), (types, close, cmd))| {
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => noop_cmd(close.span.end)
            };
            Command::SendTypes(
                open.span.join(cmd.span()),
                types,
                Box::new(cmd),
            )
        })
        .parse_next(input)
}

fn cmd_recv_type(input: &mut Input) -> Result<Command<Name>> {
    commit_after(
        tn!("[type": TokenKind::LBkt, TokenKind::Type),
        (list(name), t(TokenKind::RBkt), cmd)
    )
        .map(|((open, _), (names, close, cmd))| {
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => noop_cmd(close.span.end)
            };
            Command::ReceiveTypes(
                open.span.join(cmd.span()),
                names,
                Box::new(cmd),
            )
        })
        .parse_next(input)
}

fn pass_process(input: &mut Input) -> Result<Process<Name>> {
    alt((proc_let, proc_telltypes, command)).parse_next(input)
}

fn cmd_branch(input: &mut Input) -> Result<CommandBranch<Name>> {
    alt((
        cmd_branch_then,
        cmd_branch_continue,
        cmd_branch_recv_type,
        cmd_branch_receive,
    ))
    .parse_next(input)
}

fn cmd_branch_then(input: &mut Input) -> Result<CommandBranch<Name>> {
    commit_after(t(TokenKind::Arrow), (t(TokenKind::LCrl), opt(process), t(TokenKind::RCrl)))
        .map(|(pre, (open, process, close))| CommandBranch::Then(
            pre.span.join(close.span),
            match process {
                Some(process) => process,
                None => Process::Noop(open.span.end),
            }
        ))
        .parse_next(input)
}

fn cmd_branch_receive(input: &mut Input) -> Result<CommandBranch<Name>> {
    commit_after(t(TokenKind::LPar), (list(pattern), t(TokenKind::RPar), cmd_branch))
        .map(|(open, (patterns, _, branch))| CommandBranch::Receive(
            open.span.join(branch.span()),
            patterns,
            Box::new(branch),
        ))
        .parse_next(input)
}

fn cmd_branch_continue(input: &mut Input) -> Result<CommandBranch<Name>> {
    commit_after(t(TokenKind::Bang), (t(TokenKind::Arrow), t(TokenKind::LCrl), opt(process), t(TokenKind::RCrl)))
        .map(|(token, (_, open, process, close))| CommandBranch::Continue(
            token.span.join(close.span),
            match process {
                Some(process) => process,
                None => Process::Noop(open.span.end),
            }
        ))
        .parse_next(input)
}

fn cmd_branch_recv_type(input: &mut Input) -> Result<CommandBranch<Name>> {
    commit_after(
        tn!("(type": TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), cmd_branch),
    )
    .map(|((open, _), (names, _, branch))| CommandBranch::ReceiveTypes(
        open.span.join(branch.span()),
        names,
        Box::new(branch),
    ))
    .parse_next(input)
}

fn loop_label(input: &mut Input) -> Result<Option<Name>> {
    opt(preceded(t(TokenKind::Colon), name)).parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;

    /*
    #[test]
    fn test_list() {
        let mut p = list(TokenKind::ab);
        assert_eq!(p.parse("ab").unwrap(), vec!["ab"]);
        assert_eq!(p.parse("ab,ab,ab").unwrap(), vec!["ab", "ab", "ab"]);
        assert_eq!(p.parse("ab,ab,ab,").unwrap(), vec!["ab", "ab", "ab"]);
        assert!(p.parse("ab,ab,ab,,").is_err());
        assert!(p.parse("ba").is_err());
        let toks = lex("ab_12,asd, asdf3");
        let toks = Input::new(&toks);
        {
            assert_eq!(
                list(name).parse(toks).unwrap(),
                vec![
                    Name {
                        string: "ab_12".to_owned()
                    },
                    Name {
                        string: "asd".to_owned()
                    },
                    Name {
                        string: "asdf3".to_owned()
                    }
                ]
            );
        }
    }

    #[test]
    fn test_loop_label() {
        let toks = lex(":one");
        let toks = Input::new(&toks);
        assert_eq!(
            with_span(loop_label).parse(toks).unwrap(),
            (
                Some(Name {
                    string: "one".to_owned()
                }),
                0..4
            )
        );
    }
*/
    #[test]
    fn test_parse_examples() {
        let input = include_str!("../../examples/sample.par");
        assert!(parse_program(input).is_ok());
        let input = include_str!("../../examples/semigroup_queue.par");
        assert!(parse_program(input).is_ok());
        let input = include_str!("../../examples/rock_paper_scissors.par");
        assert!(parse_program(input).is_ok());
        let input = include_str!("../../examples/flatten.par");
        assert!(parse_program(input).is_ok());
        let input = include_str!("../../examples/fibonacci.par");
        assert!(parse_program(input).is_ok());
        let input = include_str!("../../examples/bubble_sort.par");
        assert!(parse_program(input).is_ok());
        let input = "begin the errors";
        assert!(parse_program(input).is_err());
    }
}
