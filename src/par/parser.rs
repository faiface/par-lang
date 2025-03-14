use super::{
    language::Expression,
    parse::{Loc, Name, Program},
    types::Type,
};
use core::ops::DerefMut;
use indexmap::IndexMap;
use winnow::{
    ascii::{alpha1, alphanumeric1, line_ending, space0, space1},
    combinator::{
        alt, cut_err, delimited, dispatch, fail, not, opt, peek, preceded, repeat, separated,
        separated_pair, seq, terminated, todo, trace,
    },
    error::{ContextError, ErrMode, ModalError, ParserError, StrContext},
    stream::{Accumulate, Compare, Range, Stream, StreamIsPartial},
    token::any,
    LocatingSlice, ModalResult as Result, Parser,
};

// struct State;
// type Input<'a> = Stateful<&'a str, State>;

type Input<'a> = LocatingSlice<&'a str>;
type Error = ErrMode<ContextError>;

/// Like regular `preceded` but cuts if `parser` after `ignored` fails, assuming that it should be unambiguous.
pub fn preceded_cut<Input, Ignored, Output, Error, IgnoredParser, ParseNext>(
    mut ignored: IgnoredParser,
    parser: ParseNext,
) -> impl Parser<Input, Output, Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    IgnoredParser: Parser<Input, Ignored, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    let mut parser = cut_err(parser);
    trace("preceded_cut", move |input: &mut Input| {
        let _ = ignored.parse_next(input)?;
        parser.parse_next(input)
    })
}

fn unit(u: ()) -> () {
    u
}
/// Adapt a parser which runs on `&str` to run over `Input`.
// fn as_str<'s, O, E>(mut parser: impl Parser<&'s str, O, E>) -> impl Parser<Input<'s>, O, E> {
//     move |input: &mut Input<'s>| {
//         let mut s: &str = input;
//         let res = parser.parse_next(&mut s)?;
//         let end = Input::new(s).checkpoint();
//         input.reset(&end);
//         Ok(res)
//     }
// }

fn whitespace<'s, T: Into<Range>, I>(
    occurrences: T,
) -> impl Parser<I, I::Slice, Error> + use<'s, I, T>
where
    I: Stream<Token = char> + StreamIsPartial + Compare<&'s str>,
{
    trace("whitespace", {
        repeat(
            occurrences,
            dispatch! {peek(any);
                ' '| '\t'| '\n'| '\r' => any.void(),
                _ => fail
            },
        )
        .map(unit)
        .take()
    })
}
/// Same as `~` in Pest. Matches 0 or more whitespace. Always successful. Returns if it matched at least 1 char.
fn ws<'s, I>(input: &mut I) -> Result<bool>
where
    I: Stream<Token = char> + StreamIsPartial + Compare<&'s str>,
{
    match whitespace(1..).parse_next(input) {
        Ok(_) => Ok(true),
        Err(_) => Ok(false),
    }
}

fn comment<'s, I>() -> impl Parser<I, I::Slice, Error> + use<'s, I>
where
    I: Stream + StreamIsPartial + Compare<&'s str>,
{
    // TODO can we add /* */ as accepted syntax?
    // delimited(("/*"), parser, ("*/"))
    preceded_cut("//", repeat(0.., (not("\n"), any)).map(unit))
        .take()
        .context(StrContext::Label("comment"))
}

fn keyword<'s, I>() -> impl Parser<I, I::Slice, Error> + use<'s, I>
where
    I: Stream + StreamIsPartial + Compare<&'s str>,
{
    alt((
        "type",
        "dec",
        "def",
        "chan",
        "let",
        "do",
        "in",
        "pass",
        "begin",
        "loop",
        "telltypes",
        "either",
        "recursive",
        "iterative",
        "self",
    ))
    .context(StrContext::Label("keyword"))
}

fn name<'s>(input: &mut Input<'s>) -> Result<Name> {
    let name_char = || alt(("_", alphanumeric1));
    (
        not((keyword(), not(name_char()))),
        (alpha1, repeat(0.., name_char()).map(unit)),
    )
        .take()
        .parse_to()
        .context(StrContext::Expected(
            winnow::error::StrContextValue::CharLiteral('_'),
        ))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description("alphanumeric"),
        ))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description("non-keyword"),
        ))
        .context(StrContext::Label("name"))
        .parse_next(input)
}

pub fn program(
    input: Input,
) -> std::result::Result<
    Program<Name, Expression<Loc, Name>>,
    winnow::error::ParseError<LocatingSlice<&str>, ContextError>,
> {
    enum Either<A, B, C, D> {
        A(A),
        B(B),
        C(C),
        D(D),
    }

    delimited(
        ws,
        repeat(
            0..,
            alt((
                type_def.map(Either::A),
                declaration.map(Either::B),
                definition.map(Either::C),
                (|input: &mut Input| match dbg!(ws.parse_next(input)) {
                    Ok(true) => Ok(()),
                    Ok(false) => dbg!(fail.parse_next(input)),
                    Err(e) => Err(e),
                })
                .map(Either::D),
                cut_err(
                    fail.context(StrContext::Label("item"))
                        .context(StrContext::Expected(
                            winnow::error::StrContextValue::Description("type"),
                        ))
                        .context(StrContext::Expected(
                            winnow::error::StrContextValue::Description("declaration"),
                        ))
                        .context(StrContext::Expected(
                            winnow::error::StrContextValue::Description("definition"),
                        )),
                ),
            )),
        )
        .fold(Program::default, |mut acc, item| {
            match item {
                Either::A((name, (params, typ))) => {
                    acc.type_defs.insert(name, (params, typ));
                }
                Either::B((name, typ)) => {
                    acc.declarations.insert(name, Some(typ));
                }
                Either::C((name, typ, expression)) => {
                    acc.declarations.insert(name.clone(), typ);
                    acc.definitions.insert(name, expression);
                }
                Either::D(_) => (),
            };
            acc
        }),
        ws,
    )
    .parse(input)
}

pub fn type_def(input: &mut Input) -> Result<(Name, (Vec<Name>, Type<Loc, Name>))> {
    preceded_cut(
        ("type", ws),
        seq!(name, _:ws, seq!(type_params, _:(ws, "=", ws), typ)),
    )
    .parse_next(input)
}

pub fn declaration(input: &mut Input) -> Result<(Name, Type<Loc, Name>)> {
    preceded_cut(("dec", ws), (separated_pair(name, (":", ws), typ)))
        .context(StrContext::Label("declaration"))
        .parse_next(input)
}

pub fn definition(
    input: &mut Input,
) -> Result<(Name, Option<Type<Loc, Name>>, Expression<Loc, Name>)> {
    preceded_cut(
        ("def", ws),
        seq!( name, _:ws,annotation,_:(ws,"=",ws), expression),
    )
    .context(StrContext::Label("definition"))
    .parse_next(input)
}

fn list<'s, P: Parser<I, O, Error>, I, O>(
    item: P,
) -> impl Parser<I, Vec<O>, Error> + use<'s, P, I, O>
where
    I: Stream<Token = char> + StreamIsPartial + Compare<&'s str>,
    Vec<O>: Accumulate<O>,
{
    terminated(separated(1.., item, (ws, ",", ws)), (ws, opt(",")))
}

fn typ(input: &mut Input) -> Result<Type<Loc, Name>> {
    // TODO, use `dispatch` to choose alternate based on peek prefix.
    // This should also help error messages.
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

fn typ_name(input: &mut Input) -> Result<Type<Loc, Name>> {
    trace(
        "typ_name",
        separated_pair(name, ws, type_args)
            .with_span()
            .map(|((name, typ_args), span)| Type::Name(Loc::from(span), name, typ_args)),
    )
    .parse_next(input)
}

fn typ_chan(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(("chan", ws), typ.context(StrContext::Label("chan type")))
        .with_span()
        .map(|(typ, span)| Type::Chan(Loc::from(span), Box::new(typ)))
        .parse_next(input)
}

fn typ_send(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(("(", ws), (terminated(list(typ), (ws, ")", ws)), typ))
        .with_span()
        .map(|((args, then), span)| {
            args.into_iter().rev().fold(then, |arg, then| {
                Type::Send(Loc::from(span.clone()), Box::new(arg), Box::new(then))
            })
        })
        .parse_next(input)
}

fn typ_receive(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(("[", ws), (terminated(list(typ), (ws, "]", ws)), typ))
        .with_span()
        .map(|((args, then), span)| {
            args.into_iter().rev().fold(then, |arg, then| {
                Type::Receive(Loc::from(span.clone()), Box::new(arg), Box::new(then))
            })
        })
        .parse_next(input)
}

fn typ_either(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(
        "either",
        seq!(
            _:(ws, "{", ws),
            repeat(0.., seq!(_:(".", ws), name, _:ws, typ, _:(ws, opt(",")))).fold(
                || IndexMap::new(),
                |mut branches, (name, typ)| {
                    branches.insert(name, typ);
                    branches
                }
            ),
            _:(ws, "}")
        ),
    )
    .with_span()
    .map(|((branches,), span)| Type::Either(Loc::from(span), branches))
    .parse_next(input)
}

fn typ_choice(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(
        ("{", ws),
        terminated(
            repeat(
                0..,
                seq!(_:(".", ws), name, _:ws, typ_branch, _:(ws, opt(","))),
            )
            .fold(
                || IndexMap::new(),
                |mut branches, (name, typ)| {
                    branches.insert(name, typ);
                    branches
                },
            ),
            (ws, "}"),
        ),
    )
    .with_span()
    .map(|((branches), span)| Type::Choice(Loc::from(span), branches))
    .parse_next(input)
}

fn typ_break(input: &mut Input) -> Result<Type<Loc, Name>> {
    "!".with_span()
        .map(|(_, span)| Type::Break(Loc::from(span)))
        .parse_next(input)
}

fn typ_continue(input: &mut Input) -> Result<Type<Loc, Name>> {
    "?".with_span()
        .map(|(_, span)| Type::Continue(Loc::from(span)))
        .parse_next(input)
}

fn typ_recursive(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(("recursive", ws), separated_pair(loop_label, ws, typ))
        .with_span()
        .map(|((label, typ), loc)| Type::Recursive(Loc::from(loc), label, Box::new(typ)))
        .parse_next(input)
}

fn typ_iterative<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(
        ("iterative", ws),
        separated_pair(loop_label, ws, typ).context(StrContext::Label("iterative type body")),
    )
    .with_span()
    .map(|((name, typ), span)| Type::Iterative(Loc::from(span), name, Box::new(typ)))
    .parse_next(input)
}

fn typ_self<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(
        ("self", ws),
        loop_label.context(StrContext::Label("self type loop label")),
    )
    .with_span()
    .map(|(label, span)| Type::Self_(Loc::from(span), label))
    .parse_next(input)
}

fn typ_send_type<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    preceded_cut(
        ("(", ws, "type", ws),
        separated_pair(
            list(name).context(StrContext::Label("list of type names to send")),
            (ws, ")", ws),
            typ,
        ),
    )
    .with_span()
    .map(|((names, typ), span)| {
        names.into_iter().rev().fold(typ, |body, name| {
            Type::SendType(Loc::from(span.clone()), name, Box::new(body))
        })
    })
    .parse_next(input)
}

fn typ_recv_type<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    preceded_cut(
        ("[", ws, "type", ws),
        separated_pair(
            list(name).context(StrContext::Label("list of type names to receive")),
            (ws, "]", ws),
            typ,
        ),
    )
    .with_span()
    .map(|((names, typ), span)| {
        names.into_iter().rev().fold(typ, |body, name| {
            Type::ReceiveType(Loc::from(span.clone()), name, Box::new(body))
        })
    })
    .parse_next(input)
}

fn type_params<'s>(input: &mut Input) -> Result<Vec<Name>> {
    opt(delimited(("<", ws), list(name), (ws, ">"))) // TODO should be able to use `<` to improve error message
        .map(Option::unwrap_or_default)
        .parse_next(input)
}

fn type_args<'s>(input: &mut Input) -> Result<Vec<Type<Loc, Name>>> {
    opt(delimited(("<", ws), list(typ), (ws, ">"))) // TODO should be able to use `<` to improve error message
        .map(Option::unwrap_or_default)
        .parse_next(input)
}

fn typ_branch<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    // TODO use `dispatch` to improve perf and errors.
    // try recv_type first so `(` is unambiguous on `typ_branch_received`
    alt((typ_branch_then, typ_branch_recv_type, typ_branch_receive)).parse_next(input)
}

fn typ_branch_then<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    preceded_cut(("=>", ws), typ).parse_next(input)
}

fn typ_branch_receive<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    preceded_cut(("(", ws), seq!(list(typ), _:(ws, ")", ws), typ_branch))
        .with_span()
        .map(|((args, then), span)| {
            args.into_iter().rev().fold(then, |acc, arg| {
                Type::Receive(Loc::from(span.clone()), Box::new(arg), Box::new(acc))
            })
        })
        .parse_next(input)
}

fn typ_branch_recv_type<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    preceded(
        ("(", ws, "type"),
        cut_err(seq!(list(name), _:(ws, ")", ws), typ_branch)),
    )
    .with_span()
    .map(|((names, body), span)| {
        names.into_iter().rev().fold(body, |acc, name| {
            Type::ReceiveType(Loc::from(span.clone()), name, Box::new(acc))
        })
    })
    .parse_next(input)
}

fn annotation(input: &mut Input) -> Result<Option<Type<Loc, Name>>> {
    opt(preceded_cut((":", ws), typ)).parse_next(input)
}

/*
pattern           = { pattern_name | pattern_receive | pattern_continue | pattern_recv_type }
pattern_name      = { name ~ annotation }
pattern_receive   = { "(" ~ pattern_list ~ ")" ~ pattern }
pattern_continue  = { "!" }
pattern_recv_type = { "(" ~ "type" ~ name_list ~ ")" ~ pattern }
 */

fn expression(input: &mut Input) -> Result<Expression<Loc, Name>> {
    // todo!()
    fail.context(StrContext::Expected(
        winnow::error::StrContextValue::Description("expression"),
    ))
    .parse_next(input)
}
/*
expression = { expr_let | expr_do | expr_fork | application | construction | ("{" ~ expression ~ "}") }
expr_let   = { "let" ~ pattern ~ "=" ~ expression ~ "in" ~ expression }
expr_do    = { "do" ~ "{" ~ process ~ "}" ~ "in" ~ expression }
expr_fork  = { "chan" ~ name ~ annotation ~ "{" ~ process ~ "}" }

construction   = { cons_begin | cons_loop | cons_then | cons_send | cons_receive | cons_choose | cons_either | cons_break | cons_send_type | cons_recv_type }
cons_then      = { expr_fork | expr_let | expr_do | application | ("{" ~ expression ~ "}") }
cons_send      = { "(" ~ expression_list ~ ")" ~ construction }
cons_receive   = { "[" ~ pattern_list ~ "]" ~ construction }
cons_choose    = { "." ~ name ~ construction }
cons_either    = { "{" ~ ("." ~ name ~ cons_branch ~ ","?)* ~ "}" }
cons_break     = { "!" }
cons_begin     = { "begin" ~ loop_label ~ construction }
cons_loop      = { "loop" ~ loop_label }
cons_send_type = { "(" ~ "type" ~ typ_list ~ ")" ~ construction }
cons_recv_type = { "[" ~ "type" ~ name_list ~ "]" ~ construction }

cons_branch           = { cons_branch_then | cons_branch_receive | cons_branch_recv_type }
cons_branch_then      = { "=>" ~ expression }
cons_branch_receive   = { "(" ~ pattern_list ~ ")" ~ cons_branch }
cons_branch_recv_type = { "(" ~ "type" ~ name_list ~ ")" ~ cons_branch }

application     = { (name | "{" ~ expression ~ "}") ~ apply }
apply           = { apply_begin | apply_loop | apply_send | apply_choose | apply_either | apply_send_type | apply_noop }
apply_noop      = { "" }
apply_send      = { "(" ~ expression_list ~ ")" ~ apply }
apply_choose    = { "." ~ name ~ apply }
apply_either    = { "{" ~ ("." ~ name ~ apply_branch ~ ","?)* ~ "}" }
apply_begin     = { "begin" ~ loop_label ~ apply }
apply_loop      = { "loop" ~ loop_label }
apply_send_type = { "(" ~ "type" ~ typ_list ~ ")" ~ apply }

apply_branch           = { apply_branch_then | apply_branch_receive | apply_branch_continue | apply_branch_recv_type }
apply_branch_then      = { name ~ "=>" ~ expression }
apply_branch_receive   = { "(" ~ pattern_list ~ ")" ~ apply_branch }
apply_branch_continue  = { "!" ~ "=>" ~ expression }
apply_branch_recv_type = { "(" ~ "type" ~ name_list ~ ")" ~ apply_branch }

process        = { proc_let | proc_pass | proc_telltypes | command | proc_noop }
proc_let       = { "let" ~ pattern ~ "=" ~ expression ~ process }
proc_pass      = { "pass" }
proc_telltypes = { "telltypes" ~ process }
proc_noop      = { "" }

command       = { name ~ cmd }
cmd           = { cmd_link | cmd_send | cmd_receive | cmd_choose | cmd_either | cmd_break | cmd_continue | cmd_begin | cmd_loop | cmd_send_type | cmd_recv_type | cmd_then }
cmd_then      = { process }
cmd_link      = { "<>" ~ expression }
cmd_send      = { "(" ~ expression_list ~ ")" ~ cmd }
cmd_receive   = { "[" ~ pattern_list ~ "]" ~ cmd }
cmd_choose    = { "." ~ name ~ cmd }
cmd_either    = { "{" ~ cmd_branches ~ "}" ~ pass_process? }
cmd_break     = { "!" }
cmd_continue  = { "?" ~ process }
cmd_begin     = { "begin" ~ loop_label ~ cmd }
cmd_loop      = { "loop" ~ loop_label }
cmd_send_type = { "(" ~ "type" ~ typ_list ~ ")" ~ cmd }
cmd_recv_type = { "[" ~ "type" ~ name_list ~ "]" ~ cmd }

pass_process = { proc_let | proc_pass | proc_telltypes | command }

cmd_branches         = { ("." ~ name ~ cmd_branch)* }
cmd_branch           = { cmd_branch_then | cmd_branch_receive | cmd_branch_continue | cmd_branch_recv_type }
cmd_branch_then      = { "=>" ~ "{" ~ process ~ "}" }
cmd_branch_receive   = { "(" ~ pattern_list ~ ")" ~ cmd_branch }
cmd_branch_continue  = { "!" ~ "=>" ~ "{" ~ process ~ "}" }
cmd_branch_recv_type = { "(" ~ "type" ~ name_list ~ ")" ~ cmd_branch }

*/

fn loop_label<'s>(input: &mut Input<'s>) -> Result<Option<Name>> {
    opt(preceded((":", ws), name)).parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_list() {
        let mut p = list("ab");
        assert_eq!(p.parse("ab"), Ok(vec!["ab"]));
        assert_eq!(p.parse("ab,ab,ab"), Ok(vec!["ab", "ab", "ab"]));
        assert_eq!(p.parse("ab,ab,ab,"), Ok(vec!["ab", "ab", "ab"]));
        assert!(p.parse("ab,ab,ab,,").is_err());
        assert!(p.parse("ba").is_err());
        let mut p = list(name);
        assert_eq!(
            p.parse(LocatingSlice::new("ab_12,asd, asdf3")),
            Ok(vec![
                Name {
                    string: "ab_12".to_owned()
                },
                Name {
                    string: "asd".to_owned()
                },
                Name {
                    string: "asdf3".to_owned()
                }
            ])
        );
    }
    #[test]
    fn test_loop_label() {
        assert_eq!(
            loop_label.with_span().parse(LocatingSlice::new(":one")),
            Ok((
                Some(Name {
                    string: "one".to_owned()
                }),
                0..4
            ))
        );
    }

    #[test]
    fn t() {
        match program(LocatingSlice::new(include_str!(
            "../../examples/semigroup_queue.par"
        ))) {
            Ok(x) => eprintln!("{x:?}"),
            Err(e) => {
                eprintln!("{e}");
                eprintln!("{:?}", e.into_inner().context().collect::<Vec<_>>())
            }
        }
    }
}
