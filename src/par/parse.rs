use std::fmt::Display;

use indexmap::IndexMap;
use pest::{
    error::LineColLocation,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use super::{
    language::{
        Apply, ApplyBranch, ApplyBranches, Command, CommandBranch, CommandBranches, Construct,
        ConstructBranch, ConstructBranches, Expression, Pattern, Process,
    },
    types::Type,
};

#[derive(Parser)]
#[grammar = "par/syntax.pest"]
pub struct Par;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Loc {
    Code { line: usize, column: usize },
    External,
}

impl Default for Loc {
    fn default() -> Self {
        Self::External
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Code { line, column } => write!(f, "{}:{}", line, column),
            Self::External => write!(f, "#:#"),
        }
    }
}

impl Loc {
    pub fn from(pair: &Pair<Rule>) -> Loc {
        let (line, column) = pair.line_col();
        Loc::Code { line, column }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name {
    pub string: String,
}

impl From<String> for Name {
    fn from(string: String) -> Self {
        Self { string }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Loc,
    pub msg: String,
}

#[derive(Clone, Debug)]
pub struct Program<Name, Expr> {
    pub type_defs: IndexMap<Name, (Vec<Name>, Type<Loc, Name>)>,
    pub declarations: IndexMap<Name, Option<Type<Loc, Name>>>,
    pub definitions: IndexMap<Name, Expr>,
}

impl<Name, Expr> Default for Program<Name, Expr> {
    fn default() -> Self {
        Self {
            type_defs: Default::default(),
            declarations: Default::default(),
            definitions: Default::default(),
        }
    }
}

pub fn parse_program(source: &str) -> Result<Program<Name, Expression<Loc, Name>>, ParseError> {
    let mut pairs = match Par::parse(Rule::program, source) {
        Ok(mut pairs) => pairs.next().unwrap().into_inner(),
        Err(error) => {
            return Err(ParseError {
                msg: error.variant.message().into_owned(),
                location: match error.line_col {
                    LineColLocation::Pos((line, column))
                    | LineColLocation::Span((line, column), _) => Loc::Code { line, column },
                },
            })
        }
    };

    let mut type_defs = IndexMap::new();
    let mut declarations = IndexMap::new();
    let mut definitions = IndexMap::new();

    while let Some(pair) = pairs.next() {
        match pair.as_rule() {
            Rule::type_def => {
                let mut pairs = pair.into_inner();
                let (_, name) = parse_name(&mut pairs)?;
                let params = parse_type_params(&mut pairs)?;
                let typ = parse_type(&mut pairs)?;
                type_defs.insert(name, (params, typ));
            }

            Rule::declaration => {
                let mut pairs = pair.into_inner();
                let (_, name) = parse_name(&mut pairs)?;
                let typ = parse_type(&mut pairs)?;
                declarations.insert(name, Some(typ));
            }

            Rule::definition => {
                let mut pairs = pair.into_inner();
                let (_, name) = parse_name(&mut pairs)?;
                let expression = parse_expression(&mut pairs)?;
                declarations.entry(name.clone()).or_insert(None);
                definitions.insert(name, expression);
            }

            Rule::EOI => break,

            _ => unreachable!(),
        }
    }

    Ok(Program {
        type_defs,
        declarations,
        definitions,
    })
}

fn parse_name(pairs: &mut Pairs<'_, Rule>) -> Result<(Loc, Name), ParseError> {
    let pair = pairs.next().unwrap();
    Ok((
        Loc::from(&pair),
        Name {
            string: String::from(pair.as_str()),
        },
    ))
}

fn parse_type_params(pairs: &mut Pairs<'_, Rule>) -> Result<Vec<Name>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();
    if let Some(pair) = pairs.next() {
        let params = parse_name_list(&mut Pairs::single(pair))?;
        Ok(params)
    } else {
        Ok(vec![])
    }
}

fn parse_type_args(pairs: &mut Pairs<'_, Rule>) -> Result<Vec<Type<Loc, Name>>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();

    if let Some(pair) = pairs.next() {
        let args = parse_type_list(&mut Pairs::single(pair))?;
        Ok(args)
    } else {
        Ok(vec![])
    }
}

fn parse_type(pairs: &mut Pairs<'_, Rule>) -> Result<Type<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::typ_name => {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let args = parse_type_args(&mut pairs)?;
            Ok(Type::Name(loc, name, args))
        }

        Rule::typ_chan => {
            let mut pairs = pair.into_inner();
            let typ = parse_type(&mut pairs)?;
            Ok(Type::Chan(loc, Box::new(typ)))
        }

        Rule::typ_send => {
            let mut pairs = pair.into_inner();
            let args = parse_type_list(&mut pairs)?;
            let mut then = parse_type(&mut pairs)?;
            for arg in args.into_iter().rev() {
                then = Type::Send(loc.clone(), Box::new(arg), Box::new(then));
            }
            Ok(then)
        }

        Rule::typ_receive => {
            let mut pairs = pair.into_inner();

            let args = parse_type_list(&mut pairs)?;
            let mut then = parse_type(&mut pairs)?;
            for arg in args.into_iter().rev() {
                then = Type::Receive(loc.clone(), Box::new(arg), Box::new(then));
            }
            Ok(then)
        }

        Rule::typ_either => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let typ = parse_type(&mut pairs)?;
                branches.insert(name, typ);
            }
            Ok(Type::Either(loc, branches))
        }

        Rule::typ_choice => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let typ = parse_type_branch(&mut pairs)?;
                branches.insert(name, typ);
            }
            Ok(Type::Choice(loc, branches))
        }

        Rule::typ_break => Ok(Type::Break(loc)),
        Rule::typ_continue => Ok(Type::Continue(loc)),

        Rule::typ_recursive => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let body = parse_type(&mut pairs)?;
            Ok(Type::Recursive(loc, label, Box::new(body)))
        }

        Rule::typ_iterative => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let body = parse_type(&mut pairs)?;
            Ok(Type::Iterative(loc, label, Box::new(body)))
        }

        Rule::typ_self => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Type::Self_(loc, label))
        }

        Rule::typ_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Type::Loop(loc, label))
        }

        Rule::typ_send_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let mut body = parse_type(&mut pairs)?;
            for name in names.into_iter().rev() {
                body = Type::SendType(loc.clone(), name, Box::new(body));
            }
            Ok(body)
        }

        Rule::typ_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let mut body = parse_type(&mut pairs)?;
            for name in names.into_iter().rev() {
                body = Type::ReceiveType(loc.clone(), name, Box::new(body));
            }
            Ok(body)
        }

        _ => unreachable!(),
    }
}

fn parse_type_branch(pairs: &mut Pairs<'_, Rule>) -> Result<Type<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::typ_branch_then => parse_type(&mut pair.into_inner()),

        Rule::typ_branch_receive => {
            let mut pairs = pair.into_inner();
            let args = parse_type_list(&mut pairs)?;
            let then = parse_type_branch(&mut pairs)?;
            let mut res = then;
            for arg in args.into_iter().rev() {
                res = Type::Receive(loc.clone(), Box::new(arg), Box::new(res));
            }
            Ok(res)
        }

        Rule::typ_branch_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let body = parse_type_branch(&mut pairs)?;
            let mut res = body;
            for name in names.into_iter().rev() {
                res = Type::ReceiveType(loc.clone(), name, Box::new(res));
            }
            Ok(res)
        }

        _ => unreachable!(),
    }
}

fn parse_annotation(pairs: &mut Pairs<'_, Rule>) -> Result<Option<Type<Loc, Name>>, ParseError> {
    match pairs.next().unwrap().into_inner().next() {
        Some(pair) => Ok(Some(parse_type(&mut Pairs::single(pair))?)),
        None => Ok(None),
    }
}

fn parse_pattern(pairs: &mut Pairs<'_, Rule>) -> Result<Pattern<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::pattern_name => {
            let mut pairs = pair.into_inner();
            let (loc, name) = parse_name(&mut pairs)?;
            let annotation = parse_annotation(&mut pairs)?;
            Ok(Pattern::Name(loc, name, annotation))
        }

        Rule::pattern_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let rest = parse_pattern(&mut pairs)?;
            let mut res = rest;
            for pattern in patterns.into_iter().rev() {
                res = Pattern::Receive(loc.clone(), Box::new(pattern), Box::new(res));
            }
            Ok(res)
        }

        Rule::pattern_continue => Ok(Pattern::Continue(loc)),

        Rule::pattern_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let rest = parse_pattern(&mut pairs)?;
            let mut res = rest;
            for name in names.into_iter().rev() {
                res = Pattern::ReceiveType(loc.clone(), name, Box::new(res));
            }
            Ok(res)
        }

        _ => unreachable!(),
    }
}

fn parse_expression(pairs: &mut Pairs<'_, Rule>) -> Result<Expression<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::expr_let => {
            let mut pairs = pair.into_inner();
            let pattern = parse_pattern(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            let body = parse_expression(&mut pairs)?;
            Ok(Expression::Let(
                loc,
                pattern,
                Box::new(expression),
                Box::new(body),
            ))
        }

        Rule::expr_do => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            Ok(Expression::Do(loc, Box::new(process), Box::new(expression)))
        }

        Rule::expr_fork => {
            let mut pairs = pair.into_inner();
            let (loc, name) = parse_name(&mut pairs)?;
            let annotation = parse_annotation(&mut pairs)?;
            let process = parse_process(&mut pairs)?;
            Ok(Expression::Fork(loc, name, annotation, Box::new(process)))
        }

        Rule::construction => Ok(Expression::Construction(
            loc,
            parse_construct(&mut Pairs::single(pair))?,
        )),

        Rule::application => {
            let mut pairs = pair.into_inner();
            let (loc, object_name) = parse_name(&mut pairs)?;
            let apply = parse_apply(&mut pairs)?;
            Ok(Expression::Application(loc, object_name, apply))
        }

        Rule::expression => parse_expression(&mut Pairs::single(pair)),

        _ => unreachable!(),
    }
}

fn parse_construct(pairs: &mut Pairs<'_, Rule>) -> Result<Construct<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cons_then => {
            let mut pairs = Pairs::single(pair);
            let expression = parse_expression(&mut pairs)?;
            Ok(Construct::Then(loc, Box::new(expression)))
        }

        Rule::cons_send => {
            let mut pairs = pair.into_inner();

            let arguments = parse_expression_list(&mut pairs)?;
            let mut construct = parse_construct(&mut pairs)?;
            for argument in arguments.into_iter().rev() {
                construct = Construct::Send(loc.clone(), Box::new(argument), Box::new(construct));
            }

            Ok(construct)
        }

        Rule::cons_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let mut construct = parse_construct(&mut pairs)?;
            for pattern in patterns.into_iter().rev() {
                construct = Construct::Receive(loc.clone(), pattern, Box::new(construct));
            }
            Ok(construct)
        }

        Rule::cons_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Choose(loc, chosen, Box::new(construct)))
        }

        Rule::cons_either => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let branch = parse_construct_branch(&mut pairs)?;
                branches.insert(name, branch);
            }
            Ok(Construct::Either(loc, ConstructBranches(branches)))
        }

        Rule::cons_break => Ok(Construct::Break(loc)),

        Rule::cons_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Begin(loc, label, Box::new(construct)))
        }

        Rule::cons_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Construct::Loop(loc, label))
        }

        Rule::cons_send_type => {
            let mut pairs = pair.into_inner();
            let names = parse_type_list(&mut pairs)?;
            let mut construct = parse_construct(&mut pairs)?;
            for name in names.into_iter().rev() {
                construct = Construct::SendType(loc.clone(), name, Box::new(construct));
            }
            Ok(construct)
        }

        Rule::cons_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let mut construct = parse_construct(&mut pairs)?;
            for name in names.into_iter().rev() {
                construct = Construct::ReceiveType(loc.clone(), name, Box::new(construct));
            }
            Ok(construct)
        }

        _ => unreachable!(),
    }
}

fn parse_expression_list(
    pairs: &mut Pairs<Rule>,
) -> Result<Vec<Expression<Loc, Name>>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();
    let mut exprs = Vec::new();
    while let Some(pair) = pairs.next() {
        let arg = parse_expression(&mut Pairs::single(pair))?;
        exprs.push(arg);
    }
    Ok(exprs)
}

fn parse_name_list(pairs: &mut Pairs<Rule>) -> Result<Vec<Name>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();
    let mut names = Vec::new();
    while let Some(pair) = pairs.next() {
        let (_, name) = parse_name(&mut Pairs::single(pair))?;
        names.push(name);
    }
    Ok(names)
}

fn parse_pattern_list(pairs: &mut Pairs<Rule>) -> Result<Vec<Pattern<Loc, Name>>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();
    let mut patterns = Vec::new();
    while let Some(pair) = pairs.next() {
        let pattern = parse_pattern(&mut Pairs::single(pair))?;
        patterns.push(pattern);
    }
    Ok(patterns)
}

fn parse_type_list(pairs: &mut Pairs<Rule>) -> Result<Vec<Type<Loc, Name>>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();
    let mut types = Vec::new();
    while let Some(pair) = pairs.next() {
        let arg = parse_type(&mut Pairs::single(pair))?;
        types.push(arg);
    }
    Ok(types)
}

fn parse_construct_branch(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<ConstructBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cons_branch_then => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(ConstructBranch::Then(loc, expression))
        }

        Rule::cons_branch_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let branch = parse_construct_branch(&mut pairs)?;
            let mut res = branch;
            for pattern in patterns.into_iter().rev() {
                res = ConstructBranch::Receive(loc.clone(), pattern, Box::new(res));
            }
            Ok(res)
        }

        Rule::cons_branch_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let branch = parse_construct_branch(&mut pairs)?;
            let mut res = branch;
            for name in names.into_iter().rev() {
                res = ConstructBranch::ReceiveType(loc.clone(), name, Box::new(res));
            }
            Ok(res)
        }

        _ => unreachable!(),
    }
}

fn parse_apply(pairs: &mut Pairs<'_, Rule>) -> Result<Apply<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::apply_noop => Ok(Apply::Noop(loc)),

        Rule::apply_send => {
            let mut pairs = pair.into_inner();
            let arguments = parse_expression_list(&mut pairs)?;
            let mut apply = parse_apply(&mut pairs)?;
            for argument in arguments.into_iter().rev() {
                apply = Apply::Send(loc.clone(), Box::new(argument), Box::new(apply));
            }
            Ok(apply)
        }

        Rule::apply_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let then = parse_apply(&mut pairs)?;
            Ok(Apply::Choose(loc, chosen, Box::new(then)))
        }

        Rule::apply_either => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let branch = parse_apply_branch(&mut pairs)?;
                branches.insert(name, branch);
            }
            Ok(Apply::Either(loc, ApplyBranches(branches)))
        }

        Rule::apply_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let then = parse_apply(&mut pairs)?;
            Ok(Apply::Begin(loc, label, Box::new(then)))
        }

        Rule::apply_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Apply::Loop(loc, label))
        }

        Rule::apply_send_type => {
            let mut pairs = pair.into_inner();
            let types = parse_type_list(&mut pairs)?;
            let mut apply = parse_apply(&mut pairs)?;
            for typ in types.into_iter().rev() {
                apply = Apply::SendType(loc.clone(), typ, Box::new(apply));
            }
            Ok(apply)
        }

        _ => unreachable!(),
    }
}

fn parse_apply_branch(pairs: &mut Pairs<'_, Rule>) -> Result<ApplyBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::apply_branch_then => {
            let mut pairs = pair.into_inner();
            let (loc, name) = parse_name(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            Ok(ApplyBranch::Then(loc, name, expression))
        }

        Rule::apply_branch_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let branch = parse_apply_branch(&mut pairs)?;
            let mut res = branch;
            for pattern in patterns.into_iter().rev() {
                res = ApplyBranch::Receive(loc.clone(), pattern, Box::new(res));
            }
            Ok(res)
        }

        Rule::apply_branch_continue => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(ApplyBranch::Continue(loc, expression))
        }

        Rule::apply_branch_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let branch = parse_apply_branch(&mut pairs)?;
            let mut res = branch;
            for name in names.into_iter().rev() {
                res = ApplyBranch::ReceiveType(loc.clone(), name, Box::new(res));
            }
            Ok(res)
        }

        _ => unreachable!(),
    }
}

fn parse_process(pairs: &mut Pairs<'_, Rule>) -> Result<Process<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::proc_let => {
            let mut pairs = pair.into_inner();
            let pattern = parse_pattern(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            let process = parse_process(&mut pairs)?;
            Ok(Process::Let(
                loc,
                pattern,
                Box::new(expression),
                Box::new(process),
            ))
        }

        Rule::proc_pass => Ok(Process::Pass(loc)),

        Rule::proc_telltypes => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(Process::Telltypes(loc, Box::new(process)))
        }

        Rule::command => {
            let mut pairs = pair.into_inner();
            let (_, object_name) = parse_name(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Process::Command(object_name, command))
        }

        Rule::proc_noop => Ok(Process::Noop(loc)),

        _ => unreachable!(),
    }
}

fn parse_command(pairs: &mut Pairs<'_, Rule>) -> Result<Command<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cmd_then => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(Command::Then(Box::new(process)))
        }

        Rule::cmd_link => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(Command::Link(loc, Box::new(expression)))
        }

        Rule::cmd_send => {
            let mut pairs = pair.into_inner();
            let arguments = parse_expression_list(&mut pairs)?;
            let mut command = parse_command(&mut pairs)?;
            for argument in arguments.into_iter().rev() {
                command = Command::Send(loc.clone(), Box::new(argument), Box::new(command));
            }
            Ok(command)
        }

        Rule::cmd_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let mut command = parse_command(&mut pairs)?;
            for pattern in patterns.into_iter().rev() {
                command = Command::Receive(loc.clone(), pattern, Box::new(command));
            }
            Ok(command)
        }

        Rule::cmd_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Choose(loc, chosen, Box::new(command)))
        }

        Rule::cmd_either => {
            let mut pairs = pair.into_inner();
            let branches = parse_command_branches(&mut pairs)?;
            let optional_process = match pairs.next() {
                Some(pair) => Some(parse_process(&mut Pairs::single(pair))?),
                None => None,
            };
            Ok(Command::Either(
                loc,
                branches,
                optional_process.map(Box::new),
            ))
        }

        Rule::cmd_break => Ok(Command::Break(loc)),

        Rule::cmd_continue => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(Command::Continue(loc, Box::new(process)))
        }

        Rule::cmd_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Begin(loc, label, Box::new(command)))
        }

        Rule::cmd_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Command::Loop(loc, label))
        }

        Rule::cmd_send_type => {
            let mut pairs = pair.into_inner();
            let types = parse_type_list(&mut pairs)?;
            let mut command = parse_command(&mut pairs)?;
            for typ in types.into_iter().rev() {
                command = Command::SendType(loc.clone(), typ, Box::new(command));
            }
            Ok(command)
        }

        Rule::cmd_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let mut command = parse_command(&mut pairs)?;
            for name in names.into_iter().rev() {
                command = Command::ReceiveType(loc.clone(), name, Box::new(command));
            }
            Ok(command)
        }

        _ => unreachable!(),
    }
}

fn parse_command_branches(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<CommandBranches<Loc, Name>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();

    let mut branches = IndexMap::new();
    while let Some(pair) = pairs.next() {
        let (_, name) = parse_name(&mut Pairs::single(pair))?;
        let branch = parse_command_branch(&mut pairs)?;
        branches.insert(name, branch);
    }
    Ok(CommandBranches(branches))
}

fn parse_command_branch(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<CommandBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cmd_branch_then => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(CommandBranch::Then(process))
        }

        Rule::cmd_branch_receive => {
            let mut pairs = pair.into_inner();
            let patterns = parse_pattern_list(&mut pairs)?;
            let branch = parse_command_branch(&mut pairs)?;
            let mut res = branch;
            for pattern in patterns.into_iter().rev() {
                res = CommandBranch::Receive(loc.clone(), pattern, Box::new(res));
            }
            Ok(res)
        }

        Rule::cmd_branch_continue => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(CommandBranch::Continue(loc, process))
        }

        Rule::cmd_branch_recv_type => {
            let mut pairs = pair.into_inner();
            let names = parse_name_list(&mut pairs)?;
            let branch = parse_command_branch(&mut pairs)?;
            let mut res = branch;
            for name in names.into_iter().rev() {
                res = CommandBranch::ReceiveType(loc.clone(), name, Box::new(res));
            }
            Ok(res)
        }

        _ => unreachable!(),
    }
}

fn parse_loop_label(pairs: &mut Pairs<Rule>) -> Result<Option<Name>, ParseError> {
    match pairs.next().unwrap().into_inner().next() {
        Some(pair) => Ok(Some(parse_name(&mut Pairs::single(pair))?.1)),
        None => Ok(None),
    }
}
