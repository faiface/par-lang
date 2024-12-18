use std::{cell::RefCell, cmp::Ordering, sync::Arc};

use pest::{
    error::LineColLocation,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use crate::base::{Capture, CaseMode, Command, Context, Expression, Process};

#[derive(Clone, Debug)]
pub struct Name {
    pub string: String,
    pub location: Location,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub message: String,

    #[allow(unused)]
    pub location: Location,
}

#[allow(unused)]
#[derive(Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Parser)]
#[grammar = "par.pest"]
pub struct Par;

pub fn parse_program<X: std::fmt::Display>(
    source: &str,
) -> Result<
    (
        Context<Arc<Name>, X>,
        Vec<(Arc<Name>, Arc<Expression<Arc<Name>>>)>,
    ),
    ParseError,
> {
    let mut context = Context::new();
    let mut definitions = Vec::new();
    for pair in Par::parse(Rule::program, source)?
        .next()
        .unwrap()
        .into_inner()
    {
        if pair.as_rule() == Rule::EOI {
            continue;
        }
        let mut pairs = pair.into_inner();
        let name = parse_name(&mut pairs)?;
        let expr = parse_expression(&mut pairs)?;
        definitions.push((name.clone(), expr.clone()));
        if let Some(_) = context.statics.insert(name.clone(), expr) {
            return Err(ParseError {
                message: format!("\"{}\" is already defined", name.string),
                location: name.location.clone(),
            });
        }
    }
    context.fix_captures();
    Ok((context, definitions))
}

fn parse_name(pairs: &mut Pairs<'_, Rule>) -> Result<Arc<Name>, ParseError> {
    let pair = pairs.next().unwrap();
    Ok(Arc::new(pair.into()))
}

fn parse_expression(pairs: &mut Pairs<'_, Rule>) -> Result<Arc<Expression<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::string => {
            let slice = pair.as_str();
            let literal = Arc::from(&slice[1..slice.len() - 1]);
            Ok(Arc::new(Expression::String(literal)))
        }

        Rule::expr_break => {
            let construct = Arc::new(Name {
                string: "_cons".to_string(),
                location: pair.clone().into(),
            });
            Ok(Arc::new(Expression::Fork(
                RefCell::default(),
                construct.clone(),
                Arc::new(Process::Do(construct, Command::Break)),
            )))
        }

        Rule::expr_fork => {
            let mut pairs = pair.into_inner();
            let object = parse_name(&mut pairs)?;
            let process = parse_process(&mut pairs, None)?;
            Ok(Arc::new(Expression::Fork(
                RefCell::new(Capture::default()),
                object,
                process,
            )))
        }

        Rule::expr_case => {
            let pair = pair.into_inner().next().unwrap();
            assert_eq!(pair.as_rule(), Rule::expr_branches);

            let construct = Arc::new(Name {
                string: "_cons".to_string(),
                location: pair.clone().into(),
            });

            let mut branches = Vec::new();
            for mut pairs in pair.into_inner().map(Pair::into_inner) {
                let branch = parse_name(&mut pairs)?;
                let expression = parse_expression(&mut pairs)?;
                branches.push((
                    branch,
                    Arc::new(Process::Link(construct.clone(), expression)),
                ))
            }

            Ok(Arc::new(Expression::Fork(
                RefCell::default(),
                construct.clone(),
                Arc::new(Process::Do(
                    construct,
                    Command::Case(CaseMode::Single, branches),
                )),
            )))
        }

        Rule::reference => {
            let mut pairs = pair.into_inner();
            let name = Arc::<Name>::new(pairs.next().unwrap().into());

            let actions = pairs.next().unwrap().into_inner().next();

            let result = Arc::new(Name {
                string: "_result".to_string(),
                location: name.as_ref().location.clone(),
            });
            let object = Arc::new(Name {
                string: "_object".to_string(),
                location: name.as_ref().location.clone(),
            });

            let after = match pairs.next() {
                Some(pair) if pair.as_rule() == Rule::apply_close => {
                    let mut pairs = pair.into_inner();
                    let expression = parse_expression(&mut pairs)?;
                    Arc::new(Process::Do(
                        object.clone(),
                        Command::Continue(Arc::new(Process::Link(result.clone(), expression))),
                    ))
                }

                Some(pair) if pair.as_rule() == Rule::apply_case => {
                    let mut branches = Vec::new();
                    for mut pairs in pair
                        .into_inner()
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(Pair::into_inner)
                    {
                        let mut head = pairs.next().unwrap().into_inner();
                        let expression = parse_expression(&mut pairs)?;
                        let branch = parse_name(&mut head)?;

                        let branch_actions = head.next().unwrap();
                        assert_eq!(branch_actions.as_rule(), Rule::apply_branch_actions);

                        let closing = head.next().unwrap();
                        let mut process = match closing.as_rule() {
                            Rule::apply_branch_close => Arc::new(Process::Do(
                                object.clone(),
                                Command::Continue(Arc::new(Process::Link(
                                    result.clone(),
                                    expression,
                                ))),
                            )),
                            Rule::apply_branch_bind => {
                                let bound = parse_name(&mut closing.into_inner())?;
                                Arc::new(Process::Let(
                                    bound,
                                    Arc::new(Expression::Ref(object.clone())),
                                    Arc::new(Process::Link(result.clone(), expression)),
                                ))
                            }
                            _ => unreachable!(),
                        };

                        process = match branch_actions.into_inner().next() {
                            Some(actions) => {
                                parse_actions(&mut actions.into_inner(), object.clone(), process)?
                            }
                            None => process,
                        };
                        branches.push((branch, process));
                    }

                    Arc::new(Process::Do(
                        object.clone(),
                        Command::Case(CaseMode::Single, branches),
                    ))
                }

                Some(_) => unreachable!(),

                None if actions.is_none() => return Ok(Arc::new(Expression::Ref(name))),

                None => Arc::new(Process::Link(
                    result.clone(),
                    Arc::new(Expression::Ref(object.clone())),
                )),
            };

            Ok(Arc::new(Expression::Fork(
                RefCell::default(),
                result.clone(),
                Arc::new(Process::Let(
                    object.clone(),
                    Arc::new(Expression::Ref(name)),
                    match actions {
                        Some(pair) => {
                            assert_eq!(pair.as_rule(), Rule::actions);
                            parse_actions(&mut pair.into_inner(), object.clone(), after)?
                        }
                        None => after,
                    },
                )),
            )))
        }

        Rule::construction => {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::actions);

            let expression = parse_expression(&mut pairs)?;

            let construct = Arc::new(Name {
                string: "_cons".to_string(),
                location: pair.clone().into(),
            });
            let after = Arc::new(Process::Link(construct.clone(), expression));

            Ok(Arc::new(Expression::Fork(
                RefCell::default(),
                construct.clone(),
                parse_actions(&mut pair.into_inner(), construct, after)?,
            )))
        }

        _ => unreachable!(),
    }
}

fn parse_actions(
    pairs: &mut Pairs<'_, Rule>,
    object: Arc<Name>,
    after: Arc<Process<Arc<Name>>>,
) -> Result<Arc<Process<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap();
    assert_eq!(pair.as_rule(), Rule::action);
    let action = pair.into_inner().next().unwrap();

    let then = match pairs.next() {
        Some(pair) => {
            assert_eq!(pair.as_rule(), Rule::actions);
            parse_actions(&mut pair.into_inner(), object.clone(), after)?
        }
        None => after,
    };

    let rule = action.as_rule();
    let mut pairs = action.into_inner();

    match rule {
        Rule::send => {
            let argument = parse_expression(&mut pairs)?;
            Ok(Arc::new(Process::Do(object, Command::Send(argument, then))))
        }
        Rule::receive => {
            let parameter = parse_name(&mut pairs)?;
            Ok(Arc::new(Process::Do(
                object,
                Command::Receive(parameter, then),
            )))
        }
        Rule::select => {
            let branch = parse_name(&mut pairs)?;
            Ok(Arc::new(Process::Do(object, Command::Select(branch, then))))
        }
        _ => unreachable!(),
    }
}

fn parse_process(
    pairs: &mut Pairs<'_, Rule>,
    pass: Option<Arc<Process<Arc<Name>>>>,
) -> Result<Arc<Process<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let rule = pair.as_rule();
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    match rule {
        Rule::proc_let => {
            let name = parse_name(&mut pairs)?;
            let expr = parse_expression(&mut pairs)?;
            let proc = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Let(name, expr, proc)))
        }

        Rule::proc_pass => match pass {
            Some(process) => Ok(process),
            None => Err(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![Rule::process],
                    negatives: vec![],
                },
                span,
            ))?,
        },

        Rule::command => {
            let subject = parse_name(&mut pairs)?;
            parse_proc_apply(subject, &mut pairs, pass)
        }

        _ => unreachable!(),
    }
}

fn parse_proc_apply(
    subject: Arc<Name>,
    pairs: &mut Pairs<'_, Rule>,
    pass: Option<Arc<Process<Arc<Name>>>>,
) -> Result<Arc<Process<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();

    match rule {
        Rule::proc_link => {
            let argument = parse_expression(&mut pairs)?;
            Ok(Arc::new(Process::Link(subject, argument)))
        }

        Rule::proc_break => Ok(Arc::new(Process::Do(subject, Command::Break))),

        Rule::proc_loop => Ok(Arc::new(Process::Do(subject, Command::Loop))),

        Rule::proc_close => {
            let then = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Do(subject, Command::Continue(then))))
        }

        Rule::proc_case | Rule::proc_iterate => {
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::proc_branches);

            let mut pass = pass;
            if let Some(pair) = pairs.next() {
                pass = Some(parse_process(&mut Pairs::single(pair), pass)?);
            }

            let mut branches = Vec::new();
            for mut pairs in pair.into_inner().map(Pair::into_inner) {
                let mut head = pairs.next().unwrap().into_inner();
                let mut process = parse_process(&mut pairs, pass.clone())?;
                let branch = parse_name(&mut head)?;

                let branch_actions = head.next().unwrap();
                assert_eq!(branch_actions.as_rule(), Rule::proc_branch_actions);
                if let Some(close) = head.next() {
                    assert_eq!(close.as_rule(), Rule::proc_branch_close);
                    process = Arc::new(Process::Do(subject.clone(), Command::Continue(process)));
                }
                process = match branch_actions.into_inner().next() {
                    Some(actions) => {
                        parse_actions(&mut actions.into_inner(), subject.clone(), process)?
                    }
                    None => process,
                };

                branches.push((branch, process));
            }

            Ok(Arc::new(Process::Do(
                subject,
                Command::Case(
                    if rule == Rule::proc_iterate {
                        CaseMode::Iterate
                    } else {
                        CaseMode::Single
                    },
                    branches,
                ),
            )))
        }

        Rule::proc_action => {
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::action);
            let action = pair.into_inner().next().unwrap();

            let pair = pairs.next().unwrap();
            let then = match pair.as_rule() {
                Rule::proc_apply => {
                    parse_proc_apply(subject.clone(), &mut Pairs::single(pair), pass)?
                }
                Rule::process => parse_process(&mut Pairs::single(pair), pass)?,
                _ => unreachable!(),
            };

            let rule = action.as_rule();
            let mut pairs = action.into_inner();
            match rule {
                Rule::send => {
                    let argument = parse_expression(&mut pairs)?;
                    Ok(Arc::new(Process::Do(
                        subject,
                        Command::Send(argument, then),
                    )))
                }
                Rule::receive => {
                    let parameter = parse_name(&mut pairs)?;
                    Ok(Arc::new(Process::Do(
                        subject,
                        Command::Receive(parameter, then),
                    )))
                }
                Rule::select => {
                    let branch = parse_name(&mut pairs)?;
                    Ok(Arc::new(Process::Do(
                        subject,
                        Command::Select(branch, then),
                    )))
                }
                _ => unreachable!(),
            }
        }

        _ => unreachable!(),
    }
}

impl From<Pair<'_, Rule>> for Location {
    fn from(value: Pair<'_, Rule>) -> Self {
        let (line, column) = value.line_col();
        Self { line, column }
    }
}

impl From<Pair<'_, Rule>> for Name {
    fn from(value: Pair<'_, Rule>) -> Self {
        Self {
            string: value.as_str().to_string(),
            location: value.into(),
        }
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Self {
            message: value.to_string(),
            location: match value.line_col {
                LineColLocation::Pos((line, column)) => Location { line, column },
                LineColLocation::Span((line, column), _) => Location { line, column },
            },
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Name {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string.cmp(&other.string)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Name {}
