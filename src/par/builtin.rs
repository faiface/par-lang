use std::{cmp::Ordering, fs::Metadata, future::Future, path::PathBuf, sync::Arc};

use arcstr::{literal, Substr};
use num_bigint::BigInt;
use tokio::fs::{self, File, ReadDir};

use crate::icombs::readback::Handle;

use super::{
    process,
    program::{Definition, Module, TypeDef},
    types::Type,
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        "Bool",
        Module::parse_and_compile(include_str!("./builtin/Bool.par")).unwrap(),
    );
    module.import(
        "List",
        Module::parse_and_compile(include_str!("./builtin/List.par")).unwrap(),
    );
    module.import(
        "Ordering",
        Module::parse_and_compile(include_str!("./builtin/Ordering.par")).unwrap(),
    );
    module.import(
        "Char",
        Module::parse_and_compile(include_str!("./builtin/Char.par")).unwrap(),
    );
    module.import(
        "String",
        Module::parse_and_compile(include_str!("./builtin/String.par")).unwrap(),
    );
    module.import(
        "Console",
        Module::parse_and_compile(include_str!("./builtin/Console.par")).unwrap(),
    );
    module.import(
        "Storage",
        Module::parse_and_compile(include_str!("./builtin/Storage.par")).unwrap(),
    );

    module.import(
        "Nat",
        Module {
            type_defs: vec![TypeDef::external("Nat", &[], Type::nat())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Add",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_add(handle)),
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_mul(handle)),
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_div(handle)),
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_mod(handle)),
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_min(handle)),
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::nat(), Type::function(Type::int(), Type::nat())),
                    |handle| Box::pin(nat_max(handle)),
                ),
                Definition::external(
                    "Clamp",
                    Type::function(
                        Type::int(),
                        Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    ),
                    |handle| Box::pin(nat_clamp(handle)),
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::nat(),
                        Type::function(Type::nat(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |handle| Box::pin(nat_equals(handle)),
                ),
                Definition::external(
                    "Compare",
                    Type::function(
                        Type::nat(),
                        Type::function(
                            Type::nat(),
                            Type::name(Some("Ordering"), "Ordering", vec![]),
                        ),
                    ),
                    |handle| Box::pin(nat_compare(handle)),
                ),
                Definition::external(
                    "Repeat",
                    Type::function(
                        Type::nat(),
                        Type::recursive(
                            None,
                            Type::either(vec![
                                ("end", Type::break_()),
                                ("step", Type::self_(None)),
                            ]),
                        ),
                    ),
                    |handle| Box::pin(nat_repeat(handle)),
                ),
                Definition::external(
                    "Range",
                    Type::function(
                        Type::nat(),
                        Type::function(
                            Type::nat(),
                            Type::name(Some("List"), "List", vec![Type::nat()]),
                        ),
                    ),
                    |handle| Box::pin(nat_range(handle)),
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::nat(), Type::string()),
                    |handle| Box::pin(nat_to_string(handle)),
                ),
            ],
        },
    );

    module.import(
        "Int",
        Module {
            type_defs: vec![TypeDef::external("Int", &[], Type::int())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Add",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_add(handle)),
                ),
                Definition::external(
                    "Sub",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_sub(handle)),
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_mul(handle)),
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_div(handle)),
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::int(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(int_mod(handle)),
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_min(handle)),
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_max(handle)),
                ),
                Definition::external(
                    "Clamp",
                    Type::function(
                        Type::int(),
                        Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    ),
                    |handle| Box::pin(int_clamp(handle)),
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::int(),
                        Type::function(Type::int(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |handle| Box::pin(int_equals(handle)),
                ),
                Definition::external(
                    "Compare",
                    Type::function(
                        Type::int(),
                        Type::function(
                            Type::int(),
                            Type::name(Some("Ordering"), "Ordering", vec![]),
                        ),
                    ),
                    |handle| Box::pin(int_compare(handle)),
                ),
                Definition::external(
                    "Range",
                    Type::function(
                        Type::int(),
                        Type::function(
                            Type::int(),
                            Type::name(Some("List"), "List", vec![Type::int()]),
                        ),
                    ),
                    |handle| Box::pin(int_range(handle)),
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::int(), Type::string()),
                    |handle| Box::pin(int_to_string(handle)),
                ),
            ],
        },
    );

    module.import(
        "Char",
        Module {
            type_defs: vec![TypeDef::external("Char", &[], Type::char())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::char(),
                        Type::function(Type::char(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |handle| Box::pin(char_equals(handle)),
                ),
                Definition::external(
                    "Code",
                    Type::function(Type::char(), Type::nat()),
                    |handle| Box::pin(char_code(handle)),
                ),
                Definition::external(
                    "Is",
                    Type::function(
                        Type::char(),
                        Type::function(
                            Type::name(None, "Class", vec![]),
                            Type::name(Some("Bool"), "Bool", vec![]),
                        ),
                    ),
                    |handle| Box::pin(char_is(handle)),
                ),
            ],
        },
    );

    module.import(
        "String",
        Module {
            type_defs: vec![TypeDef::external("String", &[], Type::string())],
            declarations: vec![],
            definitions: vec![
                Definition::external("Builder", Type::name(None, "Builder", vec![]), |handle| {
                    Box::pin(string_builder(handle))
                }),
                Definition::external(
                    "Quote",
                    Type::function(Type::string(), Type::string()),
                    |handle| Box::pin(string_quote(handle)),
                ),
                Definition::external(
                    "Reader",
                    Type::function(
                        Type::string(),
                        Type::name(None, "Reader", vec![Type::break_()]),
                    ),
                    |handle| Box::pin(string_reader(handle)),
                ),
            ],
        },
    );

    module.import(
        "Debug",
        Module {
            type_defs: vec![],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Log",
                Type::function(Type::string(), Type::break_()),
                |handle| Box::pin(debug_log(handle)),
            )],
        },
    );

    module.import(
        "Console",
        Module {
            type_defs: vec![],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Open",
                Type::name(None, "Console", vec![]),
                |handle| Box::pin(console_open(handle)),
            )],
        },
    );

    module.import(
        "Storage",
        Module {
            type_defs: vec![],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Open",
                Type::function(
                    Type::name(None, "Path", vec![]),
                    Type::name(None, "OpenResult", vec![]),
                ),
                |handle| Box::pin(storage_open(handle)),
            )],
        },
    );
}

async fn nat_add(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x + y);
}

async fn nat_mul(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x * y);
}

async fn nat_div(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x / y
    });
}

async fn nat_mod(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x % y
    });
}

async fn nat_min(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x.min(y));
}

async fn nat_max(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().int().await;
    handle.provide_nat(x.max(y));
}

async fn nat_clamp(mut handle: Handle) {
    let int = handle.receive().int().await;
    let min = handle.receive().nat().await;
    let max = handle.receive().nat().await;
    handle.provide_nat(int.min(max).max(min));
}

async fn nat_equals(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

async fn nat_compare(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    match x.cmp(&y) {
        Ordering::Less => handle.signal(literal!("less")),
        Ordering::Equal => handle.signal(literal!("equal")),
        Ordering::Greater => handle.signal(literal!("greater")),
    }
    handle.break_();
}

async fn nat_repeat(mut handle: Handle) {
    let mut n = handle.receive().nat().await;
    while n > BigInt::ZERO {
        handle.signal(literal!("step"));
        n -= 1;
    }
    handle.signal(literal!("end"));
    handle.break_();
}

async fn nat_range(mut handle: Handle) {
    let lo = handle.receive().nat().await;
    let hi = handle.receive().nat().await;

    let mut i = lo;
    while i < hi {
        handle.signal(literal!("item"));
        handle.send().provide_nat(i.clone());
        i += 1;
    }
    handle.signal(literal!("end"));
    handle.break_();
}

async fn nat_to_string(mut handle: Handle) {
    let x = handle.receive().nat().await;
    handle.provide_string(Substr::from(x.to_str_radix(10)))
}

async fn int_add(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x + y);
}

async fn int_sub(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x - y);
}

async fn int_mul(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x * y);
}

async fn int_div(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x / y
    });
}

async fn int_mod(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().nat().await;
    if y == BigInt::ZERO {
        handle.provide_nat(BigInt::ZERO);
    } else if x < BigInt::ZERO {
        let rem = x % y.clone();
        handle.provide_nat(if rem == BigInt::ZERO {
            BigInt::ZERO
        } else {
            y.clone() + rem
        });
    } else {
        handle.provide_nat(x % y);
    }
}

async fn int_min(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x.min(y));
}

async fn int_max(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x.max(y));
}

async fn int_clamp(mut handle: Handle) {
    let int = handle.receive().int().await;
    let min = handle.receive().int().await;
    let max = handle.receive().int().await;
    handle.provide_int(int.min(max).max(min));
}

async fn int_equals(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

async fn int_compare(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    match x.cmp(&y) {
        Ordering::Equal => handle.signal(literal!("equal")),
        Ordering::Greater => handle.signal(literal!("greater")),
        Ordering::Less => handle.signal(literal!("less")),
    }
    handle.break_();
}

async fn int_range(mut handle: Handle) {
    let lo = handle.receive().int().await;
    let hi = handle.receive().int().await;

    let mut i = lo;
    while i < hi {
        handle.signal(literal!("item"));
        handle.send().provide_int(i.clone());
        i += 1;
    }
    handle.signal(literal!("end"));
    handle.break_();
}

async fn int_to_string(mut handle: Handle) {
    let x = handle.receive().int().await;
    handle.provide_string(Substr::from(x.to_str_radix(10)))
}

async fn char_equals(mut handle: Handle) {
    let x = handle.receive().char().await;
    let y = handle.receive().char().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

async fn char_code(mut handle: Handle) {
    let c = handle.receive().char().await;
    handle.provide_nat(BigInt::from(c as u32))
}

async fn char_is(mut handle: Handle) {
    let ch = handle.receive().char().await;
    let class = CharClass::readback(handle.receive()).await;
    if class.contains(ch) {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

#[derive(Debug, Clone)]
enum CharClass {
    Any,
    Char(char),
    Whitespace,
    AsciiAny,
    AsciiAlpha,
    AsciiAlphanum,
    AsciiDigit,
}

impl CharClass {
    async fn readback(mut handle: Handle) -> Self {
        match handle.case().await.as_str() {
            "any" => Self::Any,
            "ascii" => match handle.case().await.as_str() {
                "alpha" => Self::AsciiAlpha,
                "alphanum" => Self::AsciiAlphanum,
                "any" => Self::AsciiAny,
                "digit" => Self::AsciiDigit,
                _ => unreachable!(),
            },
            "char" => Self::Char(handle.char().await),
            "whitespace" => Self::Whitespace,
            _ => unreachable!(),
        }
    }

    fn contains(&self, ch: char) -> bool {
        match self {
            Self::Any => true,
            Self::Char(ch1) => ch == *ch1,
            Self::Whitespace => ch.is_whitespace(),
            Self::AsciiAny => ch.is_ascii(),
            Self::AsciiAlpha => ch.is_ascii_alphabetic(),
            Self::AsciiAlphanum => ch.is_ascii_alphanumeric(),
            Self::AsciiDigit => ch.is_ascii_digit(),
        }
    }
}

async fn string_builder(mut handle: Handle) {
    let mut buf = String::new();
    loop {
        match handle.case().await.as_str() {
            "add" => {
                buf += &handle.receive().string().await;
            }
            "build" => {
                handle.provide_string(Substr::from(buf));
                break;
            }
            _ => unreachable!(),
        }
    }
}

async fn string_quote(mut handle: Handle) {
    let s = handle.receive().string().await;
    handle.provide_string(Substr::from(format!("{:?}", s)));
}

async fn string_reader(mut handle: Handle) {
    let mut remainder = handle.receive().string().await;

    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
            }
            "match" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                if remainder.is_empty() {
                    handle.signal(literal!("end"));
                    handle.break_();
                    return;
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                let mut best_match = None;
                for (pos, ch) in remainder.char_indices() {
                    match (m.leftmost_feasible_split(pos), best_match) {
                        (Some(fi), Some((bi, _))) if fi > bi => break,
                        (None, _) => break,
                        _ => {}
                    }
                    m.advance(pos, ch);
                    match (m.leftmost_accepting_split(), best_match) {
                        (Some(ai), Some((bi, _))) if ai <= bi => {
                            best_match = Some((ai, pos + ch.len_utf8()))
                        }
                        (Some(ai), None) => best_match = Some((ai, pos + ch.len_utf8())),
                        _ => {}
                    }
                }

                match best_match {
                    Some((i, j)) => {
                        handle.signal(literal!("match"));
                        handle.send().provide_string(remainder.substr(..i));
                        handle.send().provide_string(remainder.substr(i..j));
                        remainder = remainder.substr(j..);
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "matchEnd" => {
                let prefix = Pattern::readback(handle.receive()).await;
                let suffix = Pattern::readback(handle.receive()).await;
                if remainder.is_empty() {
                    handle.signal(literal!("end"));
                    handle.break_();
                    return;
                }

                let mut m = Machine::start(Box::new(Pattern::Concat(prefix, suffix)));

                for (pos, ch) in remainder.char_indices() {
                    if m.accepts() == None {
                        break;
                    }
                    m.advance(pos, ch);
                }

                match m.leftmost_accepting_split() {
                    Some(i) => {
                        handle.signal(literal!("match"));
                        handle.send().provide_string(remainder.substr(..i));
                        handle.send().provide_string(remainder.substr(i..));
                        handle.break_();
                        return;
                    }
                    None => {
                        handle.signal(literal!("fail"));
                    }
                }
            }
            "remainder" => {
                handle.provide_string(remainder);
                return;
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
enum Pattern {
    Nil,
    All,
    Empty,
    Length(BigInt),
    Str(Substr),
    One(CharClass),
    Non(CharClass),
    Concat(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Repeat(Box<Self>),
    Repeat1(Box<Self>),
}

impl Pattern {
    async fn readback(mut handle: Handle) -> Box<Self> {
        match handle.case().await.as_str() {
            "and" => {
                // .and List<self>
                let mut conj = Box::new(Self::All);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    conj = Box::new(Self::And(p, conj));
                }
                conj
            }
            "concat" => {
                // .concat List<self>
                let mut conc = Box::new(Self::Empty);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    conc = Box::new(Self::Concat(p, conc));
                }
                conc
            }
            "empty" => {
                // .empty!
                handle.break_();
                Box::new(Self::Empty)
            }
            "length" => {
                // .length Nat
                let n = handle.nat().await;
                Box::new(Self::Length(n))
            }
            "non" => {
                // .non Char.Class
                let class = CharClass::readback(handle).await;
                Box::new(Self::Non(class))
            }
            "one" => {
                // .one Char.Class
                let class = CharClass::readback(handle).await;
                Box::new(Self::One(class))
            }
            "or" => {
                // .or List<self>,
                let mut disj = Box::new(Self::Nil);
                let patterns =
                    readback_list(handle, |handle| Box::pin(Self::readback(handle))).await;
                for p in patterns.into_iter().rev() {
                    disj = Box::new(Self::Or(p, disj));
                }
                disj
            }
            "repeat" => {
                // .repeat self
                let p = Box::pin(Self::readback(handle)).await;
                Box::new(Self::Repeat(p))
            }
            "repeat1" => {
                // .repeat1 self
                let p = Box::pin(Self::readback(handle)).await;
                Box::new(Self::Repeat1(p))
            }
            "str" => {
                // .str String
                let s = handle.string().await;
                Box::new(Self::Str(s))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Machine {
    pattern: Box<Pattern>,
    inner: MachineInner,
}

impl Machine {
    fn start(pattern: Box<Pattern>) -> Self {
        let inner = MachineInner::start(&pattern, 0);
        Self { pattern, inner }
    }

    fn accepts(&self) -> Option<bool> {
        self.inner.accepts(&self.pattern)
    }

    fn advance(&mut self, pos: usize, ch: char) {
        self.inner.advance(&self.pattern, pos, ch);
    }

    fn leftmost_accepting_split(&self) -> Option<usize> {
        let Pattern::Concat(_, p2) = self.pattern.as_ref() else {
            return None;
        };
        let State::Concat(_, heap) = &self.inner.state else {
            return None;
        };
        heap.iter()
            .filter(|m2| m2.accepts(p2) == Some(true))
            .map(|m2| m2.start)
            .min()
    }

    fn leftmost_feasible_split(&self, pos: usize) -> Option<usize> {
        let State::Concat(_, heap) = &self.inner.state else {
            return None;
        };
        heap.iter().map(|m2| m2.start).min().or(Some(pos))
    }
}

#[derive(Debug)]
struct MachineInner {
    state: State,
    start: usize,
}

impl MachineInner {
    fn start(pattern: &Pattern, start: usize) -> Self {
        let state = match pattern {
            Pattern::Nil => State::Halt,

            Pattern::All => State::Init,

            Pattern::Empty => State::Init,

            Pattern::Length(_) => State::Index(0),

            Pattern::Str(_) => State::Index(0),

            Pattern::One(_) => State::Index(0),
            Pattern::Non(_) => State::Index(0),

            Pattern::Concat(p1, p2) => {
                let prefix = Self::start(p1, start);
                let suffixes = if prefix.accepts(p1) == Some(true) {
                    vec![Self::start(p2, start)]
                } else {
                    vec![]
                };
                State::Concat(Box::new(prefix), suffixes)
            }

            Pattern::And(p1, p2) | Pattern::Or(p1, p2) => State::Pair(
                Box::new(Self::start(p1, start)),
                Box::new(Self::start(p2, start)),
            ),

            Pattern::Repeat(_) => State::Init,
            Pattern::Repeat1(p) => State::Heap(vec![Self::start(p, start)]),
        };

        Self { state, start }
    }

    fn accepts(&self, pattern: &Pattern) -> Option<bool> {
        match (pattern, &self.state) {
            (_, State::Halt) => None,

            (Pattern::All, State::Init) => Some(true),

            (Pattern::Empty, State::Init) => Some(true),

            (Pattern::Length(n), State::Index(i)) => Some(n == &BigInt::from(*i)),

            (Pattern::Str(s), State::Index(i)) => Some(s.len() == *i),

            (Pattern::One(_), State::Index(i)) => Some(*i == 1),
            (Pattern::Non(_), State::Index(i)) => Some(*i == 1),

            (Pattern::Concat(p1, p2), State::Concat(m1, heap)) => heap
                .iter()
                .filter_map(|m2| m2.accepts(p2))
                .max()
                .or_else(|| m1.accepts(p1).map(|_| false)),

            (Pattern::And(p1, p2), State::Pair(m1, m2)) => match (m1.accepts(p1), m2.accepts(p2)) {
                (Some(a1), Some(a2)) => Some(a1 && a2),
                (None, _) | (_, None) => None,
            },

            (Pattern::Or(p1, p2), State::Pair(m1, m2)) => match (m1.accepts(p1), m2.accepts(p2)) {
                (Some(a1), Some(a2)) => Some(a1 || a2),
                (None, a) | (a, None) => a,
            },

            (Pattern::Repeat(_), State::Init) => Some(true),
            (Pattern::Repeat(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (Pattern::Repeat1(p), State::Heap(heap)) => {
                heap.iter().filter_map(|m| m.accepts(p)).max()
            }

            (p, s) => unreachable!("invalid combination of pattern {:?} and state {:?}", p, s),
        }
    }

    fn advance(&mut self, pattern: &Pattern, pos: usize, ch: char) {
        match (pattern, &mut self.state) {
            (_, State::Halt) => {}

            (Pattern::All, State::Init) => {}

            (Pattern::Empty, State::Init) => self.state = State::Halt,

            (Pattern::Length(n), State::Index(i)) => {
                if &BigInt::from(*i) < n {
                    *i += 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::Str(s), State::Index(i)) => {
                if s.substr(*i..).chars().next() == Some(ch) {
                    *i += ch.len_utf8();
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::One(class), State::Index(i)) => {
                if *i == 0 && class.contains(ch) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }
            (Pattern::Non(class), State::Index(i)) => {
                if *i == 0 && !class.contains(ch) {
                    *i = 1;
                } else {
                    self.state = State::Halt;
                }
            }

            (Pattern::Concat(p1, p2), State::Concat(m1, heap)) => {
                m1.advance(p1, pos, ch);
                for m2 in heap.iter_mut() {
                    m2.advance(p2, pos, ch);
                }
                heap.retain(|m2| m2.state != State::Halt);
                if m1.accepts(p1) == Some(true) {
                    heap.push(Self::start(p2, pos + ch.len_utf8()));
                }
                heap.sort_by_key(|m| m.start);
                heap.sort();
                heap.dedup();
                if m1.state == State::Halt && heap.is_empty() {
                    self.state = State::Halt;
                }
            }

            (Pattern::And(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, ch);
                m2.advance(p2, pos, ch);
                if m1.state == State::Halt || m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (Pattern::Or(p1, p2), State::Pair(m1, m2)) => {
                m1.advance(p1, pos, ch);
                m2.advance(p2, pos, ch);
                if m1.state == State::Halt && m2.state == State::Halt {
                    self.state = State::Halt;
                }
            }

            (Pattern::Repeat(p), State::Init) => {
                let mut m = Self::start(p, pos);
                m.advance(p, pos, ch);
                self.state = State::Heap(vec![m])
            }
            (Pattern::Repeat(p) | Pattern::Repeat1(p), State::Heap(heap)) => {
                if heap.iter().any(|m| m.accepts(p) == Some(true)) {
                    heap.push(Self::start(p, pos));
                }
                for m in heap.iter_mut() {
                    m.advance(p, pos, ch);
                }
                heap.retain(|m| m.state != State::Halt);
                heap.sort_by_key(|m| m.start);
                heap.sort();
                heap.dedup();
                if heap.is_empty() {
                    self.state = State::Halt;
                }
            }

            (p, s) => unreachable!("invalid combination of pattern {:?} and state {:?}", p, s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum State {
    Init,
    Halt,
    Index(usize),
    Pair(Box<MachineInner>, Box<MachineInner>),
    Heap(Vec<MachineInner>),
    Concat(Box<MachineInner>, Vec<MachineInner>),
}

impl PartialEq for MachineInner {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}
impl Eq for MachineInner {}
impl PartialOrd for MachineInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.state.partial_cmp(&other.state)
    }
}
impl Ord for MachineInner {
    fn cmp(&self, other: &Self) -> Ordering {
        self.state.cmp(&other.state)
    }
}

async fn debug_log(mut handle: Handle) {
    println!("{}", handle.receive().string().await);
    handle.break_();
}

async fn console_open(mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                break;
            }
            "print" => {
                println!("{}", handle.receive().string().await);
            }
            _ => unreachable!(),
        }
    }
}

async fn storage_open(mut handle: Handle) {
    let path = PathBuf::from(handle.receive().string().await.as_str());
    let meta = match fs::metadata(&path).await {
        Ok(meta) => meta,
        Err(error) => {
            handle.signal(literal!("err"));
            return handle.provide_string(Substr::from(error.to_string()));
        }
    };
    handle_open_result(path, meta, handle).await
}

fn handle_open_result(
    path: PathBuf,
    meta: Metadata,
    mut handle: Handle,
) -> impl Send + Future<Output = ()> {
    async move {
        let path = fs::canonicalize(&path).await.unwrap_or(path);

        if meta.is_file() {
            let file = match File::open(&path).await {
                Ok(file) => file,
                Err(error) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(error.to_string()));
                }
            };
            handle.signal(literal!("file"));
            return handle_file_info(
                Substr::from(path.to_string_lossy()),
                BigInt::from(meta.len()),
                file,
                handle,
            )
            .await;
        }

        if meta.is_dir() {
            let dir = match fs::read_dir(&path).await {
                Ok(dir) => dir,
                Err(error) => {
                    handle.signal(literal!("err"));
                    return handle.provide_string(Substr::from(error.to_string()));
                }
            };
            handle.signal(literal!("directory"));
            return handle_directory_info(Substr::from(path.to_string_lossy()), dir, handle).await;
        }

        handle.signal(literal!("err"));
        handle.provide_string(Substr::from("unsupported storage item type"));
    }
}

async fn handle_file_info(path: Substr, size: BigInt, _file: File, mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                return;
            }
            "getPath" => {
                handle.send().provide_string(path.clone());
            }
            "getSize" => {
                handle.send().provide_nat(size.clone());
            }
            "readUTF8" => {
                todo!("implement")
            }
            _ => unreachable!(),
        }
    }
}

async fn handle_directory_info(path: Substr, mut dir: ReadDir, mut handle: Handle) {
    loop {
        match handle.case().await.as_str() {
            "close" => {
                handle.break_();
                return;
            }
            "getPath" => {
                handle.send().provide_string(path.clone());
            }
            "list" => {
                while let Ok(Some(entry)) = dir.next_entry().await {
                    let Ok(meta) = entry.metadata().await else {
                        continue;
                    };

                    handle.signal(literal!("item"));
                    handle.send().concurrently(|mut handle| async move {
                        let path = Substr::from(entry.path().to_string_lossy());
                        handle.send().provide_string(path);
                        match handle.case().await.as_str() {
                            "open" => handle_open_result(entry.path(), meta, handle).await,
                            "skip" => {
                                handle.break_();
                                return;
                            }
                            _ => unreachable!(),
                        }
                    });
                }
                handle.signal(literal!("end"));
                handle.break_();
                return;
            }
            _ => unreachable!(),
        }
    }
}

async fn readback_list<T, F>(
    mut handle: Handle,
    mut readback_item: impl FnMut(Handle) -> F,
) -> Vec<T>
where
    F: Future<Output = T>,
{
    let mut items = Vec::new();
    loop {
        match handle.case().await.as_str() {
            "end" => {
                handle.break_();
                return items;
            }
            "item" => {
                let item = readback_item(handle.receive()).await;
                items.push(item);
            }
            _ => unreachable!(),
        }
    }
}
