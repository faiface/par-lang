use arcstr::literal;
use num_bigint::BigInt;
use std::{cmp::Ordering, sync::Arc};

use crate::{
    icombs::readback::Handle,
    par::{
        primitive::ParString,
        process,
        program::{Definition, Module, TypeDef},
        types::Type,
    },
};

pub fn external_module() -> Module<Arc<process::Expression<()>>> {
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
                "Sub",
                Type::function(Type::nat(), Type::function(Type::int(), Type::nat())),
                |handle| Box::pin(nat_sub(handle)),
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
                        Type::either(vec![("end", Type::break_()), ("step", Type::self_(None))]),
                    ),
                ),
                |handle| Box::pin(nat_repeat(handle)),
            ),
            Definition::external(
                "RepeatLazy",
                Type::function(
                    Type::nat(),
                    Type::recursive(
                        None,
                        Type::either(vec![
                            ("end", Type::break_()),
                            (
                                "step",
                                Type::box_(Type::choice(vec![("next", Type::self_(None))])),
                            ),
                        ]),
                    ),
                ),
                |handle| Box::pin(nat_repeat_lazy(handle)),
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
            Definition::external(
                "FromString",
                Type::function(
                    Type::string(),
                    Type::either(vec![("ok", Type::nat()), ("err", Type::break_())]),
                ),
                |handle| Box::pin(nat_from_string(handle)),
            ),
        ],
    }
}

async fn nat_add(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x + y);
}

async fn nat_sub(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().int().await;
    handle.provide_nat((x - y).max(0.into()));
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

async fn nat_repeat_lazy(mut handle: Handle) {
    let n = handle.receive().nat().await;
    nat_repeat_lazy_inner(handle, n.clone());
}

fn nat_repeat_lazy_inner(mut handle: Handle, n: BigInt) {
    if n > BigInt::ZERO {
        handle.signal(literal!("step"));
        handle.provide_box(move |mut handle| {
            let mut n = n.clone();
            n -= 1;
            async move {
                let n = n.clone();
                match handle.case().await.as_str() {
                    "next" => nat_repeat_lazy_inner(handle, n.clone()),
                    _ => unreachable!(),
                }
            }
        });
    } else {
        handle.signal(literal!("end"));
        handle.break_();
    }
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
    handle.provide_string(ParString::from(x.to_str_radix(10)))
}

async fn nat_from_string(mut handle: Handle) {
    let string = handle.receive().string().await;
    match string.as_str().parse::<BigInt>() {
        Ok(num) => {
            if num >= BigInt::ZERO {
                handle.signal(literal!("ok"));
                handle.provide_nat(num);
            } else {
                handle.signal(literal!("err"));
                handle.break_();
            }
        }
        Err(_) => {
            handle.signal(literal!("err"));
            handle.break_();
        }
    };
}
