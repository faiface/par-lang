use std::{cmp::Ordering, sync::Arc};

use arcstr::literal;
use num_bigint::BigInt;

use crate::{
    icombs::readback::Handle,
    par::{
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
        ],
    }
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
