use arcstr::literal;
use num_bigint::BigInt;
use par_core::{
    frontend::{process, Definition, Module, ParString, Type, TypeDef},
    runtime::Handle,
};
use std::{cmp::Ordering, sync::Arc};

pub(crate) fn external_module() -> Module<Arc<process::Expression<()>>> {
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
            Definition::external("Abs", Type::function(Type::int(), Type::nat()), |handle| {
                Box::pin(int_abs(handle))
            }),
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
            Definition::external(
                "FromString",
                Type::function(
                    Type::string(),
                    Type::either(vec![("ok", Type::int()), ("err", Type::break_())]),
                ),
                |handle| Box::pin(int_from_string(handle)),
            ),
        ],
    }
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

async fn int_abs(mut handle: Handle) {
    let int = handle.receive().int().await;
    if int < BigInt::ZERO {
        handle.provide_nat(-int);
    } else {
        handle.provide_nat(int);
    }
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
    handle.provide_string(ParString::from(x.to_str_radix(10)))
}

async fn int_from_string(mut handle: Handle) {
    let string = handle.receive().string().await;
    match string.as_str().parse::<BigInt>() {
        Ok(num) => {
            handle.signal(literal!("ok"));
            handle.provide_int(num);
        }
        Err(_) => {
            handle.signal(literal!("err"));
            handle.break_();
        }
    };
}
