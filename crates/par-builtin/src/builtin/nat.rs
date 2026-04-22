//package: core
use arcstr::literal;
use num_bigint::BigInt;
use par_runtime::data::Data;
use std::cmp::Ordering;

use par_core::frontend::{ExternalTypeDef, PrimitiveType, Type};
use par_core::source::Span;
use par_runtime::primitive::ParString;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalTypeDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Nat"
    },
    typ: Type::Primitive(Span::None, PrimitiveType::Nat)
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Add"
    },
    f: |handle| Box::pin(nat_add(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Sub"
    },
    f: |handle| Box::pin(nat_sub(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Mul"
    },
    f: |handle| Box::pin(nat_mul(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Div"
    },
    f: |handle| Box::pin(nat_div(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Mod"
    },
    f: |handle| Box::pin(nat_mod(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Min"
    },
    f: |handle| Box::pin(nat_min(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Max"
    },
    f: |handle| Box::pin(nat_max(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Clamp"
    },
    f: |handle| Box::pin(nat_clamp(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Equals"
    },
    f: |handle| Box::pin(nat_equals(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Compare"
    },
    f: |handle| Box::pin(nat_compare(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Repeat"
    },
    f: |handle| Box::pin(nat_repeat(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "RepeatLazy"
    },
    f: |handle| Box::pin(nat_repeat_lazy(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "Range"
    },
    f: |handle| Box::pin(nat_range(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "ToString"
    },
    f: |handle| Box::pin(nat_to_string(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Nat",
        name: "FromString"
    },
    f: |handle| Box::pin(nat_from_string(handle)),
});

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
        Ordering::Less => {
            handle.provide_data(&Data::Either(literal!("less"), Box::new(Data::Unit)))
        }
        Ordering::Equal => {
            handle.provide_data(&Data::Either(literal!("equal"), Box::new(Data::Unit)))
        }
        Ordering::Greater => {
            handle.provide_data(&Data::Either(literal!("greater"), Box::new(Data::Unit)))
        }
    };
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
    handle.provide_string(ParString::from(x.to_str_radix(10)));
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
