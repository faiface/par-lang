//package: core
use std::f64::consts;

use arcstr::literal;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use par_core::frontend::{ExternalTypeDef, PrimitiveType, Type};
use par_core::source::Span;
use par_runtime::primitive::{format_float, parse_float_text};
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalTypeDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Float"
    },
    typ: Type::Primitive(Span::None, PrimitiveType::Float)
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "NaN_"
    },
    f: |handle| Box::pin(float_nan(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Inf_"
    },
    f: |handle| Box::pin(float_inf(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "NegInf_"
    },
    f: |handle| Box::pin(float_neg_inf(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Pi_"
    },
    f: |handle| Box::pin(float_pi(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "E_"
    },
    f: |handle| Box::pin(float_e(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "IsNaN"
    },
    f: |handle| Box::pin(float_is_nan(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "IsFinite"
    },
    f: |handle| Box::pin(float_is_finite(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "IsInfinite"
    },
    f: |handle| Box::pin(float_is_infinite(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "FromInt"
    },
    f: |handle| Box::pin(float_from_int(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "ToInt"
    },
    f: |handle| Box::pin(float_to_int(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "ToString"
    },
    f: |handle| Box::pin(float_to_string(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "FromString"
    },
    f: |handle| Box::pin(float_from_string(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Neg"
    },
    f: |handle| Box::pin(float_neg(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Abs"
    },
    f: |handle| Box::pin(float_abs(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Floor"
    },
    f: |handle| Box::pin(float_floor(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Ceil"
    },
    f: |handle| Box::pin(float_ceil(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Round"
    },
    f: |handle| Box::pin(float_round(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Add"
    },
    f: |handle| Box::pin(float_add(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Sub"
    },
    f: |handle| Box::pin(float_sub(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Mul"
    },
    f: |handle| Box::pin(float_mul(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Div"
    },
    f: |handle| Box::pin(float_div(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Pow"
    },
    f: |handle| Box::pin(float_pow(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Min"
    },
    f: |handle| Box::pin(float_min(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Max"
    },
    f: |handle| Box::pin(float_max(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Clamp"
    },
    f: |handle| Box::pin(float_clamp(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Equals"
    },
    f: |handle| Box::pin(float_equals(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Sqrt"
    },
    f: |handle| Box::pin(float_sqrt(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Exp"
    },
    f: |handle| Box::pin(float_exp(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Ln"
    },
    f: |handle| Box::pin(float_ln(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Sin"
    },
    f: |handle| Box::pin(float_sin(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Cos"
    },
    f: |handle| Box::pin(float_cos(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Tan"
    },
    f: |handle| Box::pin(float_tan(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Float",
        name: "Atan2"
    },
    f: |handle| Box::pin(float_atan2(handle)),
});

fn signal_bool(mut handle: Handle, value: bool) {
    if value {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

fn min_float(left: f64, right: f64) -> f64 {
    if left.is_nan() || right.is_nan() {
        f64::NAN
    } else {
        left.min(right)
    }
}

fn max_float(left: f64, right: f64) -> f64 {
    if left.is_nan() || right.is_nan() {
        f64::NAN
    } else {
        left.max(right)
    }
}

fn bigint_to_float(value: BigInt) -> f64 {
    match value.to_f64() {
        Some(value) => value,
        None if value.sign() == Sign::Minus => f64::NEG_INFINITY,
        None => f64::INFINITY,
    }
}

fn float_to_bigint(value: f64) -> BigInt {
    if !value.is_finite() {
        return BigInt::ZERO;
    }

    let truncated = value.trunc();
    BigInt::parse_bytes(format!("{truncated:.0}").as_bytes(), 10).unwrap_or(BigInt::ZERO)
}

async fn float_nan(mut handle: Handle) {
    handle.receive().continue_();
    handle.provide_float(f64::NAN);
}

async fn float_inf(mut handle: Handle) {
    handle.receive().continue_();
    handle.provide_float(f64::INFINITY);
}

async fn float_neg_inf(mut handle: Handle) {
    handle.receive().continue_();
    handle.provide_float(f64::NEG_INFINITY);
}

async fn float_pi(mut handle: Handle) {
    handle.receive().continue_();
    handle.provide_float(consts::PI);
}

async fn float_e(mut handle: Handle) {
    handle.receive().continue_();
    handle.provide_float(consts::E);
}

async fn float_is_nan(mut handle: Handle) {
    let value = handle.receive().float().await;
    signal_bool(handle, value.is_nan());
}

async fn float_is_finite(mut handle: Handle) {
    let value = handle.receive().float().await;
    signal_bool(handle, value.is_finite());
}

async fn float_is_infinite(mut handle: Handle) {
    let value = handle.receive().float().await;
    signal_bool(handle, value.is_infinite());
}

async fn float_from_int(mut handle: Handle) {
    let value = handle.receive().int().await;
    handle.provide_float(bigint_to_float(value));
}

async fn float_to_int(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_int(float_to_bigint(value));
}

async fn float_to_string(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_string(format_float(value).into());
}

async fn float_from_string(mut handle: Handle) {
    let string = handle.receive().string().await;
    match parse_float_text(string.as_str()) {
        Some(value) => {
            handle.signal(literal!("ok"));
            handle.provide_float(value);
        }
        None => {
            handle.signal(literal!("err"));
            handle.break_();
        }
    }
}

async fn float_neg(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(-value);
}

async fn float_abs(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.abs());
}

async fn float_floor(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.floor());
}

async fn float_ceil(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.ceil());
}

async fn float_round(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.round());
}

async fn float_add(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(x + y);
}

async fn float_sub(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(x - y);
}

async fn float_mul(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(x * y);
}

async fn float_div(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(x / y);
}

async fn float_pow(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(x.powf(y));
}

async fn float_min(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(min_float(x, y));
}

async fn float_max(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    handle.provide_float(max_float(x, y));
}

async fn float_clamp(mut handle: Handle) {
    let value = handle.receive().float().await;
    let lo = handle.receive().float().await;
    let hi = handle.receive().float().await;
    handle.provide_float(max_float(min_float(value, hi), lo));
}

async fn float_equals(mut handle: Handle) {
    let x = handle.receive().float().await;
    let y = handle.receive().float().await;
    let tolerance = handle.receive().float().await;
    let equal = !x.is_nan()
        && !y.is_nan()
        && !tolerance.is_nan()
        && (x == y || (x - y).abs() <= tolerance.abs());
    signal_bool(handle, equal);
}

async fn float_sqrt(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.sqrt());
}

async fn float_exp(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.exp());
}

async fn float_ln(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.ln());
}

async fn float_sin(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.sin());
}

async fn float_cos(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.cos());
}

async fn float_tan(mut handle: Handle) {
    let value = handle.receive().float().await;
    handle.provide_float(value.tan());
}

async fn float_atan2(mut handle: Handle) {
    let y = handle.receive().float().await;
    let x = handle.receive().float().await;
    handle.provide_float(y.atan2(x));
}
