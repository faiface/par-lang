use arcstr::literal;
use num_bigint::BigInt;

use par_core::frontend::ExternalTypeDef;
use par_core::frontend::{PrimitiveType, Type};
use par_core::source::Span;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalTypeDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Byte",
        name: "Byte"
    },
    typ: Type::Primitive(Span::None, PrimitiveType::Byte)
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Byte",
        name: "Equals"
    },
    f: |handle| Box::pin(byte_equals(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Byte",
        name: "Code"
    },
    f: |handle| Box::pin(byte_code(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Byte",
        name: "Is"
    },
    f: |handle| Box::pin(byte_is(handle)),
});

async fn byte_equals(mut handle: Handle) {
    let x = handle.receive().byte().await;
    let y = handle.receive().byte().await;
    if x == y {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

async fn byte_code(mut handle: Handle) {
    let c = handle.receive().byte().await;
    handle.provide_nat(BigInt::from(c))
}

async fn byte_is(mut handle: Handle) {
    let b = handle.receive().byte().await;
    let class = ByteClass::readback(handle.receive()).await;
    if class.contains(b) {
        handle.signal(literal!("true"));
    } else {
        handle.signal(literal!("false"));
    }
    handle.break_();
}

#[derive(Debug, Clone)]
pub(super) enum ByteClass {
    Any,
    Byte(u8),
    Range(u8, u8),
}

impl ByteClass {
    pub(super) async fn readback(mut handle: Handle) -> Self {
        match handle.case().await.as_str() {
            "any" => Self::Any,
            "byte" => Self::Byte(handle.byte().await),
            "range" => {
                let min = handle.receive().byte().await;
                let max = handle.receive().byte().await;
                handle.continue_();
                Self::Range(min, max)
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn contains(&self, b: u8) -> bool {
        match self {
            Self::Any => true,
            Self::Byte(b1) => b == *b1,
            Self::Range(min, max) => *min <= b && b <= *max,
        }
    }
}
