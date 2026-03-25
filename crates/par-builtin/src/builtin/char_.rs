use arcstr::literal;
use num_bigint::BigInt;

use par_core::frontend::{ExternalTypeDef, PrimitiveType, Type};
use par_core::source::Span;
use par_runtime::readback::Handle;
use par_runtime::registry::{DefinitionRef, ExternalDef, PackageRef};

inventory::submit!(ExternalTypeDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Char",
        name: "Char"
    },
    typ: Type::Primitive(Span::None, PrimitiveType::Char)
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Char",
        name: "Equals"
    },
    f: |handle| Box::pin(char_equals(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Char",
        name: "Code"
    },
    f: |handle| Box::pin(char_code(handle)),
});

inventory::submit!(ExternalDef {
    path: DefinitionRef {
        package: PackageRef::Special("core"),
        path: &[],
        module: "Char",
        name: "Is"
    },
    f: |handle| Box::pin(char_is(handle)),
});

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
    handle.provide_nat(BigInt::from(c as u32));
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
pub(super) enum CharClass {
    Any,
    Char(char),
    Whitespace,
    AsciiAny,
    AsciiAlpha,
    AsciiAlphanum,
    AsciiDigit,
}

impl CharClass {
    pub(super) async fn readback(mut handle: Handle) -> Self {
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

    pub(super) fn contains(&self, ch: char) -> bool {
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
