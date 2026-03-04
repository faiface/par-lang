#![deny(unreachable_pub)]

mod builtin;

pub use builtin::{
    BuiltinModule, BuiltinModulePath, BuiltinPackage, BuiltinPackages, builtin_packages,
};
