mod bench;
mod boxmap;
mod byte;
mod bytes;
mod char_;
mod console;
mod debug;
#[cfg(not(target_arch = "wasm32"))]
mod http;
mod int;
mod list;
mod map;
mod nat;
#[cfg(not(target_arch = "wasm32"))]
mod os;
mod parser;
mod string;
mod time;
mod url;

use std::collections::{BTreeMap, btree_map::Entry};
use std::path::PathBuf;
use std::sync::Arc;

use par_core::frontend::language::Unresolved;
use par_core::frontend::{Definition, Module, TypeDef, get_external_type_defs, process};
use par_runtime::registry::{PackageRef, get_external_defs};

pub type BuiltinModule = Module<Arc<process::Expression<(), Unresolved>>, Unresolved>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinModulePath {
    pub directories: Vec<String>,
    pub module: String,
}

impl BuiltinModulePath {
    pub fn new(
        directories: impl IntoIterator<Item = impl Into<String>>,
        module: impl Into<String>,
    ) -> Self {
        Self {
            directories: directories.into_iter().map(Into::into).collect(),
            module: module.into(),
        }
    }

    pub fn root(module: impl Into<String>) -> Self {
        Self::new(Vec::<String>::new(), module)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinPackage {
    pub root: PathBuf,
    pub externals: BTreeMap<BuiltinModulePath, BuiltinModule>,
}

#[derive(Debug, Clone)]
pub struct BuiltinPackages {
    pub packages: BTreeMap<String, BuiltinPackage>,
}

pub fn builtin_packages() -> BuiltinPackages {
    let mut packages = BTreeMap::new();
    packages.insert("core".to_string(), core_builtin_package());

    #[cfg(not(target_arch = "wasm32"))]
    packages.insert("basic".to_string(), basic_builtin_package());

    BuiltinPackages { packages }
}

fn core_builtin_package() -> BuiltinPackage {
    let mut package = BuiltinPackage {
        root: builtin_package_root("core"),
        externals: BTreeMap::new(),
    };

    package.load_external_type_defs("core");
    package
}

#[cfg(not(target_arch = "wasm32"))]
fn basic_builtin_package() -> BuiltinPackage {
    let mut package = BuiltinPackage {
        root: builtin_package_root("basic"),
        externals: BTreeMap::new(),
    };

    package.load_external_type_defs("basic");

    package
}

fn builtin_package_root(package_name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("packages")
        .join(package_name)
}

impl BuiltinPackage {
    fn append_external(&mut self, module_path: BuiltinModulePath, module: BuiltinModule) {
        match self.externals.entry(module_path) {
            Entry::Vacant(vacant) => {
                vacant.insert(module);
            }
            Entry::Occupied(mut occupied) => {
                append_module(occupied.get_mut(), module);
            }
        }
    }

    fn load_external_type_defs(&mut self, name: &str) {
        for type_def in get_external_type_defs(&PackageRef::Package(name)) {
            let builtin_path = BuiltinModulePath {
                directories: type_def.path.path.iter().map(|s| s.to_string()).collect(),
                module: type_def.path.module.into(),
            };
            if !self.externals.contains_key(&builtin_path) {
                self.append_external(builtin_path.clone(), BuiltinModule::default());
            }
            self.externals
                .get_mut(&builtin_path)
                .unwrap()
                .type_defs
                .push(TypeDef::external(
                    type_def.path.name,
                    &[],
                    type_def.typ.clone(),
                ));
        }
    }
}

fn append_module(target: &mut BuiltinModule, mut other: BuiltinModule) {
    target.type_defs.append(&mut other.type_defs);
    target.declarations.append(&mut other.declarations);
    target.definitions.append(&mut other.definitions);
}
