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

use par_core::frontend::language::{GlobalName, Unresolved};
use par_core::frontend::{Module, process};
use par_core::testing::import_test_module;

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

    package.append_root_external("Nat", nat::external_module());
    package.append_root_external("Int", int::external_module());
    package.append_root_external("Bench", bench::external_module());
    package.append_root_external("Char", char_::external_module());
    package.append_root_external("String", string::external_module());
    package.append_root_external("Byte", byte::external_module());
    package.append_root_external("Bytes", bytes::external_module());
    package.append_root_external("Map", map::external_module());
    package.append_root_external("BoxMap", boxmap::external_module());
    package.append_root_external("Url", url::external_module());

    let mut test = BuiltinModule::default();
    import_test_module(&mut test);
    package.append_root_external("Test", test);

    package
}

#[cfg(not(target_arch = "wasm32"))]
fn basic_builtin_package() -> BuiltinPackage {
    let mut package = BuiltinPackage {
        root: builtin_package_root("basic"),
        externals: BTreeMap::new(),
    };

    package.append_root_external("Debug", debug::external_module());
    package.append_root_external("Console", console::external_module());
    package.append_root_external("Time", time::external_module());

    package.append_root_external("Os", os::external_module());
    package.append_root_external("Http", http::external_module());

    package
}

fn builtin_package_root(package_name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("packages")
        .join(package_name)
}

impl BuiltinPackage {
    fn append_external(&mut self, module_path: BuiltinModulePath, module: BuiltinModule) {
        let module = module_with_name(&module_path.module, module);
        match self.externals.entry(module_path) {
            Entry::Vacant(vacant) => {
                vacant.insert(module);
            }
            Entry::Occupied(mut occupied) => {
                append_module(occupied.get_mut(), module);
            }
        }
    }

    fn append_root_external(&mut self, module_name: impl Into<String>, module: BuiltinModule) {
        self.append_external(BuiltinModulePath::root(module_name), module);
    }
}

fn module_with_name(module_name: &str, module: BuiltinModule) -> BuiltinModule {
    module
        .map_global_names(|mut name| {
            if name.module.qualifier.is_none() {
                name.module.qualifier = Some(module_name.to_string());
            }
            Ok::<GlobalName<Unresolved>, ()>(name)
        })
        .expect("builtin module naming should not fail")
}

fn append_module(target: &mut BuiltinModule, mut other: BuiltinModule) {
    target.type_defs.append(&mut other.type_defs);
    target.declarations.append(&mut other.declarations);
    target.definitions.append(&mut other.definitions);
}
