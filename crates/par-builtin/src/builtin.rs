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
use par_core::frontend::{Module, TypeDef, get_external_type_defs, process};
use par_core::workspace::{LoadedPackageFile, ParsedPackage, parse_loaded_files};
use par_runtime::registry::PackageRef;

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
    pub parsed: ParsedPackage,
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
        parsed: parse_builtin_package("core", CORE_SOURCE_FILES),
        externals: BTreeMap::new(),
    };

    package.load_external_type_defs("core");
    package
}

#[cfg(not(target_arch = "wasm32"))]
fn basic_builtin_package() -> BuiltinPackage {
    let mut package = BuiltinPackage {
        parsed: parse_builtin_package("basic", BASIC_SOURCE_FILES),
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

struct BuiltinSourceFile {
    relative_path_from_src: &'static str,
    source: &'static str,
}

const CORE_SOURCE_FILES: &[BuiltinSourceFile] = &[
    BuiltinSourceFile {
        relative_path_from_src: "Bench.par",
        source: include_str!("../packages/core/src/Bench.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Bool.par",
        source: include_str!("../packages/core/src/Bool.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "BoxMap.par",
        source: include_str!("../packages/core/src/BoxMap.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Byte.par",
        source: include_str!("../packages/core/src/Byte.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Bytes.par",
        source: include_str!("../packages/core/src/Bytes.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Cell.par",
        source: include_str!("../packages/core/src/Cell.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Char.par",
        source: include_str!("../packages/core/src/Char.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Int.par",
        source: include_str!("../packages/core/src/Int.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "List.par",
        source: include_str!("../packages/core/src/List.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Map.par",
        source: include_str!("../packages/core/src/Map.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Nat.par",
        source: include_str!("../packages/core/src/Nat.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Option.par",
        source: include_str!("../packages/core/src/Option.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Ordering.par",
        source: include_str!("../packages/core/src/Ordering.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Result.par",
        source: include_str!("../packages/core/src/Result.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "String.par",
        source: include_str!("../packages/core/src/String.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Test.par",
        source: include_str!("../packages/core/src/Test.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Url.par",
        source: include_str!("../packages/core/src/Url.par"),
    },
];

#[cfg(not(target_arch = "wasm32"))]
const BASIC_SOURCE_FILES: &[BuiltinSourceFile] = &[
    BuiltinSourceFile {
        relative_path_from_src: "Console.par",
        source: include_str!("../packages/basic/src/Console.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Debug.par",
        source: include_str!("../packages/basic/src/Debug.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Http.par",
        source: include_str!("../packages/basic/src/Http.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Os.par",
        source: include_str!("../packages/basic/src/Os.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Time.par",
        source: include_str!("../packages/basic/src/Time.par"),
    },
];

fn parse_builtin_package(
    package_name: &str,
    source_files: &[BuiltinSourceFile],
) -> ParsedPackage {
    let root = builtin_package_root(package_name);
    let files = source_files
        .iter()
        .map(|file| LoadedPackageFile {
            absolute_path: root.join("src").join(file.relative_path_from_src),
            relative_path_from_src: PathBuf::from(file.relative_path_from_src),
            source: file.source.to_owned(),
        })
        .collect();
    parse_loaded_files(files).expect("embedded builtin package should parse")
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
