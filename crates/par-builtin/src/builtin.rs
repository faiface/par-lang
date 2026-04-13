mod bench;
mod boxmap;
mod byte;
mod bytes;
mod char_;
mod console;
mod debug;
mod float;
#[cfg(not(target_family = "wasm"))]
mod http;
mod int;
mod json;
mod list;
mod map;
mod nat;
#[cfg(not(target_family = "wasm"))]
mod os;
mod parser;
mod string;
mod time;
mod url;

use std::collections::{BTreeMap, btree_map::Entry};
use std::path::PathBuf;

use arcstr::literal;
use par_core::frontend::language::PackageId;
use par_core::frontend::{TypeDef, get_external_type_defs};
use par_core::source::FileName;
use par_core::workspace::{
    ExternalModule, LoadedPackageFile, ModulePath, WorkspacePackage, parse_loaded_files,
};
use par_runtime::registry::PackageRef;

pub fn builtin_packages() -> Vec<WorkspacePackage> {
    let mut packages = Vec::new();
    packages.push(core_package());

    #[cfg(not(target_family = "wasm"))]
    packages.push(basic_package());

    packages
}

fn core_package() -> WorkspacePackage {
    let parsed = parse_builtin_sources("core", CORE_SOURCE_FILES);
    let externals = load_external_type_defs("core");
    WorkspacePackage::new(PackageId::Special(literal!("core")), parsed).with_externals(externals)
}

#[cfg(not(target_family = "wasm"))]
fn basic_package() -> WorkspacePackage {
    let parsed = parse_builtin_sources("basic", BASIC_SOURCE_FILES);
    let externals = load_external_type_defs("basic");
    WorkspacePackage::new(PackageId::Special(literal!("basic")), parsed)
        .with_dependency("core", PackageId::Special(literal!("core")))
        .with_externals(externals)
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
        relative_path_from_src: "Debug.par",
        source: include_str!("../packages/core/src/Debug.par"),
    },
    BuiltinSourceFile {
        relative_path_from_src: "Float.par",
        source: include_str!("../packages/core/src/Float.par"),
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
        relative_path_from_src: "Json.par",
        source: include_str!("../packages/core/src/Json.par"),
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

#[cfg(not(target_family = "wasm"))]
const BASIC_SOURCE_FILES: &[BuiltinSourceFile] = &[
    BuiltinSourceFile {
        relative_path_from_src: "Console.par",
        source: include_str!("../packages/basic/src/Console.par"),
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

fn parse_builtin_sources(
    package_name: &str,
    source_files: &[BuiltinSourceFile],
) -> par_core::workspace::ParsedPackage {
    let files = source_files
        .iter()
        .map(|file| LoadedPackageFile {
            name: FileName::from(
                format!("par:{}/{}", package_name, file.relative_path_from_src).as_str(),
            ),
            relative_path_from_src: PathBuf::from(file.relative_path_from_src),
            source: file.source.to_owned(),
        })
        .collect();
    parse_loaded_files(files).expect("embedded builtin package should parse")
}

fn load_external_type_defs(name: &str) -> BTreeMap<ModulePath, ExternalModule> {
    let mut externals = BTreeMap::<ModulePath, ExternalModule>::new();
    for type_def in get_external_type_defs(&PackageRef::Special(name)) {
        let module_path = ModulePath {
            directories: type_def.path.path.iter().map(|s| s.to_string()).collect(),
            module: type_def.path.module.into(),
        };
        match externals.entry(module_path) {
            Entry::Vacant(vacant) => {
                let mut module = ExternalModule::default();
                module.type_defs.push(TypeDef::external(
                    type_def.path.name,
                    &[],
                    type_def.typ.clone(),
                ));
                vacant.insert(module);
            }
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().type_defs.push(TypeDef::external(
                    type_def.path.name,
                    &[],
                    type_def.typ.clone(),
                ));
            }
        }
    }
    externals
}
