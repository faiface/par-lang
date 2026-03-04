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
use std::sync::Arc;

use par_core::frontend::language::{GlobalName, Unresolved};
use par_core::frontend::{Module, process};
use par_core::source::FileName;
use par_core::testing::import_test_module;

type BuiltinModule = Module<
    Arc<process::Expression<(), par_core::frontend::language::Unresolved>>,
    par_core::frontend::language::Unresolved,
>;

#[derive(Debug, Clone, Default)]
pub struct BuiltinPackage {
    pub modules: BTreeMap<String, BuiltinModule>,
}

impl BuiltinPackage {
    fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    fn append_module(&mut self, module_name: impl Into<String>, module: BuiltinModule) {
        match self.modules.entry(module_name.into()) {
            Entry::Vacant(vacant) => {
                vacant.insert(module);
            }
            Entry::Occupied(mut occupied) => {
                append_module(occupied.get_mut(), module);
            }
        }
    }

    fn append_modules(&mut self, modules: BTreeMap<String, BuiltinModule>) {
        for (module_name, module) in modules {
            self.append_module(module_name, module);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinPackages {
    pub core: BuiltinPackage,
    pub basic: Option<BuiltinPackage>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuiltinPackageRef {
    Core,
    Basic,
}

pub fn builtin_packages() -> BuiltinPackages {
    let mut core = BuiltinPackage::default();
    let mut basic = BuiltinPackage::default();

    distribute_modules_to_packages(
        split_module_by_root(parse_builtin_source(
            include_str!("./builtin/Builtin.par"),
            FileName::BUILTIN,
        )),
        &mut core,
        &mut basic,
    );

    #[cfg(not(target_arch = "wasm32"))]
    distribute_modules_to_packages(
        split_module_by_root(parse_builtin_source(
            include_str!("./builtin/NativeBuiltin.par"),
            FileName::NATIVE_BUILTIN,
        )),
        &mut core,
        &mut basic,
    );

    core.append_module("Nat", module_with_name("Nat", nat::external_module()));
    core.append_module("Int", module_with_name("Int", int::external_module()));
    core.append_module("Bench", module_with_name("Bench", bench::external_module()));
    core.append_module("Char", module_with_name("Char", char_::external_module()));
    core.append_module(
        "String",
        module_with_name("String", string::external_module()),
    );
    core.append_module("Byte", module_with_name("Byte", byte::external_module()));
    core.append_module("Bytes", module_with_name("Bytes", bytes::external_module()));
    core.append_module("Map", module_with_name("Map", map::external_module()));
    core.append_module(
        "BoxMap",
        module_with_name("BoxMap", boxmap::external_module()),
    );
    core.append_module("Url", module_with_name("Url", url::external_module()));

    basic.append_module("Debug", module_with_name("Debug", debug::external_module()));
    basic.append_module(
        "Console",
        module_with_name("Console", console::external_module()),
    );
    basic.append_module("Time", module_with_name("Time", time::external_module()));

    #[cfg(not(target_arch = "wasm32"))]
    {
        basic.append_module("Os", module_with_name("Os", os::external_module()));
        basic.append_module("Http", module_with_name("Http", http::external_module()));
    }

    let mut test = BuiltinModule::default();
    import_test_module(&mut test);
    core.append_modules(split_module_by_root(test));

    BuiltinPackages {
        core,
        basic: (!basic.is_empty()).then_some(basic),
    }
}

pub fn import_builtins(module: &mut BuiltinModule) {
    let packages = builtin_packages();
    import_package(module, packages.core);
    if let Some(basic) = packages.basic {
        import_package(module, basic);
    }
}

pub fn import_core_builtins(module: &mut BuiltinModule) {
    import_package(module, builtin_packages().core);
}

pub fn import_basic_builtins(module: &mut BuiltinModule) {
    if let Some(basic) = builtin_packages().basic {
        import_package(module, basic);
    }
}

fn import_package(target: &mut BuiltinModule, package: BuiltinPackage) {
    for (_, module) in package.modules {
        append_module(target, module);
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

fn distribute_modules_to_packages(
    modules: BTreeMap<String, BuiltinModule>,
    core: &mut BuiltinPackage,
    basic: &mut BuiltinPackage,
) {
    for (module_name, module) in modules {
        match package_for_builtin_root(&module_name) {
            BuiltinPackageRef::Core => core.append_module(module_name, module),
            BuiltinPackageRef::Basic => basic.append_module(module_name, module),
        }
    }
}

fn parse_builtin_source(source: &str, file: FileName) -> BuiltinModule {
    BuiltinModule::parse_and_compile(source, file).expect("builtin source should compile")
}

fn split_module_by_root(module: BuiltinModule) -> BTreeMap<String, BuiltinModule> {
    let mut split = BTreeMap::<String, BuiltinModule>::new();
    let BuiltinModule {
        type_defs,
        declarations,
        definitions,
    } = module;

    for type_def in type_defs {
        let root = root_builtin_module_name(&type_def.name).to_string();
        split.entry(root).or_default().type_defs.push(type_def);
    }
    for declaration in declarations {
        let root = root_builtin_module_name(&declaration.name).to_string();
        split
            .entry(root)
            .or_default()
            .declarations
            .push(declaration);
    }
    for definition in definitions {
        let root = root_builtin_module_name(&definition.name).to_string();
        split.entry(root).or_default().definitions.push(definition);
    }
    split
}

fn append_module(target: &mut BuiltinModule, mut other: BuiltinModule) {
    target.type_defs.append(&mut other.type_defs);
    target.declarations.append(&mut other.declarations);
    target.definitions.append(&mut other.definitions);
}

fn root_builtin_module_name(name: &GlobalName<Unresolved>) -> &str {
    name.module
        .qualifier
        .as_deref()
        .unwrap_or(name.primary.as_str())
}

fn package_for_builtin_root(root: &str) -> BuiltinPackageRef {
    match root {
        "Console" | "Debug" | "Http" | "Os" | "Time" => BuiltinPackageRef::Basic,
        _ => BuiltinPackageRef::Core,
    }
}
