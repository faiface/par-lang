use std::collections::{BTreeMap, HashMap, btree_map::Entry};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use par_builtin::{BuiltinPackage, builtin_packages};
use par_core::frontend::language::{
    CompileError, GlobalName, Resolved, Universal, UniversalPackage, Unresolved,
};
use par_core::frontend::process;
use par_core::frontend::{ImportDecl, ImportPath, Module, lower};
use par_core::source::{FileName, Span};

use crate::package_loader::{
    CanonicalModulePath, PackageLoadError, ParsedPackage, ParsedPackageFile, load_package,
};

type ResolvedModulePath = Resolved;
type UniversalModulePath = Universal;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ModuleLookupKey {
    package: UniversalPackage,
    directories: Vec<String>,
    module_lower: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ResolvedLookupKey {
    directories: Vec<String>,
    module_lower: String,
}

#[derive(Debug, Clone)]
pub struct LoweredPackage {
    pub lowered: Module<Arc<process::Expression<(), Universal>>, Universal>,
    pub local_modules: Vec<CanonicalModulePath>,
    pub sources: HashMap<FileName, Arc<str>>,
}

#[derive(Debug, Clone)]
pub enum PackageBuildError {
    Load(PackageLoadError),
    LowerError {
        path: PathBuf,
        source: Arc<str>,
        error: CompileError,
    },
    UnsupportedDependency {
        source: Arc<str>,
        span: Span,
        dependency: String,
    },
    ImportedModuleNotFound {
        source: Arc<str>,
        span: Span,
        import_path: String,
    },
    DuplicateImportAlias {
        source: Arc<str>,
        span: Span,
        alias: String,
    },
    UnknownModuleQualifier {
        source: Arc<str>,
        span: Span,
        qualifier: String,
        name: String,
    },
    ResolvedModuleNotFound {
        source: Arc<str>,
        span: Span,
        module_path: String,
    },
    AmbiguousResolvedModule {
        module_path: String,
        first: String,
        second: String,
    },
}

impl Display for PackageBuildError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load(error) => write!(f, "{error}"),
            Self::LowerError { path, .. } => {
                write!(f, "Failed to lower source file {}", path.display())
            }
            Self::UnsupportedDependency { dependency, .. } => {
                write!(f, "Unsupported dependency `@{}`", dependency)
            }
            Self::ImportedModuleNotFound { import_path, .. } => {
                write!(f, "Imported module `{}` was not found", import_path)
            }
            Self::DuplicateImportAlias { alias, .. } => {
                write!(f, "Duplicate import alias `{}`", alias)
            }
            Self::UnknownModuleQualifier {
                qualifier, name, ..
            } => write!(
                f,
                "Unknown module qualifier `{}` in reference `{}`",
                qualifier, name
            ),
            Self::ResolvedModuleNotFound { module_path, .. } => {
                write!(f, "Resolved module `{}` was not found", module_path)
            }
            Self::AmbiguousResolvedModule {
                module_path,
                first,
                second,
            } => write!(
                f,
                "Resolved module `{}` is ambiguous between `{}` and `{}`",
                module_path, first, second
            ),
        }
    }
}

impl std::error::Error for PackageBuildError {}

pub fn load_lowered_package(start: impl AsRef<Path>) -> Result<LoweredPackage, PackageBuildError> {
    let parsed = load_package(start).map_err(PackageBuildError::Load)?;
    lower_parsed_package(parsed)
}

pub fn lower_parsed_package(parsed: ParsedPackage) -> Result<LoweredPackage, PackageBuildError> {
    let local_module_lookup = build_local_module_lookup(&parsed);
    let (builtin_module_lookup, builtin_universal_lookup, builtin_module) = build_builtin_context();
    let universal_lookup = build_resolved_to_universal_lookup(&parsed, builtin_universal_lookup)?;

    let mut lowered = Module::default();
    append_module(&mut lowered, builtin_module);
    let mut sources = HashMap::<FileName, Arc<str>>::new();

    for parsed_module in &parsed.modules {
        let current_module_path = resolved_from_local(&parsed_module.path);

        for file in &parsed_module.files {
            sources.insert(file.absolute_path.clone().into(), Arc::clone(&file.source));
            let imports = build_file_import_aliases(
                file,
                &current_module_path,
                &local_module_lookup,
                &builtin_module_lookup,
            )?;
            let lowered_file =
                lower_file_to_universal(file, &current_module_path, imports, &universal_lookup)?;
            append_module(&mut lowered, lowered_file);
        }
    }

    Ok(LoweredPackage {
        lowered,
        local_modules: parsed
            .modules
            .into_iter()
            .map(|module| module.path)
            .collect(),
        sources,
    })
}

fn lower_file_to_universal(
    file: &ParsedPackageFile,
    current_module_path: &ResolvedModulePath,
    imports: BTreeMap<String, ResolvedModulePath>,
    universal_lookup: &BTreeMap<ResolvedLookupKey, UniversalModulePath>,
) -> Result<Module<Arc<process::Expression<(), Universal>>, Universal>, PackageBuildError> {
    let lowered_file =
        lower(file.source_file.body.clone()).map_err(|error| PackageBuildError::LowerError {
            path: file.absolute_path.clone(),
            source: Arc::clone(&file.source),
            error,
        })?;
    let resolved_file = resolve_file_names(
        lowered_file,
        imports,
        current_module_path,
        Arc::clone(&file.source),
    )?;
    universalize_file_names(resolved_file, universal_lookup, Arc::clone(&file.source))
}

fn resolve_file_names(
    module: Module<Arc<process::Expression<(), Unresolved>>, Unresolved>,
    imports: BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
    file_source: Arc<str>,
) -> Result<Module<Arc<process::Expression<(), Resolved>>, Resolved>, PackageBuildError> {
    module.map_global_names(|name| {
        resolve_name_to_resolved(
            name,
            &imports,
            current_module_path,
            Arc::clone(&file_source),
        )
    })
}

fn universalize_file_names(
    module: Module<Arc<process::Expression<(), Resolved>>, Resolved>,
    universal_lookup: &BTreeMap<ResolvedLookupKey, UniversalModulePath>,
    file_source: Arc<str>,
) -> Result<Module<Arc<process::Expression<(), Universal>>, Universal>, PackageBuildError> {
    module.map_global_names(|name| {
        resolve_name_to_universal(name, universal_lookup, Arc::clone(&file_source))
    })
}

fn build_local_module_lookup(
    parsed: &ParsedPackage,
) -> BTreeMap<ModuleLookupKey, ResolvedModulePath> {
    let mut map = BTreeMap::new();
    for module in &parsed.modules {
        let path = resolved_from_local(&module.path);
        map.insert(module_lookup_key(UniversalPackage::Local, &path), path);
    }
    map
}

fn build_builtin_context() -> (
    BTreeMap<ModuleLookupKey, ResolvedModulePath>,
    BTreeMap<ResolvedLookupKey, UniversalModulePath>,
    Module<Arc<process::Expression<(), Universal>>, Universal>,
) {
    let packages = builtin_packages();
    let mut builtin_packages = vec![(
        UniversalPackage::Dependency("core".to_string()),
        packages.core,
    )];
    if let Some(basic) = packages.basic {
        builtin_packages.push((UniversalPackage::Dependency("basic".to_string()), basic));
    }

    let mut root_packages = BTreeMap::<String, UniversalPackage>::new();
    for (package_ref, package) in &builtin_packages {
        for module_name in package.modules.keys() {
            if let Some(previous) = root_packages.insert(module_name.clone(), package_ref.clone()) {
                panic!(
                    "builtin module root `{}` appears in both {:?} and {:?}",
                    module_name, previous, package_ref
                );
            }
        }
    }

    let mut lookup = BTreeMap::new();
    let mut universal_lookup = BTreeMap::new();
    for (root, package) in &root_packages {
        let resolved = ResolvedModulePath {
            directories: Vec::new(),
            module: root.clone(),
        };
        let universal = UniversalModulePath {
            package: package.clone(),
            directories: Vec::new(),
            module: root.clone(),
        };
        lookup.insert(
            module_lookup_key(package.clone(), &resolved),
            resolved.clone(),
        );
        universal_lookup.insert(resolved_lookup_key(&resolved), universal);
    }

    let mut lowered = Module::default();
    for (_package_ref, package) in builtin_packages {
        append_builtin_package(&mut lowered, package, &universal_lookup);
    }

    (lookup, universal_lookup, lowered)
}

fn root_builtin_module_name(name: &GlobalName<Unresolved>) -> String {
    name.module
        .qualifier
        .clone()
        .unwrap_or_else(|| name.primary.clone())
}

fn append_builtin_package(
    lowered: &mut Module<Arc<process::Expression<(), Universal>>, Universal>,
    package: BuiltinPackage,
    universal_lookup: &BTreeMap<ResolvedLookupKey, UniversalModulePath>,
) {
    for (_, module) in package.modules {
        let resolved_module = module
            .map_global_names(|name| {
                let root = root_builtin_module_name(&name);
                Ok::<GlobalName<Resolved>, ()>(GlobalName::new(
                    name.span,
                    ResolvedModulePath {
                        directories: Vec::new(),
                        module: root,
                    },
                    name.primary,
                ))
            })
            .expect("builtin name rewriting should not fail");
        let universal_module = resolved_module
            .map_global_names(|name| {
                let module_path = universal_lookup
                    .get(&resolved_lookup_key(&name.module))
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!(
                            "builtin name `{}` references unknown resolved module `{}`",
                            name, name.module
                        )
                    });
                Ok::<GlobalName<Universal>, ()>(GlobalName::new(
                    name.span,
                    module_path,
                    name.primary,
                ))
            })
            .expect("builtin universal rewrite should not fail");
        append_module(lowered, universal_module);
    }
}

fn build_file_import_aliases(
    file: &ParsedPackageFile,
    current_module_path: &ResolvedModulePath,
    local_lookup: &BTreeMap<ModuleLookupKey, ResolvedModulePath>,
    builtin_lookup: &BTreeMap<ModuleLookupKey, ResolvedModulePath>,
) -> Result<BTreeMap<String, ResolvedModulePath>, PackageBuildError> {
    let mut aliases = BTreeMap::new();
    aliases.insert(
        current_module_path.module.clone(),
        current_module_path.clone(),
    );

    for import in &file.source_file.imports {
        let imported_module = resolve_imported_module(import, file, local_lookup, builtin_lookup)?;
        let alias = import
            .alias
            .clone()
            .unwrap_or_else(|| imported_module.module.clone());

        match aliases.entry(alias.clone()) {
            Entry::Vacant(vacant) => {
                vacant.insert(imported_module);
            }
            Entry::Occupied(_) => {
                return Err(PackageBuildError::DuplicateImportAlias {
                    source: Arc::clone(&file.source),
                    span: import.span.clone(),
                    alias,
                });
            }
        }
    }

    Ok(aliases)
}

fn resolve_imported_module(
    import: &ImportDecl,
    file: &ParsedPackageFile,
    local_lookup: &BTreeMap<ModuleLookupKey, ResolvedModulePath>,
    builtin_lookup: &BTreeMap<ModuleLookupKey, ResolvedModulePath>,
) -> Result<ResolvedModulePath, PackageBuildError> {
    let dependency = match import.path.dependency.as_deref() {
        None => UniversalPackage::Local,
        Some("core") => UniversalPackage::Dependency("core".to_string()),
        Some("basic") => UniversalPackage::Dependency("basic".to_string()),
        Some(other) => {
            return Err(PackageBuildError::UnsupportedDependency {
                source: Arc::clone(&file.source),
                span: import.span.clone(),
                dependency: other.to_string(),
            });
        }
    };

    let lookup_key = ModuleLookupKey {
        package: dependency.clone(),
        directories: import
            .path
            .directories
            .iter()
            .map(|segment| segment.to_lowercase())
            .collect(),
        module_lower: import.path.module.to_lowercase(),
    };

    let lookup = match dependency {
        UniversalPackage::Local => local_lookup,
        UniversalPackage::Dependency(_) => builtin_lookup,
    };

    lookup
        .get(&lookup_key)
        .cloned()
        .ok_or_else(|| PackageBuildError::ImportedModuleNotFound {
            source: Arc::clone(&file.source),
            span: import.span.clone(),
            import_path: format_import_path(&import.path),
        })
}

fn format_import_path(path: &ImportPath) -> String {
    let mut segments = Vec::new();
    if let Some(dependency) = &path.dependency {
        segments.push(format!("@{dependency}"));
    }
    for directory in &path.directories {
        segments.push(directory.clone());
    }
    segments.push(path.module.clone());
    segments.join("/")
}

fn resolve_name_to_resolved(
    mut name: GlobalName<Unresolved>,
    imports: &BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
    file_source: Arc<str>,
) -> Result<GlobalName<Resolved>, PackageBuildError> {
    if let Some(module_qualifier) = name.module.qualifier.take() {
        let Some(target_module) = imports.get(module_qualifier.as_str()) else {
            return Err(PackageBuildError::UnknownModuleQualifier {
                source: file_source,
                span: name.span.clone(),
                qualifier: module_qualifier.clone(),
                name: format!("{}.{}", module_qualifier, name.primary),
            });
        };

        return Ok(GlobalName::new(
            name.span,
            target_module.clone(),
            name.primary,
        ));
    }

    let (module, primary) = if let Some(target_module) = imports.get(&name.primary) {
        (target_module.clone(), target_module.module.clone())
    } else {
        (current_module_path.clone(), name.primary)
    };
    Ok(GlobalName::new(name.span, module, primary))
}

fn resolve_name_to_universal(
    name: GlobalName<Resolved>,
    universal_lookup: &BTreeMap<ResolvedLookupKey, UniversalModulePath>,
    file_source: Arc<str>,
) -> Result<GlobalName<Universal>, PackageBuildError> {
    let module = universal_lookup
        .get(&resolved_lookup_key(&name.module))
        .cloned()
        .ok_or_else(|| PackageBuildError::ResolvedModuleNotFound {
            source: file_source,
            span: name.span.clone(),
            module_path: name.module.to_string(),
        })?;

    Ok(GlobalName::new(name.span, module, name.primary))
}

fn build_resolved_to_universal_lookup(
    parsed: &ParsedPackage,
    builtin_lookup: BTreeMap<ResolvedLookupKey, UniversalModulePath>,
) -> Result<BTreeMap<ResolvedLookupKey, UniversalModulePath>, PackageBuildError> {
    let mut lookup = builtin_lookup;
    for module in &parsed.modules {
        let resolved = resolved_from_local(&module.path);
        let universal = universal_from_local(&module.path);
        insert_universal_lookup(&mut lookup, &resolved, universal)?;
    }
    Ok(lookup)
}

fn universal_from_local(local: &CanonicalModulePath) -> UniversalModulePath {
    UniversalModulePath {
        package: UniversalPackage::Local,
        directories: local.directories.clone(),
        module: local.module.clone(),
    }
}

fn resolved_from_local(local: &CanonicalModulePath) -> ResolvedModulePath {
    ResolvedModulePath {
        directories: local.directories.clone(),
        module: local.module.clone(),
    }
}

fn insert_universal_lookup(
    lookup: &mut BTreeMap<ResolvedLookupKey, UniversalModulePath>,
    resolved: &ResolvedModulePath,
    universal: UniversalModulePath,
) -> Result<(), PackageBuildError> {
    let key = resolved_lookup_key(resolved);
    if let Some(existing) = lookup.get(&key) {
        if existing != &universal {
            return Err(PackageBuildError::AmbiguousResolvedModule {
                module_path: resolved.to_string(),
                first: existing.to_string(),
                second: universal.to_string(),
            });
        }
        return Ok(());
    }
    lookup.insert(key, universal);
    Ok(())
}

fn module_lookup_key(package: UniversalPackage, path: &ResolvedModulePath) -> ModuleLookupKey {
    ModuleLookupKey {
        package,
        directories: path.directories.clone(),
        module_lower: path.module.to_lowercase(),
    }
}

fn resolved_lookup_key(path: &ResolvedModulePath) -> ResolvedLookupKey {
    ResolvedLookupKey {
        directories: path.directories.clone(),
        module_lower: path.module.to_lowercase(),
    }
}

fn append_module(
    target: &mut Module<Arc<process::Expression<(), Universal>>, Universal>,
    mut other: Module<Arc<process::Expression<(), Universal>>, Universal>,
) {
    target.type_defs.append(&mut other.type_defs);
    target.declarations.append(&mut other.declarations);
    target.definitions.append(&mut other.definitions);
}
