use std::collections::{BTreeMap, HashMap, btree_map::Entry};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use par_builtin::{BuiltinModule, BuiltinModulePath, BuiltinPackage, builtin_packages};
use par_core::frontend::language::{
    CompileError, GlobalName, PackageId, Resolved, ResolvedPackageRef, Universal, Unresolved,
};
use par_core::frontend::process;
use par_core::frontend::{ImportDecl, ImportPath, Module, lower};
use par_core::source::{FileName, Span};

use crate::package_loader::{
    CanonicalModulePath, PackageLoadError, ParsedPackage, ParsedPackageFile, parse_package,
};

type ResolvedModulePath = Resolved;
type UniversalModulePath = Universal;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct AbsoluteModuleLookupKey {
    package: PackageId,
    directories: Vec<String>,
    module_lower: String,
}

#[derive(Debug, Clone)]
struct PackageUnit {
    id: PackageId,
    parsed: ParsedPackage,
    dependencies: BTreeMap<String, PackageId>,
    externals: BTreeMap<CanonicalModulePath, BuiltinModule>,
    collect_sources: bool,
}

#[derive(Debug, Clone)]
pub struct PackageUniverse {
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
    UnknownDependency {
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
    UnattachedExternalModule {
        package: PackageId,
        module_path: String,
    },
}

impl Display for PackageBuildError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load(error) => write!(f, "{error}"),
            Self::LowerError { path, .. } => {
                write!(f, "Failed to lower source file {}", path.display())
            }
            Self::UnknownDependency { dependency, .. } => {
                write!(f, "Unknown dependency alias `@{dependency}`")
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
            Self::UnattachedExternalModule {
                package,
                module_path,
            } => write!(
                f,
                "External module `{}` was not attached to any parsed module in package `{:?}`",
                module_path, package
            ),
        }
    }
}

impl std::error::Error for PackageBuildError {}

pub fn parse_and_load_package(
    start: impl AsRef<Path>,
) -> Result<PackageUniverse, PackageBuildError> {
    let parsed = parse_package(start).map_err(PackageBuildError::Load)?;
    load_parsed_package(parsed)
}

pub fn load_parsed_package(
    local_parsed: ParsedPackage,
) -> Result<PackageUniverse, PackageBuildError> {
    let package_units = build_package_units(local_parsed)?;
    let module_lookup = build_module_lookup(&package_units);
    let local_modules = local_module_paths(&package_units);

    let mut lowered = Module::default();
    let mut sources = HashMap::<FileName, Arc<str>>::new();

    for package_unit in package_units {
        load_package_unit(&mut lowered, &mut sources, &module_lookup, package_unit)?;
    }

    Ok(PackageUniverse {
        lowered,
        local_modules,
        sources,
    })
}

fn build_package_units(local_parsed: ParsedPackage) -> Result<Vec<PackageUnit>, PackageBuildError> {
    let builtin_packages = builtin_packages().packages;
    let core_package = builtin_packages
        .get("core")
        .cloned()
        .expect("core builtins package should always exist");
    let basic_package = builtin_packages.get("basic").cloned();

    let mut units = Vec::new();
    units.push(builtin_package_unit("core", core_package, BTreeMap::new())?);

    if let Some(basic_package) = basic_package {
        let mut deps = BTreeMap::new();
        deps.insert("core".to_string(), PackageId::Builtin("core".to_string()));
        units.push(builtin_package_unit("basic", basic_package, deps)?);
    }

    let mut local_dependencies = BTreeMap::new();
    local_dependencies.insert("core".to_string(), PackageId::Builtin("core".to_string()));
    if builtin_packages.contains_key("basic") {
        local_dependencies.insert("basic".to_string(), PackageId::Builtin("basic".to_string()));
    }

    units.push(PackageUnit {
        id: PackageId::Local,
        parsed: local_parsed,
        dependencies: local_dependencies,
        externals: BTreeMap::new(),
        collect_sources: true,
    });

    Ok(units)
}

fn builtin_package_unit(
    package_name: &str,
    package: BuiltinPackage,
    dependencies: BTreeMap<String, PackageId>,
) -> Result<PackageUnit, PackageBuildError> {
    let parsed = parse_package(&package.root).map_err(PackageBuildError::Load)?;
    Ok(PackageUnit {
        id: PackageId::Builtin(package_name.to_string()),
        parsed,
        dependencies,
        externals: package
            .externals
            .into_iter()
            .map(|(module_path, module)| (canonical_path_from_builtin(module_path), module))
            .collect(),
        collect_sources: false,
    })
}

fn canonical_path_from_builtin(path: BuiltinModulePath) -> CanonicalModulePath {
    CanonicalModulePath {
        directories: path
            .directories
            .into_iter()
            .map(|directory| directory.to_lowercase())
            .collect(),
        module: path.module,
    }
}

fn local_module_paths(package_units: &[PackageUnit]) -> Vec<CanonicalModulePath> {
    package_units
        .iter()
        .find(|unit| unit.id == PackageId::Local)
        .map(|unit| {
            unit.parsed
                .modules
                .iter()
                .map(|module| module.path.clone())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn build_module_lookup(
    package_units: &[PackageUnit],
) -> BTreeMap<AbsoluteModuleLookupKey, CanonicalModulePath> {
    let mut lookup = BTreeMap::new();
    for package_unit in package_units {
        add_package_modules_to_lookup(&mut lookup, package_unit);
    }
    lookup
}

fn add_package_modules_to_lookup(
    lookup: &mut BTreeMap<AbsoluteModuleLookupKey, CanonicalModulePath>,
    package_unit: &PackageUnit,
) {
    for module in &package_unit.parsed.modules {
        lookup.insert(
            module_lookup_key(
                &package_unit.id,
                &module.path.directories,
                &module.path.module,
            ),
            module.path.clone(),
        );
    }
}

fn load_package_unit(
    lowered: &mut Module<Arc<process::Expression<(), Universal>>, Universal>,
    sources: &mut HashMap<FileName, Arc<str>>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, CanonicalModulePath>,
    package_unit: PackageUnit,
) -> Result<(), PackageBuildError> {
    let PackageUnit {
        id,
        parsed,
        dependencies,
        mut externals,
        collect_sources,
    } = package_unit;

    for parsed_module in &parsed.modules {
        let current_module_path =
            resolved_module_path(&parsed_module.path, ResolvedPackageRef::Local);

        for file in &parsed_module.files {
            if collect_sources {
                sources.insert(file.absolute_path.clone().into(), Arc::clone(&file.source));
            }

            let imports = build_file_import_aliases(
                file,
                &current_module_path,
                &id,
                &dependencies,
                module_lookup,
            )?;
            let mut lowered_file = lower(file.source_file.body.clone()).map_err(|error| {
                PackageBuildError::LowerError {
                    path: file.absolute_path.clone(),
                    source: Arc::clone(&file.source),
                    error,
                }
            })?;
            if file.module_part_suffix.is_none() {
                if let Some(external_module) = externals.remove(&parsed_module.path) {
                    merge_module(&mut lowered_file, external_module);
                }
            }
            let resolved_file = resolve_module(
                lowered_file,
                imports,
                &current_module_path,
                Arc::clone(&file.source),
            )?;
            let universal_file = resolve_module_to_universal(
                resolved_file,
                &id,
                &dependencies,
                Arc::clone(&file.source),
            )?;
            merge_module(lowered, universal_file);
        }
    }

    if let Some((module_path, _)) = externals.into_iter().next() {
        return Err(PackageBuildError::UnattachedExternalModule {
            package: id,
            module_path: module_path.to_slash_path(),
        });
    }

    Ok(())
}

fn resolve_module(
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

fn resolve_module_to_universal(
    module: Module<Arc<process::Expression<(), Resolved>>, Resolved>,
    current_package: &PackageId,
    dependencies: &BTreeMap<String, PackageId>,
    file_source: Arc<str>,
) -> Result<Module<Arc<process::Expression<(), Universal>>, Universal>, PackageBuildError> {
    module.map_global_names(|name| {
        resolve_name_to_universal(
            name,
            current_package,
            dependencies,
            Arc::clone(&file_source),
        )
    })
}

fn build_file_import_aliases(
    file: &ParsedPackageFile,
    current_module_path: &ResolvedModulePath,
    current_package: &PackageId,
    current_dependencies: &BTreeMap<String, PackageId>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, CanonicalModulePath>,
) -> Result<BTreeMap<String, ResolvedModulePath>, PackageBuildError> {
    let mut aliases = BTreeMap::new();
    aliases.insert(
        current_module_path.module.clone(),
        current_module_path.clone(),
    );

    for import in &file.source_file.imports {
        let imported_module = resolve_imported_module(
            import,
            file,
            current_package,
            current_dependencies,
            module_lookup,
        )?;
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
    current_package: &PackageId,
    current_dependencies: &BTreeMap<String, PackageId>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, CanonicalModulePath>,
) -> Result<ResolvedModulePath, PackageBuildError> {
    let (resolved_package, absolute_package) = match import.path.dependency.as_deref() {
        None => (ResolvedPackageRef::Local, current_package.clone()),
        Some(alias) => {
            let Some(package_id) = current_dependencies.get(alias).cloned() else {
                return Err(PackageBuildError::UnknownDependency {
                    source: Arc::clone(&file.source),
                    span: import.span.clone(),
                    dependency: alias.to_string(),
                });
            };
            (
                ResolvedPackageRef::Dependency(alias.to_string()),
                package_id,
            )
        }
    };

    let lookup_key = module_lookup_key(
        &absolute_package,
        &import
            .path
            .directories
            .iter()
            .map(|segment| segment.to_lowercase())
            .collect::<Vec<_>>(),
        &import.path.module,
    );

    let canonical = module_lookup.get(&lookup_key).ok_or_else(|| {
        PackageBuildError::ImportedModuleNotFound {
            source: Arc::clone(&file.source),
            span: import.span.clone(),
            import_path: format_import_path(&import.path),
        }
    })?;

    Ok(ResolvedModulePath {
        package: resolved_package,
        directories: canonical.directories.clone(),
        module: canonical.module.clone(),
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
    current_package: &PackageId,
    dependencies: &BTreeMap<String, PackageId>,
    file_source: Arc<str>,
) -> Result<GlobalName<Universal>, PackageBuildError> {
    let package = match &name.module.package {
        ResolvedPackageRef::Local => current_package.clone(),
        ResolvedPackageRef::Dependency(alias) => {
            let Some(package_id) = dependencies.get(alias).cloned() else {
                return Err(PackageBuildError::UnknownDependency {
                    source: file_source,
                    span: name.span.clone(),
                    dependency: alias.clone(),
                });
            };
            package_id
        }
    };

    Ok(GlobalName::new(
        name.span,
        UniversalModulePath {
            package,
            directories: name.module.directories,
            module: name.module.module,
        },
        name.primary,
    ))
}

fn module_lookup_key(
    package: &PackageId,
    directories: &[String],
    module: &str,
) -> AbsoluteModuleLookupKey {
    AbsoluteModuleLookupKey {
        package: package.clone(),
        directories: directories.to_vec(),
        module_lower: module.to_lowercase(),
    }
}

fn resolved_module_path(
    local: &CanonicalModulePath,
    package: ResolvedPackageRef,
) -> ResolvedModulePath {
    ResolvedModulePath {
        package,
        directories: local.directories.clone(),
        module: local.module.clone(),
    }
}

fn merge_module<Expr, S>(target: &mut Module<Expr, S>, mut other: Module<Expr, S>) {
    target.type_defs.append(&mut other.type_defs);
    target.declarations.append(&mut other.declarations);
    target.definitions.append(&mut other.definitions);
}
