use std::collections::{BTreeMap, BTreeSet, HashMap, btree_map::Entry};
use std::fmt::{self, Display, Formatter, Write};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::frontend::lower;
use crate::frontend::parse_source_file;
use crate::frontend_impl::language::{
    CompileError, GlobalName, PackageId, Resolved, ResolvedPackageRef, Universal, Unresolved,
};
use crate::frontend_impl::parse::SyntaxError;
use crate::frontend_impl::process;
use crate::frontend_impl::program::{
    CheckedModule, ImportDecl, ImportPath, Module, SourceFile, TypeOnHover,
};
use crate::frontend_impl::types::{PrimitiveType, Type, TypeError};
use crate::location::{FileName, Span};
use crate::runtime_impl::{Compiled, RuntimeCompilerError};

pub type ExternalModule = Module<Arc<process::Expression<(), Unresolved>>, Unresolved>;

const MANIFEST_FILE: &str = "Par.toml";
const SOURCE_DIRECTORY: &str = "src";

#[derive(Debug, Clone)]
pub struct PackageLayout {
    pub root_dir: PathBuf,
    pub manifest_path: PathBuf,
    pub src_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub struct LoadedPackageFile {
    pub absolute_path: PathBuf,
    pub relative_path_from_src: PathBuf,
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath {
    pub directories: Vec<String>,
    pub module: String,
}

impl ModulePath {
    pub fn to_slash_path(&self) -> String {
        if self.directories.is_empty() {
            return self.module.clone();
        }
        format!("{}/{}", self.directories.join("/"), self.module)
    }

    fn key(&self) -> ModulePathKey {
        ModulePathKey {
            directories: self.directories.clone(),
            module_lower: self.module.to_lowercase(),
        }
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_slash_path())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPackageFile {
    pub absolute_path: PathBuf,
    pub relative_path_from_src: PathBuf,
    pub source: Arc<str>,
    pub module_part_suffix: Option<String>,
    pub source_file: SourceFile<crate::frontend_impl::language::Expression<Unresolved>>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub path: ModulePath,
    pub files: Vec<ParsedPackageFile>,
}

#[derive(Debug, Clone)]
pub struct ParsedPackage {
    pub modules: Vec<ParsedModule>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ModulePathKey {
    directories: Vec<String>,
    module_lower: String,
}

#[derive(Debug, Clone)]
pub enum PackageLoadError {
    PackageRootNotFound {
        start: PathBuf,
    },
    ManifestReadError {
        path: PathBuf,
        message: String,
    },
    SrcDirectoryMissing {
        path: PathBuf,
    },
    DirectoryReadError {
        path: PathBuf,
        message: String,
    },
    FileReadError {
        path: PathBuf,
        message: String,
    },
    InvalidSourceFilePath {
        path: PathBuf,
    },
    InvalidSourceFileName {
        path: PathBuf,
    },
    ParseError {
        path: PathBuf,
        source: Arc<str>,
        error: SyntaxError,
    },
    MissingModuleDeclaration {
        path: PathBuf,
    },
    FileNameModuleMismatch {
        path: PathBuf,
        declared_module: String,
        file_module_name: String,
    },
    ConflictingModuleNameCasing {
        module_path: String,
        first_path: PathBuf,
        first_declared_name: String,
        second_path: PathBuf,
        second_declared_name: String,
    },
}

impl Display for PackageLoadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::PackageRootNotFound { start } => {
                write!(
                    f,
                    "Could not find `{MANIFEST_FILE}` when searching from {}",
                    start.display()
                )
            }
            Self::ManifestReadError { path, message } => {
                write!(f, "Failed to read manifest {}: {}", path.display(), message)
            }
            Self::SrcDirectoryMissing { path } => {
                write!(
                    f,
                    "Package source directory does not exist: {}",
                    path.display()
                )
            }
            Self::DirectoryReadError { path, message } => {
                write!(
                    f,
                    "Failed to read directory {}: {}",
                    path.display(),
                    message
                )
            }
            Self::FileReadError { path, message } => {
                write!(
                    f,
                    "Failed to read source file {}: {}",
                    path.display(),
                    message
                )
            }
            Self::InvalidSourceFilePath { path } => {
                write!(f, "Source file path is not valid UTF-8: {}", path.display())
            }
            Self::InvalidSourceFileName { path } => {
                write!(
                    f,
                    "Invalid source file name (expected `Module.par` or `Module.*.par`): {}",
                    path.display()
                )
            }
            Self::ParseError { path, .. } => {
                write!(f, "Failed to parse source file {}", path.display())
            }
            Self::MissingModuleDeclaration { path } => {
                write!(
                    f,
                    "Source file is missing `module` declaration: {}",
                    path.display()
                )
            }
            Self::FileNameModuleMismatch {
                path,
                declared_module,
                file_module_name,
            } => {
                write!(
                    f,
                    "Module declaration `{}` does not match source file name `{}` in {}",
                    declared_module,
                    file_module_name,
                    path.display()
                )
            }
            Self::ConflictingModuleNameCasing {
                module_path,
                first_path,
                first_declared_name,
                second_path,
                second_declared_name,
            } => {
                write!(
                    f,
                    "Module `{}` has inconsistent declaration casing (`{}` in {}, `{}` in {})",
                    module_path,
                    first_declared_name,
                    first_path.display(),
                    second_declared_name,
                    second_path.display()
                )
            }
        }
    }
}

impl std::error::Error for PackageLoadError {}

#[derive(Debug, Clone)]
pub enum WorkspaceError {
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
    BindingNameConflictsWithImportAlias {
        source: Arc<str>,
        span: Span,
        name: String,
    },
    UnknownModuleQualifier {
        source: Arc<str>,
        span: Span,
        qualifier: String,
        name: String,
    },
    QualifiedCurrentModuleReference {
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

impl Display for WorkspaceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            Self::BindingNameConflictsWithImportAlias { name, .. } => {
                write!(
                    f,
                    "Top-level binding `{}` conflicts with an import alias",
                    name
                )
            }
            Self::UnknownModuleQualifier {
                qualifier, name, ..
            } => write!(
                f,
                "Unknown module qualifier `{}` in reference `{}`",
                qualifier, name
            ),
            Self::QualifiedCurrentModuleReference {
                qualifier, name, ..
            } => write!(
                f,
                "Reference `{}` uses qualifier `{}` for current module; use unqualified name",
                name, qualifier
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

impl std::error::Error for WorkspaceError {}

#[derive(Debug, Clone)]
pub struct WorkspacePackage {
    pub id: PackageId,
    pub parsed: ParsedPackage,
    pub dependencies: BTreeMap<String, PackageId>,
    pub externals: BTreeMap<ModulePath, ExternalModule>,
    pub collect_sources: bool,
}

impl WorkspacePackage {
    pub fn new(id: PackageId, parsed: ParsedPackage) -> Self {
        Self {
            id,
            parsed,
            dependencies: BTreeMap::new(),
            externals: BTreeMap::new(),
            collect_sources: true,
        }
    }

    pub fn from_parsed(id: PackageId, parsed: ParsedPackage) -> Self {
        Self::new(id, parsed)
    }

    pub fn from_path(id: PackageId, start: impl AsRef<Path>) -> Result<Self, PackageLoadError> {
        parse_package(start).map(|parsed| Self::new(id, parsed))
    }

    pub fn with_dependency(mut self, alias: impl Into<String>, package: PackageId) -> Self {
        self.dependencies.insert(alias.into(), package);
        self
    }

    pub fn with_dependencies(
        mut self,
        dependencies: impl IntoIterator<Item = (impl Into<String>, PackageId)>,
    ) -> Self {
        for (alias, package) in dependencies {
            self.dependencies.insert(alias.into(), package);
        }
        self
    }

    pub fn with_external(mut self, module_path: ModulePath, module: ExternalModule) -> Self {
        self.externals.insert(module_path, module);
        self
    }

    pub fn with_externals(
        mut self,
        externals: impl IntoIterator<Item = (ModulePath, ExternalModule)>,
    ) -> Self {
        for (module_path, module) in externals {
            self.externals.insert(module_path, module);
        }
        self
    }

    pub fn collect_sources(mut self, collect_sources: bool) -> Self {
        self.collect_sources = collect_sources;
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct WorkspaceInput {
    packages: Vec<WorkspacePackage>,
    root_packages: Vec<PackageId>,
}

impl WorkspaceInput {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_root_package(mut self, package: WorkspacePackage) -> Self {
        self.root_packages.push(package.id.clone());
        self.packages.push(package);
        self
    }

    pub fn add_root_package(&mut self, package: WorkspacePackage) {
        self.root_packages.push(package.id.clone());
        self.packages.push(package);
    }

    pub fn with_package(mut self, package: WorkspacePackage) -> Self {
        self.packages.push(package);
        self
    }

    pub fn add_package(&mut self, package: WorkspacePackage) {
        self.packages.push(package);
    }

    pub fn build(self) -> Result<Workspace, WorkspaceError> {
        load_workspace(self)
    }
}

#[derive(Debug, Clone)]
pub struct FileImportScope<S> {
    pub current_module: S,
    pub aliases: BTreeMap<String, S>,
}

#[derive(Debug, Clone)]
pub struct Workspace {
    lowered: Module<Arc<process::Expression<(), Universal>>, Universal>,
    package_modules: BTreeMap<PackageId, Vec<ModulePath>>,
    root_packages: Vec<PackageId>,
    file_scopes: HashMap<FileName, FileImportScope<Universal>>,
    pub sources: HashMap<FileName, Arc<str>>,
}

impl Workspace {
    pub fn lowered_module(&self) -> &Module<Arc<process::Expression<(), Universal>>, Universal> {
        &self.lowered
    }

    pub fn root_packages(&self) -> &[PackageId] {
        &self.root_packages
    }

    pub fn root_modules(&self) -> Vec<ModulePath> {
        self.root_packages
            .iter()
            .flat_map(|package| self.modules_in_package(package).iter().cloned())
            .collect()
    }

    pub fn modules_in_package(&self, package: &PackageId) -> &[ModulePath] {
        self.package_modules
            .get(package)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    pub fn import_scope(&self, file: &FileName) -> Option<&FileImportScope<Universal>> {
        self.file_scopes.get(file)
    }

    pub fn type_check(&self) -> Result<CheckedWorkspace, TypeError<Universal>> {
        let checked = self.lowered.type_check()?;
        Ok(CheckedWorkspace::new(
            checked,
            self.package_modules.clone(),
            self.root_packages.clone(),
            self.file_scopes.clone(),
            self.sources.clone(),
        ))
    }
}

#[derive(Clone)]
pub struct CheckedWorkspace {
    checked: CheckedModule<Universal>,
    package_modules: BTreeMap<PackageId, Vec<ModulePath>>,
    root_packages: Vec<PackageId>,
    file_scopes: HashMap<FileName, FileImportScope<Universal>>,
    pub sources: HashMap<FileName, Arc<str>>,
    type_on_hover: TypeOnHover<Universal>,
}

impl CheckedWorkspace {
    fn new(
        checked: CheckedModule<Universal>,
        package_modules: BTreeMap<PackageId, Vec<ModulePath>>,
        root_packages: Vec<PackageId>,
        file_scopes: HashMap<FileName, FileImportScope<Universal>>,
        sources: HashMap<FileName, Arc<str>>,
    ) -> Self {
        let type_on_hover = TypeOnHover::new(&checked);
        Self {
            checked,
            package_modules,
            root_packages,
            file_scopes,
            sources,
            type_on_hover,
        }
    }

    pub fn checked_module(&self) -> &CheckedModule<Universal> {
        &self.checked
    }

    pub fn root_packages(&self) -> &[PackageId] {
        &self.root_packages
    }

    pub fn root_modules(&self) -> Vec<ModulePath> {
        self.root_packages
            .iter()
            .flat_map(|package| self.modules_in_package(package).iter().cloned())
            .collect()
    }

    pub fn modules_in_package(&self, package: &PackageId) -> &[ModulePath] {
        self.package_modules
            .get(package)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    pub fn import_scope(&self, file: &FileName) -> Option<&FileImportScope<Universal>> {
        self.file_scopes.get(file)
    }

    pub fn type_on_hover(&self) -> &TypeOnHover<Universal> {
        &self.type_on_hover
    }

    pub fn hover_at(
        &self,
        file: &FileName,
        row: u32,
        column: u32,
    ) -> Option<process::NameWithType<Universal>> {
        self.type_on_hover.query(file, row, column)
    }

    pub fn compile_runtime(&self, max_interactions: u32) -> Result<Compiled, RuntimeCompilerError> {
        Compiled::compile_file(&self.checked, max_interactions)
    }

    pub fn render_global_in_file(&self, file: &FileName, name: &GlobalName<Universal>) -> String {
        let mut output = String::new();
        let _ = write_global_name_in_file(&mut output, self.import_scope(file), name);
        output
    }

    pub fn render_type_in_file(&self, file: &FileName, typ: &Type<Universal>) -> String {
        let mut output = String::new();
        let _ = write_type_in_file(&mut output, self.import_scope(file), typ);
        output
    }

    pub fn render_hover_in_file(
        &self,
        file: &FileName,
        hover: &process::NameWithType<Universal>,
    ) -> String {
        let mut output = String::new();
        if let Some(global_name) = hover.global_name.as_ref() {
            let _ = write_global_name_in_file(&mut output, self.import_scope(file), global_name);
            let _ = write!(output, ": ");
        } else if let Some(name) = hover.name.as_ref() {
            let _ = write!(output, "{}: ", name);
        }
        let _ = write_type_in_file(&mut output, self.import_scope(file), &hover.typ);
        output
    }
}

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
    externals: BTreeMap<ModulePath, ExternalModule>,
    collect_sources: bool,
}

type ResolvedModulePath = Resolved;
type UniversalModulePath = Universal;

pub fn load_workspace(input: WorkspaceInput) -> Result<Workspace, WorkspaceError> {
    let package_units = input
        .packages
        .into_iter()
        .map(|package| PackageUnit {
            id: package.id,
            parsed: package.parsed,
            dependencies: package.dependencies,
            externals: package.externals,
            collect_sources: package.collect_sources,
        })
        .collect::<Vec<_>>();
    let module_lookup = build_module_lookup(&package_units);
    let package_modules = package_module_paths(&package_units);

    let mut lowered = Module::default();
    let mut sources = HashMap::<FileName, Arc<str>>::new();
    let mut file_scopes = HashMap::<FileName, FileImportScope<Universal>>::new();

    for package_unit in package_units {
        load_package_unit(
            &mut lowered,
            &mut sources,
            &mut file_scopes,
            &module_lookup,
            package_unit,
        )?;
    }

    Ok(Workspace {
        lowered,
        package_modules,
        root_packages: input.root_packages,
        file_scopes,
        sources,
    })
}

pub fn find_package_layout(start: impl AsRef<Path>) -> Result<PackageLayout, PackageLoadError> {
    let start = start.as_ref();
    let mut cursor = if start.is_dir() {
        start.to_path_buf()
    } else {
        start
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."))
    };
    let original_start = start.to_path_buf();

    loop {
        let manifest_path = cursor.join(MANIFEST_FILE);
        if manifest_path.exists() {
            let manifest_read_result = fs::read_to_string(&manifest_path);
            if let Err(error) = manifest_read_result {
                return Err(PackageLoadError::ManifestReadError {
                    path: manifest_path,
                    message: error.to_string(),
                });
            }

            let src_dir = cursor.join(SOURCE_DIRECTORY);
            if !src_dir.is_dir() {
                return Err(PackageLoadError::SrcDirectoryMissing { path: src_dir });
            }

            return Ok(PackageLayout {
                root_dir: cursor,
                manifest_path,
                src_dir,
            });
        }

        if !cursor.pop() {
            return Err(PackageLoadError::PackageRootNotFound {
                start: original_start,
            });
        }
    }
}

pub fn collect_source_files(
    layout: &PackageLayout,
) -> Result<Vec<LoadedPackageFile>, PackageLoadError> {
    let mut files = Vec::new();
    collect_source_files_recursive(&layout.src_dir, &layout.src_dir, &mut files)?;
    files.sort_by(|a, b| a.relative_path_from_src.cmp(&b.relative_path_from_src));
    Ok(files)
}

pub fn parse_loaded_files(
    files: Vec<LoadedPackageFile>,
) -> Result<ParsedPackage, PackageLoadError> {
    let mut modules_by_key: BTreeMap<ModulePathKey, ParsedModule> = BTreeMap::new();

    for file in files {
        let (file_module_name, module_part_suffix) =
            parse_module_name_from_file_name(&file.relative_path_from_src)?;
        let source: Arc<str> = Arc::from(file.source.as_str());
        let source_file = parse_source_file(&file.source, file.absolute_path.clone().into())
            .map_err(|error| PackageLoadError::ParseError {
                path: file.absolute_path.clone(),
                source: Arc::clone(&source),
                error,
            })?;

        let declared_module_name = source_file
            .module_decl
            .as_ref()
            .map(|module_decl| module_decl.name.clone())
            .ok_or_else(|| PackageLoadError::MissingModuleDeclaration {
                path: file.absolute_path.clone(),
            })?;

        if !declared_module_name.eq_ignore_ascii_case(&file_module_name) {
            return Err(PackageLoadError::FileNameModuleMismatch {
                path: file.absolute_path.clone(),
                declared_module: declared_module_name,
                file_module_name,
            });
        }

        let module_path = derive_module_path(&file.relative_path_from_src, &declared_module_name)?;
        let path_key = module_path.key();

        let parsed_file = ParsedPackageFile {
            absolute_path: file.absolute_path,
            relative_path_from_src: file.relative_path_from_src,
            source,
            module_part_suffix,
            source_file,
        };

        match modules_by_key.entry(path_key) {
            Entry::Vacant(vacant) => {
                vacant.insert(ParsedModule {
                    path: module_path,
                    files: vec![parsed_file],
                });
            }
            Entry::Occupied(mut occupied) => {
                let existing_module = occupied.get_mut();
                if !existing_module.path.module.eq(&module_path.module) {
                    let first_file = &existing_module.files[0];
                    return Err(PackageLoadError::ConflictingModuleNameCasing {
                        module_path: existing_module.path.to_slash_path(),
                        first_path: first_file.absolute_path.clone(),
                        first_declared_name: existing_module.path.module.clone(),
                        second_path: parsed_file.absolute_path.clone(),
                        second_declared_name: module_path.module.clone(),
                    });
                }
                existing_module.files.push(parsed_file);
            }
        }
    }

    let mut modules: Vec<ParsedModule> = modules_by_key.into_values().collect();
    for module in &mut modules {
        module.files.sort_by(|left, right| {
            module_file_ordering_key(left).cmp(&module_file_ordering_key(right))
        });
    }
    modules.sort_by(|left, right| left.path.cmp(&right.path));

    Ok(ParsedPackage { modules })
}

pub fn parse_package(start: impl AsRef<Path>) -> Result<ParsedPackage, PackageLoadError> {
    let layout = find_package_layout(start)?;
    let files = collect_source_files(&layout)?;
    parse_loaded_files(files)
}

fn build_module_lookup(
    package_units: &[PackageUnit],
) -> BTreeMap<AbsoluteModuleLookupKey, ModulePath> {
    let mut lookup = BTreeMap::new();
    for package_unit in package_units {
        add_package_modules_to_lookup(&mut lookup, package_unit);
    }
    lookup
}

fn add_package_modules_to_lookup(
    lookup: &mut BTreeMap<AbsoluteModuleLookupKey, ModulePath>,
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

fn package_module_paths(package_units: &[PackageUnit]) -> BTreeMap<PackageId, Vec<ModulePath>> {
    package_units
        .iter()
        .map(|unit| {
            (
                unit.id.clone(),
                unit.parsed
                    .modules
                    .iter()
                    .map(|module| module.path.clone())
                    .collect(),
            )
        })
        .collect()
}

fn load_package_unit(
    lowered: &mut Module<Arc<process::Expression<(), Universal>>, Universal>,
    sources: &mut HashMap<FileName, Arc<str>>,
    file_scopes: &mut HashMap<FileName, FileImportScope<Universal>>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, ModulePath>,
    package_unit: PackageUnit,
) -> Result<(), WorkspaceError> {
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
            let file_scope = universalize_file_scope(
                &imports,
                &current_module_path,
                &id,
                &dependencies,
                Arc::clone(&file.source),
            )?;
            file_scopes.insert(file.absolute_path.clone().into(), file_scope);

            let imported_aliases = imported_aliases(&imports, &current_module_path);
            let mut lowered_file = lower(file.source_file.body.clone()).map_err(|error| {
                WorkspaceError::LowerError {
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
            validate_binding_names(&lowered_file, &imported_aliases, Arc::clone(&file.source))?;
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
        return Err(WorkspaceError::UnattachedExternalModule {
            package: id,
            module_path: module_path.to_slash_path(),
        });
    }

    Ok(())
}

fn build_file_import_aliases(
    file: &ParsedPackageFile,
    current_module_path: &ResolvedModulePath,
    current_package: &PackageId,
    current_dependencies: &BTreeMap<String, PackageId>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, ModulePath>,
) -> Result<BTreeMap<String, ResolvedModulePath>, WorkspaceError> {
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
                return Err(WorkspaceError::DuplicateImportAlias {
                    source: Arc::clone(&file.source),
                    span: import.span.clone(),
                    alias,
                });
            }
        }
    }

    Ok(aliases)
}

fn universalize_file_scope(
    aliases: &BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
    current_package: &PackageId,
    dependencies: &BTreeMap<String, PackageId>,
    source: Arc<str>,
) -> Result<FileImportScope<Universal>, WorkspaceError> {
    let mut universal_aliases = BTreeMap::new();
    for (alias, module) in aliases {
        universal_aliases.insert(
            alias.clone(),
            universalize_module_path(module, current_package, dependencies, Arc::clone(&source))?,
        );
    }
    Ok(FileImportScope {
        current_module: universalize_module_path(
            current_module_path,
            current_package,
            dependencies,
            source,
        )?,
        aliases: universal_aliases,
    })
}

fn universalize_module_path(
    module: &ResolvedModulePath,
    current_package: &PackageId,
    dependencies: &BTreeMap<String, PackageId>,
    file_source: Arc<str>,
) -> Result<UniversalModulePath, WorkspaceError> {
    let package = match &module.package {
        ResolvedPackageRef::Local => current_package.clone(),
        ResolvedPackageRef::Dependency(alias) => {
            let Some(package_id) = dependencies.get(alias).cloned() else {
                return Err(WorkspaceError::UnknownDependency {
                    source: file_source,
                    span: Span::None,
                    dependency: alias.clone(),
                });
            };
            package_id
        }
    };

    Ok(UniversalModulePath {
        package,
        directories: module.directories.clone(),
        module: module.module.clone(),
    })
}

fn imported_aliases(
    aliases: &BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
) -> BTreeSet<String> {
    let mut imported = aliases.keys().cloned().collect::<BTreeSet<_>>();
    imported.remove(&current_module_path.module);
    imported
}

fn validate_binding_names(
    module: &Module<Arc<process::Expression<(), Unresolved>>, Unresolved>,
    imported_aliases: &BTreeSet<String>,
    source: Arc<str>,
) -> Result<(), WorkspaceError> {
    for type_def in &module.type_defs {
        if imported_aliases.contains(type_def.name.primary.as_str()) {
            return Err(WorkspaceError::BindingNameConflictsWithImportAlias {
                source,
                span: type_def.name.span.clone(),
                name: type_def.name.primary.clone(),
            });
        }
    }
    for declaration in &module.declarations {
        if imported_aliases.contains(declaration.name.primary.as_str()) {
            return Err(WorkspaceError::BindingNameConflictsWithImportAlias {
                source: Arc::clone(&source),
                span: declaration.name.span.clone(),
                name: declaration.name.primary.clone(),
            });
        }
    }
    for definition in &module.definitions {
        if imported_aliases.contains(definition.name.primary.as_str()) {
            return Err(WorkspaceError::BindingNameConflictsWithImportAlias {
                source: Arc::clone(&source),
                span: definition.name.span.clone(),
                name: definition.name.primary.clone(),
            });
        }
    }
    Ok(())
}

fn resolve_imported_module(
    import: &ImportDecl,
    file: &ParsedPackageFile,
    current_package: &PackageId,
    current_dependencies: &BTreeMap<String, PackageId>,
    module_lookup: &BTreeMap<AbsoluteModuleLookupKey, ModulePath>,
) -> Result<ResolvedModulePath, WorkspaceError> {
    let (resolved_package, absolute_package) = match import.path.dependency.as_deref() {
        None => (ResolvedPackageRef::Local, current_package.clone()),
        Some(alias) => {
            let Some(package_id) = current_dependencies.get(alias).cloned() else {
                return Err(WorkspaceError::UnknownDependency {
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

    let canonical =
        module_lookup
            .get(&lookup_key)
            .ok_or_else(|| WorkspaceError::ImportedModuleNotFound {
                source: Arc::clone(&file.source),
                span: import.span.clone(),
                import_path: format_import_path(&import.path),
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

fn resolve_module(
    module: Module<Arc<process::Expression<(), Unresolved>>, Unresolved>,
    imports: BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
    file_source: Arc<str>,
) -> Result<Module<Arc<process::Expression<(), Resolved>>, Resolved>, WorkspaceError> {
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
) -> Result<Module<Arc<process::Expression<(), Universal>>, Universal>, WorkspaceError> {
    module.map_global_names(|name| {
        resolve_name_to_universal(
            name,
            current_package,
            dependencies,
            Arc::clone(&file_source),
        )
    })
}

fn resolve_name_to_resolved(
    mut name: GlobalName<Unresolved>,
    imports: &BTreeMap<String, ResolvedModulePath>,
    current_module_path: &ResolvedModulePath,
    file_source: Arc<str>,
) -> Result<GlobalName<Resolved>, WorkspaceError> {
    if let Some(module_qualifier) = name.module.qualifier.take() {
        let Some(target_module) = imports.get(module_qualifier.as_str()) else {
            return Err(WorkspaceError::UnknownModuleQualifier {
                source: file_source,
                span: name.span.clone(),
                qualifier: module_qualifier.clone(),
                name: format!("{}.{}", module_qualifier, name.primary),
            });
        };

        if target_module == current_module_path {
            return Err(WorkspaceError::QualifiedCurrentModuleReference {
                source: file_source,
                span: name.span.clone(),
                qualifier: module_qualifier.clone(),
                name: format!("{}.{}", module_qualifier, name.primary),
            });
        }

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
) -> Result<GlobalName<Universal>, WorkspaceError> {
    let package = match &name.module.package {
        ResolvedPackageRef::Local => current_package.clone(),
        ResolvedPackageRef::Dependency(alias) => {
            let Some(package_id) = dependencies.get(alias).cloned() else {
                return Err(WorkspaceError::UnknownDependency {
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

fn resolved_module_path(local: &ModulePath, package: ResolvedPackageRef) -> ResolvedModulePath {
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

fn parse_module_name_from_file_name(
    relative_path_from_src: &Path,
) -> Result<(String, Option<String>), PackageLoadError> {
    let file_name = relative_path_from_src
        .file_name()
        .ok_or_else(|| PackageLoadError::InvalidSourceFileName {
            path: relative_path_from_src.to_path_buf(),
        })?
        .to_str()
        .ok_or_else(|| PackageLoadError::InvalidSourceFilePath {
            path: relative_path_from_src.to_path_buf(),
        })?;

    let module_file_stem =
        file_name
            .strip_suffix(".par")
            .ok_or_else(|| PackageLoadError::InvalidSourceFileName {
                path: relative_path_from_src.to_path_buf(),
            })?;

    let mut segments = module_file_stem.split('.');
    let module_name = segments
        .next()
        .ok_or_else(|| PackageLoadError::InvalidSourceFileName {
            path: relative_path_from_src.to_path_buf(),
        })?;

    if module_name.is_empty() {
        return Err(PackageLoadError::InvalidSourceFileName {
            path: relative_path_from_src.to_path_buf(),
        });
    }

    let mut suffix_segments = Vec::new();
    for segment in segments {
        if segment.is_empty() {
            return Err(PackageLoadError::InvalidSourceFileName {
                path: relative_path_from_src.to_path_buf(),
            });
        }
        suffix_segments.push(segment.to_string());
    }

    let suffix = if suffix_segments.is_empty() {
        None
    } else {
        Some(suffix_segments.join("."))
    };

    Ok((module_name.to_string(), suffix))
}

fn derive_module_path(
    relative_path_from_src: &Path,
    declared_module_name: &str,
) -> Result<ModulePath, PackageLoadError> {
    let mut directories = Vec::new();
    if let Some(parent) = relative_path_from_src.parent() {
        for component in parent.components() {
            let component = component.as_os_str().to_str().ok_or_else(|| {
                PackageLoadError::InvalidSourceFilePath {
                    path: relative_path_from_src.to_path_buf(),
                }
            })?;
            directories.push(component.to_lowercase());
        }
    }

    Ok(ModulePath {
        directories,
        module: declared_module_name.to_string(),
    })
}

fn module_file_ordering_key(file: &ParsedPackageFile) -> (u8, String, PathBuf) {
    match &file.module_part_suffix {
        None => (0, String::new(), file.relative_path_from_src.clone()),
        Some(suffix) => (1, suffix.clone(), file.relative_path_from_src.clone()),
    }
}

fn collect_source_files_recursive(
    src_root: &Path,
    current_dir: &Path,
    files: &mut Vec<LoadedPackageFile>,
) -> Result<(), PackageLoadError> {
    let entries =
        fs::read_dir(current_dir).map_err(|error| PackageLoadError::DirectoryReadError {
            path: current_dir.to_path_buf(),
            message: error.to_string(),
        })?;

    for entry in entries {
        let entry = entry.map_err(|error| PackageLoadError::DirectoryReadError {
            path: current_dir.to_path_buf(),
            message: error.to_string(),
        })?;
        let path = entry.path();
        if path.is_dir() {
            collect_source_files_recursive(src_root, &path, files)?;
            continue;
        }

        if path.extension().and_then(|ext| ext.to_str()) != Some("par") {
            continue;
        }

        let source =
            fs::read_to_string(&path).map_err(|error| PackageLoadError::FileReadError {
                path: path.clone(),
                message: error.to_string(),
            })?;

        let relative_path_from_src = path
            .strip_prefix(src_root)
            .map(Path::to_path_buf)
            .unwrap_or_else(|_| path.clone());

        files.push(LoadedPackageFile {
            absolute_path: path,
            relative_path_from_src,
            source,
        });
    }
    Ok(())
}

fn write_global_name_in_file(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    name: &GlobalName<Universal>,
) -> fmt::Result {
    if let Some(scope) = scope {
        if name.module == scope.current_module {
            return write!(f, "{}", name.primary);
        }

        for (alias, module) in &scope.aliases {
            if module == &name.module {
                if alias == &name.primary && alias == &module.module {
                    return write!(f, "{alias}");
                }
                if name.primary == module.module {
                    return write!(f, "{alias}");
                }
                return write!(f, "{}.{}", alias, name.primary);
            }
        }
    }

    let module = name.module.to_string();
    if module.is_empty() {
        write!(f, "{}", name.primary)
    } else {
        write!(f, "{}.{}", module, name.primary)
    }
}

fn write_type_in_file(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    typ: &Type<Universal>,
) -> fmt::Result {
    write_type_in_file_with_indent(f, scope, typ, 0)
}

fn write_type_in_file_with_indent(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    typ: &Type<Universal>,
    indent: usize,
) -> fmt::Result {
    match typ {
        Type::Primitive(_, primitive) => write_primitive_type(f, primitive.clone()),
        Type::DualPrimitive(_, primitive) => {
            write!(f, "dual ")?;
            write_primitive_type(f, primitive.clone())
        }
        Type::Var(_, name) => write!(f, "{name}"),
        Type::DualVar(_, name) => write!(f, "dual {name}"),
        Type::Name(_, name, args) => {
            write_global_name_in_file(f, scope, name)?;
            write_type_args(f, scope, args, indent)
        }
        Type::DualName(_, name, args) => {
            write!(f, "dual ")?;
            write_global_name_in_file(f, scope, name)?;
            write_type_args(f, scope, args, indent)
        }
        Type::Box(_, body) => {
            write!(f, "box ")?;
            write_type_in_file_with_indent(f, scope, body, indent)
        }
        Type::DualBox(_, body) => {
            write!(f, "dual box ")?;
            write_type_in_file_with_indent(f, scope, body, indent)
        }
        Type::Pair(_, arg, then, vars) => {
            write_pair_like(f, scope, "(", ")", arg, then, vars, false, indent)
        }
        Type::Function(_, arg, then, vars) => {
            write_pair_like(f, scope, "[", "]", arg, then, vars, true, indent)
        }
        Type::Either(_, branches) => {
            write_braced_branches(f, scope, "either", branches, indent, false)
        }
        Type::Choice(_, branches) => {
            write_braced_branches(f, scope, "choice", branches, indent, true)
        }
        Type::Break(_) => write!(f, "!"),
        Type::Continue(_) => write!(f, "?"),
        Type::Recursive { label, body, .. } => {
            write!(f, "recursive")?;
            if let Some(label) = label {
                write!(f, "@{label}")?;
            }
            write!(f, " ")?;
            write_type_in_file_with_indent(f, scope, body, indent)
        }
        Type::Iterative { label, body, .. } => {
            write!(f, "iterative")?;
            if let Some(label) = label {
                write!(f, "@{label}")?;
            }
            write!(f, " ")?;
            write_type_in_file_with_indent(f, scope, body, indent)
        }
        Type::Self_(_, label) => {
            write!(f, "self")?;
            if let Some(label) = label {
                write!(f, "@{label}")?;
            }
            Ok(())
        }
        Type::DualSelf(_, label) => {
            write!(f, "dual self")?;
            if let Some(label) = label {
                write!(f, "@{label}")?;
            }
            Ok(())
        }
        Type::Exists(_, name, then) => {
            write_quantified_type(f, scope, "(", ")", "type", name, then, indent)
        }
        Type::Forall(_, name, then) => {
            write_quantified_type(f, scope, "[", "]", "type", name, then, indent)
        }
        Type::Hole(_, name, _) => write!(f, "%{name}"),
        Type::DualHole(_, name, _) => write!(f, "dual %{name}"),
    }
}

fn write_primitive_type(f: &mut impl Write, primitive: PrimitiveType) -> fmt::Result {
    let text = match primitive {
        PrimitiveType::Nat => "Nat",
        PrimitiveType::Int => "Int",
        PrimitiveType::String => "String",
        PrimitiveType::Char => "Char",
        PrimitiveType::Byte => "Byte",
        PrimitiveType::Bytes => "Bytes",
    };
    write!(f, "{text}")
}

fn write_type_args(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    args: &[Type<Universal>],
    indent: usize,
) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }
    write!(f, "<")?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write_type_in_file_with_indent(f, scope, arg, indent)?;
    }
    write!(f, ">")
}

fn write_pair_like(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    open: &str,
    close: &str,
    arg: &Type<Universal>,
    then: &Type<Universal>,
    vars: &[crate::frontend_impl::language::LocalName],
    function: bool,
    indent: usize,
) -> fmt::Result {
    let mut then = then;
    if !vars.is_empty() {
        write!(f, "<")?;
        write!(f, "{}", vars[0])?;
        for var in vars.iter().skip(1) {
            write!(f, ", {}", var)?;
        }
        write!(f, ">")?;
        write!(f, "{open}")?;
        write_type_in_file_with_indent(f, scope, arg, indent)?;
    } else {
        write!(f, "{open}")?;
        write_type_in_file_with_indent(f, scope, arg, indent)?;
        while let Some((next_arg, next_then, next_vars)) = if function {
            match then {
                Type::Function(_, next_arg, next_then, next_vars) => {
                    Some((next_arg.as_ref(), next_then.as_ref(), next_vars.as_slice()))
                }
                _ => None,
            }
        } else {
            match then {
                Type::Pair(_, next_arg, next_then, next_vars) => {
                    Some((next_arg.as_ref(), next_then.as_ref(), next_vars.as_slice()))
                }
                _ => None,
            }
        } {
            if !next_vars.is_empty() {
                break;
            }
            write!(f, ", ")?;
            write_type_in_file_with_indent(f, scope, next_arg, indent)?;
            then = next_then;
        }
    }

    let is_terminal = if function {
        matches!(then, Type::Continue(_))
    } else {
        matches!(then, Type::Break(_))
    };
    if is_terminal {
        if function {
            write!(f, "{close}?")
        } else {
            write!(f, "{close}!")
        }
    } else {
        write!(f, "{close} ")?;
        write_type_in_file_with_indent(f, scope, then, indent)
    }
}

fn write_quantified_type(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    open: &str,
    close: &str,
    prefix: &str,
    name: &crate::frontend_impl::language::LocalName,
    then: &Type<Universal>,
    indent: usize,
) -> fmt::Result {
    let mut then = then;
    write!(f, "{open}{prefix} {name}")?;
    loop {
        match then {
            Type::Exists(_, next_name, next_then) if open == "(" => {
                write!(f, ", {next_name}")?;
                then = next_then;
            }
            Type::Forall(_, next_name, next_then) if open == "[" => {
                write!(f, ", {next_name}")?;
                then = next_then;
            }
            _ => break,
        }
    }
    write!(f, "{close} ")?;
    write_type_in_file_with_indent(f, scope, then, indent)
}

fn write_braced_branches(
    f: &mut impl Write,
    scope: Option<&FileImportScope<Universal>>,
    prefix: &str,
    branches: &std::collections::BTreeMap<
        crate::frontend_impl::language::LocalName,
        Type<Universal>,
    >,
    indent: usize,
    choice: bool,
) -> fmt::Result {
    if branches.is_empty() {
        return write!(f, "{prefix} {{}}");
    }

    write!(f, "{prefix} {{")?;
    for (branch, branch_type) in branches {
        write_indentation(f, indent + 1)?;
        if choice {
            write!(f, ".{} => ", branch)?;
        } else {
            write!(f, ".{} ", branch)?;
        }
        write_type_in_file_with_indent(f, scope, branch_type, indent + 1)?;
        write!(f, ",")?;
    }
    write_indentation(f, indent)?;
    write!(f, "}}")
}

fn write_indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
