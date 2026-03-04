use std::collections::{BTreeMap, btree_map::Entry};
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use par_core::frontend::{
    SourceFile, SyntaxError,
    language::{Expression, Unresolved},
    parse_source_file,
};

const MANIFEST_FILE: &str = "Par.toml";
const SOURCE_DIRECTORY: &str = "src";

#[derive(Debug, Clone)]
pub struct PackageLayout {
    pub src_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub struct LoadedPackageFile {
    pub absolute_path: PathBuf,
    pub relative_path_from_src: PathBuf,
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CanonicalModulePath {
    pub directories: Vec<String>,
    pub module: String,
}

impl CanonicalModulePath {
    pub fn to_slash_path(&self) -> String {
        if self.directories.is_empty() {
            return self.module.clone();
        }
        format!("{}/{}", self.directories.join("/"), self.module)
    }

    fn key(&self) -> CanonicalModulePathKey {
        CanonicalModulePathKey {
            directories: self.directories.clone(),
            module_lower: self.module.to_lowercase(),
        }
    }
}

impl Display for CanonicalModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_slash_path())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedPackageFile {
    pub absolute_path: PathBuf,
    pub relative_path_from_src: PathBuf,
    pub source: Arc<str>,
    pub module_part_suffix: Option<String>,
    pub source_file: SourceFile<Expression<Unresolved>>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub path: CanonicalModulePath,
    pub files: Vec<ParsedPackageFile>,
}

#[derive(Debug, Clone)]
pub struct ParsedPackage {
    pub modules: Vec<ParsedModule>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CanonicalModulePathKey {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

            return Ok(PackageLayout { src_dir });
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
    let mut modules_by_key: BTreeMap<CanonicalModulePathKey, ParsedModule> = BTreeMap::new();

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

        let module_path =
            derive_canonical_module_path(&file.relative_path_from_src, &declared_module_name)?;
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

pub fn load_package(start: impl AsRef<Path>) -> Result<ParsedPackage, PackageLoadError> {
    let layout = find_package_layout(start)?;
    let files = collect_source_files(&layout)?;
    parse_loaded_files(files)
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

fn derive_canonical_module_path(
    relative_path_from_src: &Path,
    declared_module_name: &str,
) -> Result<CanonicalModulePath, PackageLoadError> {
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

    Ok(CanonicalModulePath {
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
