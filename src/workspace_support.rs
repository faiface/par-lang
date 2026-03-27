use std::path::{Path, PathBuf};

use crate::package_utils::{SourceLookup, source_for_type_error};
use arcstr::literal;
use par_builtin::builtin_packages;
use par_core::frontend::{
    TypeError,
    language::{PackageId, Universal},
};
use par_core::runtime::{Compiled, RuntimeCompilerError};
use par_core::source::FileName;
use par_core::workspace::{
    CheckedWorkspace, FileImportScope, LoadedPackageFile, ParsedPackage, SourceOverrides,
    Workspace, WorkspaceDiscoveryError, WorkspacePackage, WorkspacePackages, assemble_workspace,
    discover_workspace_packages_from_path, parse_loaded_files,
};
use par_runtime::linker::{Linked, Unlinked};

#[derive(Debug, Clone)]
pub(crate) enum WorkspaceBuildError {
    Discovery(WorkspaceDiscoveryError),
    Workspace(par_core::workspace::WorkspaceError),
}

#[derive(Debug, Clone)]
pub(crate) struct ScopedTypeError {
    pub error: TypeError<Universal>,
    pub file_scope: Option<FileImportScope<Universal>>,
}

impl ScopedTypeError {
    fn from_workspace(workspace: &Workspace, error: TypeError<Universal>) -> Self {
        Self {
            file_scope: error
                .spans()
                .0
                .file()
                .and_then(|file| workspace.import_scope(&file).cloned()),
            error,
        }
    }

    pub(crate) fn to_report(&self, sources: &SourceLookup) -> miette::Report {
        self.error.to_report(
            source_for_type_error(&self.error, sources),
            self.file_scope.as_ref(),
        )
    }
}

#[derive(Clone)]
pub(crate) struct CheckedWorkspaceBuild {
    pub checked: CheckedWorkspace,
    pub sources: SourceLookup,
    pub type_errors: Vec<ScopedTypeError>,
}

impl CheckedWorkspaceBuild {
    fn from_workspace(workspace: Workspace) -> Self {
        let sources = workspace.sources().clone();
        let (checked, type_errors) = workspace.type_check();
        Self {
            checked,
            sources,
            type_errors: type_errors
                .into_iter()
                .map(|error| ScopedTypeError::from_workspace(&workspace, error))
                .collect(),
        }
    }

    pub(crate) fn compile_unlinked(
        self,
        max_interactions: u32,
    ) -> Result<
        (CheckedWorkspace, Compiled<Unlinked>, SourceLookup),
        (CheckedWorkspace, RuntimeCompilerError),
    > {
        let Self {
            checked,
            sources,
            type_errors: _,
        } = self;
        match checked.compile_runtime(max_interactions) {
            Ok(compiled) => Ok((checked, compiled, sources)),
            Err(error) => Err((checked, error)),
        }
    }

    pub(crate) fn compile_linked(
        self,
        max_interactions: u32,
    ) -> Result<
        (CheckedWorkspace, Compiled<Linked>, SourceLookup),
        (CheckedWorkspace, RuntimeCompilerError),
    > {
        let (checked, compiled, sources) = self.compile_unlinked(max_interactions)?;
        match compiled.link() {
            Ok(compiled) => Ok((checked, compiled, sources)),
            Err(error) => Err((checked, error)),
        }
    }
}

pub fn default_workspace_packages_from_path(
    start: impl AsRef<Path>,
    overrides: Option<&SourceOverrides>,
) -> Result<WorkspacePackages, WorkspaceDiscoveryError> {
    let discovered = discover_workspace_packages_from_path(start, overrides)?;
    inject_builtin_packages(discovered)
}

pub fn default_workspace_packages_from_parsed(local: ParsedPackage) -> WorkspacePackages {
    inject_builtin_packages(WorkspacePackages {
        root_package: PackageId::Special(literal!("__synthetic__")),
        packages: vec![WorkspacePackage::new(
            PackageId::Special(literal!("__synthetic__")),
            local,
        )],
    })
    .expect("synthetic workspace should not conflict with builtin aliases")
}

pub fn assemble_default_workspace(
    workspace_packages: WorkspacePackages,
) -> Result<Workspace, par_core::workspace::WorkspaceError> {
    assemble_workspace(workspace_packages)
}

pub(crate) fn checked_workspace_from_path(
    start: impl AsRef<Path>,
    overrides: Option<&SourceOverrides>,
) -> Result<CheckedWorkspaceBuild, WorkspaceBuildError> {
    let packages =
        default_workspace_packages_from_path(start, overrides).map_err(WorkspaceBuildError::Discovery)?;
    let workspace = assemble_default_workspace(packages).map_err(WorkspaceBuildError::Workspace)?;
    Ok(CheckedWorkspaceBuild::from_workspace(workspace))
}

fn checked_workspace_from_parsed(
    parsed: ParsedPackage,
) -> Result<CheckedWorkspaceBuild, WorkspaceBuildError> {
    let workspace_packages = default_workspace_packages_from_parsed(parsed);
    let workspace = assemble_default_workspace(workspace_packages).map_err(WorkspaceBuildError::Workspace)?;
    Ok(CheckedWorkspaceBuild::from_workspace(workspace))
}

pub(crate) fn checked_workspace_from_single_file(
    file_path: &Path,
    fallback_file_name: &str,
    source: &str,
) -> Result<CheckedWorkspaceBuild, WorkspaceBuildError> {
    let relative_path_from_src = file_path
        .file_name()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(fallback_file_name));
    let parsed = parse_loaded_files(vec![LoadedPackageFile {
        name: FileName::from(file_path),
        relative_path_from_src,
        source: source.to_owned(),
    }])
    .map_err(|error| WorkspaceBuildError::Discovery(WorkspaceDiscoveryError::Load(error)))?;
    checked_workspace_from_parsed(parsed)
}

fn inject_builtin_packages(
    mut workspace_packages: WorkspacePackages,
) -> Result<WorkspacePackages, WorkspaceDiscoveryError> {
    let builtin_packages = builtin_packages();
    let builtin_aliases = builtin_packages
        .iter()
        .map(|package| match &package.id {
            PackageId::Special(name) => Ok((name.to_string(), package.id.clone())),
            PackageId::Local(_) | PackageId::Remote(_) => {
                Err(WorkspaceDiscoveryError::DependencyAliasCollision {
                    package: package.id.clone(),
                    alias: String::from("<builtin>"),
                })
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    for package in &mut workspace_packages.packages {
        for (alias, builtin_id) in &builtin_aliases {
            if package.dependencies.contains_key(alias) {
                return Err(WorkspaceDiscoveryError::DependencyAliasCollision {
                    package: package.id.clone(),
                    alias: alias.clone(),
                });
            }
            package
                .dependencies
                .insert(alias.clone(), builtin_id.clone());
        }
    }

    workspace_packages.packages.extend(builtin_packages);
    Ok(workspace_packages)
}

#[cfg(test)]
mod tests {
    use super::*;
    use par_core::source::FileName;
    use par_core::workspace::{LoadedPackageFile, parse_loaded_files};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_package_root(prefix: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("par-lang-{prefix}-{unique}"));
        fs::create_dir_all(&root).expect("failed to create temp package root");
        root
    }

    fn write_package(root: &Path, manifest: &str, files: &[(&str, &str)]) {
        fs::create_dir_all(root.join("src")).expect("failed to create src directory");
        fs::write(root.join("Par.toml"), manifest).expect("failed to write manifest");
        for (relative_path, source) in files {
            let path = root.join(relative_path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("failed to create parent directory");
            }
            fs::write(path, source).expect("failed to write source file");
        }
    }

    #[test]
    fn dependency_packages_receive_builtin_aliases() {
        let root = temp_package_root("builtin-dependency");
        let helper = root.join("helper");

        write_package(
            &helper,
            "\
[package]
name = \"helper\"
",
            &[(
                "src/Util.par",
                "\
export module Util
import @core/Bool

export {
  type Flag = Bool.Bool
  dec Run : !
}

def Run = !
",
            )],
        );
        write_package(
            &root,
            "\
[package]
name = \"root\"

[dependencies]
helper = \"./helper\"
",
            &[(
                "src/Main.par",
                "\
module Main
import @helper/Util

type Alias = Util.Flag

dec Main : !
def Main = Util.Run
",
            )],
        );

        let workspace_packages = default_workspace_packages_from_path(&root, None).unwrap();
        let workspace = assemble_default_workspace(workspace_packages).unwrap();
        let (_checked, type_errors) = workspace.type_check();
        assert!(type_errors.is_empty(), "type errors: {:?}", type_errors);
    }

    #[test]
    fn builtin_alias_collisions_are_rejected() {
        let root = temp_package_root("builtin-collision");
        let helper = root.join("helper");

        write_package(
            &helper,
            "\
[package]
name = \"helper\"
",
            &[("src/Util.par", "module Util\n")],
        );
        write_package(
            &root,
            "\
[package]
name = \"root\"

[dependencies]
core = \"./helper\"
",
            &[("src/Main.par", "module Main\n")],
        );

        let error = default_workspace_packages_from_path(&root, None).unwrap_err();
        assert!(matches!(
            error,
            WorkspaceDiscoveryError::DependencyAliasCollision { alias, .. } if alias == "core"
        ));
    }

    #[test]
    fn synthetic_workspaces_use_special_root_package() {
        let parsed = parse_loaded_files(vec![LoadedPackageFile {
            name: FileName::from("Synthetic.par"),
            relative_path_from_src: PathBuf::from("Main.par"),
            source: String::from(
                "\
module Main

dec Main : !
def Main = !
",
            ),
        }])
        .unwrap();

        let workspace_packages = default_workspace_packages_from_parsed(parsed);
        assert_eq!(
            workspace_packages.root_package,
            PackageId::Special(literal!("__synthetic__"))
        );

        let workspace = assemble_default_workspace(workspace_packages).unwrap();
        assert_eq!(
            workspace.root_package(),
            &PackageId::Special(literal!("__synthetic__"))
        );
        let (_checked, type_errors) = workspace.type_check();
        assert!(type_errors.is_empty(), "type errors: {:?}", type_errors);
    }
}
