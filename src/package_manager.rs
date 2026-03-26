use par_core::workspace::{
    DEPENDENCIES_DIRECTORY, DependencySpec, MissingRemotePackageHandler,
    MissingRemotePackageRequest, PackageGraph, PackageLayout, PackageManifest,
    RemoteDependencySource, WorkspaceDiscoveryError,
};
use std::collections::BTreeSet;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddedDependencyStatus {
    Added { alias: String, source: String },
    AlreadyPresent { alias: String, source: String },
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PackageManagerResult {
    pub added_dependency: Option<AddedDependencyStatus>,
    pub fetched_dependencies: Vec<String>,
}

#[derive(Debug)]
pub enum PackageManagerError {
    Discovery(WorkspaceDiscoveryError),
    InvalidRemoteDependencySource {
        source: String,
        message: String,
    },
    DependencyAliasConflict {
        manifest_path: PathBuf,
        alias: String,
        existing_source: String,
        new_source: String,
    },
    DependenciesPathIsFile {
        path: PathBuf,
    },
    FileWrite {
        path: PathBuf,
        message: String,
    },
    DirectoryCreate {
        path: PathBuf,
        message: String,
    },
    DirectoryRemove {
        path: PathBuf,
        message: String,
    },
    RemoteFetch {
        source: String,
        destination: PathBuf,
        message: String,
    },
}

impl Display for PackageManagerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Discovery(error) => write!(f, "{error}"),
            Self::InvalidRemoteDependencySource { source, message } => {
                write!(f, "Invalid remote dependency source `{source}`: {message}")
            }
            Self::DependencyAliasConflict {
                manifest_path,
                alias,
                existing_source,
                new_source,
            } => write!(
                f,
                "Manifest {} already declares dependency `@{}` as `{}`, so it cannot also point to `{}`",
                manifest_path.display(),
                alias,
                existing_source,
                new_source
            ),
            Self::DependenciesPathIsFile { path } => write!(
                f,
                "Managed dependencies path exists but is not a directory: {}",
                path.display()
            ),
            Self::FileWrite { path, message } => {
                write!(f, "Failed to write file {}: {message}", path.display())
            }
            Self::DirectoryCreate { path, message } => {
                write!(
                    f,
                    "Failed to create directory {}: {message}",
                    path.display()
                )
            }
            Self::DirectoryRemove { path, message } => {
                write!(
                    f,
                    "Failed to remove directory {}: {message}",
                    path.display()
                )
            }
            Self::RemoteFetch {
                source,
                destination,
                message,
            } => write!(
                f,
                "Failed to fetch remote dependency `{}` into {}: {}",
                source,
                destination.display(),
                message
            ),
        }
    }
}

impl std::error::Error for PackageManagerError {}

trait RemoteFetcher {
    fn fetch(&self, source: &str, destination: &Path) -> Result<(), String>;
}

struct GitRemoteFetcher;

impl RemoteFetcher for GitRemoteFetcher {
    fn fetch(&self, source: &str, destination: &Path) -> Result<(), String> {
        let url = format!("https://{source}.git");
        let output = Command::new("git")
            .arg("clone")
            .arg("--depth")
            .arg("1")
            .arg(&url)
            .arg(destination)
            .output()
            .map_err(|error| error.to_string())?;
        if output.status.success() {
            return Ok(());
        }

        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_owned();
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_owned();
        let message = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("git clone exited with status {}", output.status)
        };
        Err(message)
    }
}

struct PreparedRemotePackage {
    manifest: PackageManifest,
    state: PreparedRemotePackageState,
}

enum PreparedRemotePackageState {
    Existing,
    Staged {
        path: PathBuf,
        destination: PathBuf,
        source: RemoteDependencySource,
    },
}

impl PreparedRemotePackage {
    fn prepare(
        fetcher: &impl RemoteFetcher,
        source: &RemoteDependencySource,
        destination: &Path,
    ) -> Result<Self, PackageManagerError> {
        if destination.exists() {
            return Ok(Self {
                manifest: PackageManifest::read_from(&destination.join("Par.toml"))
                    .map_err(PackageManagerError::Discovery)?,
                state: PreparedRemotePackageState::Existing,
            });
        }

        let Some(parent) = destination.parent() else {
            return Err(PackageManagerError::RemoteFetch {
                source: source.to_string(),
                destination: destination.to_path_buf(),
                message: String::from("destination has no parent directory"),
            });
        };
        fs::create_dir_all(parent).map_err(|error| PackageManagerError::DirectoryCreate {
            path: parent.to_path_buf(),
            message: error.to_string(),
        })?;

        let file_name = destination
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("package");
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        let stage = parent.join(format!(".{file_name}.par-tmp-{unique}"));
        fetcher.fetch(source.as_str(), &stage).map_err(|message| {
            PackageManagerError::RemoteFetch {
                source: source.to_string(),
                destination: stage.clone(),
                message,
            }
        })?;

        Ok(Self {
            manifest: PackageManifest::read_from(&stage.join("Par.toml"))
                .map_err(PackageManagerError::Discovery)?,
            state: PreparedRemotePackageState::Staged {
                path: stage,
                destination: destination.to_path_buf(),
                source: source.clone(),
            },
        })
    }

    fn install(&mut self) -> Result<bool, PackageManagerError> {
        let PreparedRemotePackageState::Staged {
            path,
            destination,
            source,
        } = &self.state
        else {
            return Ok(false);
        };

        let (path, destination, source) = (path.clone(), destination.clone(), source.clone());
        if destination.exists() {
            let _ = fs::remove_dir_all(&path);
            self.state = PreparedRemotePackageState::Existing;
            return Ok(false);
        }

        fs::rename(&path, &destination).map_err(|error| PackageManagerError::RemoteFetch {
            source: source.to_string(),
            destination,
            message: error.to_string(),
        })?;
        self.state = PreparedRemotePackageState::Existing;
        Ok(true)
    }
}

impl Drop for PreparedRemotePackage {
    fn drop(&mut self) {
        if let PreparedRemotePackageState::Staged { path, .. } = &self.state {
            let _ = fs::remove_dir_all(path);
        }
    }
}

pub fn add(
    package_path: &Path,
    source: Option<&str>,
) -> Result<PackageManagerResult, PackageManagerError> {
    let fetcher = GitRemoteFetcher;
    add_with_fetcher(package_path, source, &fetcher)
}

pub fn update(package_path: &Path) -> Result<PackageManagerResult, PackageManagerError> {
    let fetcher = GitRemoteFetcher;
    update_with_fetcher(package_path, &fetcher)
}

fn add_with_fetcher(
    package_path: &Path,
    source: Option<&str>,
    fetcher: &impl RemoteFetcher,
) -> Result<PackageManagerResult, PackageManagerError> {
    match source {
        Some(source) => add_remote_dependency_with_fetcher(package_path, source, fetcher),
        None => sync_dependencies_with_fetcher(package_path, fetcher).map(|fetched_dependencies| {
            PackageManagerResult {
                added_dependency: None,
                fetched_dependencies,
            }
        }),
    }
}

fn update_with_fetcher(
    package_path: &Path,
    fetcher: &impl RemoteFetcher,
) -> Result<PackageManagerResult, PackageManagerError> {
    let layout = PackageLayout::find_from(package_path).map_err(PackageManagerError::Discovery)?;
    let dependencies_dir = layout.root_dir.join(DEPENDENCIES_DIRECTORY);
    if dependencies_dir.exists() && !dependencies_dir.is_dir() {
        return Err(PackageManagerError::DependenciesPathIsFile {
            path: dependencies_dir,
        });
    }
    if dependencies_dir.is_dir() {
        fs::remove_dir_all(&dependencies_dir).map_err(|error| {
            PackageManagerError::DirectoryRemove {
                path: dependencies_dir.clone(),
                message: error.to_string(),
            }
        })?;
    }

    sync_dependencies_with_fetcher(&layout.root_dir, fetcher).map(|fetched_dependencies| {
        PackageManagerResult {
            added_dependency: None,
            fetched_dependencies,
        }
    })
}

fn add_remote_dependency_with_fetcher(
    package_path: &Path,
    raw_source: &str,
    fetcher: &impl RemoteFetcher,
) -> Result<PackageManagerResult, PackageManagerError> {
    let layout = PackageLayout::find_from(package_path).map_err(PackageManagerError::Discovery)?;
    let source = RemoteDependencySource::parse(raw_source).map_err(|message| {
        PackageManagerError::InvalidRemoteDependencySource {
            source: raw_source.to_string(),
            message,
        }
    })?;
    let destination = layout.managed_remote_dependency_path(&source);

    let mut manifest = PackageManifest::read_from(&layout.manifest_path)
        .map_err(PackageManagerError::Discovery)?;
    if let Some(alias) = find_existing_remote_alias(&manifest.dependencies, &source) {
        let fetched_dependencies = sync_dependencies_with_fetcher(&layout.root_dir, fetcher)?;
        return Ok(PackageManagerResult {
            added_dependency: Some(AddedDependencyStatus::AlreadyPresent {
                alias,
                source: source.to_string(),
            }),
            fetched_dependencies,
        });
    }

    let mut remote = PreparedRemotePackage::prepare(fetcher, &source, &destination)?;
    let declared_name = remote.manifest.name.clone();

    if let Some(existing) = manifest.dependencies.get(&declared_name) {
        match existing {
            DependencySpec::Remote(existing_source) if existing_source == &source => {}
            DependencySpec::Remote(existing_source) => {
                return Err(PackageManagerError::DependencyAliasConflict {
                    manifest_path: layout.manifest_path.clone(),
                    alias: declared_name,
                    existing_source: existing_source.to_string(),
                    new_source: source.to_string(),
                });
            }
            DependencySpec::Local(existing_path) => {
                return Err(PackageManagerError::DependencyAliasConflict {
                    manifest_path: layout.manifest_path.clone(),
                    alias: declared_name,
                    existing_source: existing_path.to_string_lossy().to_string(),
                    new_source: source.to_string(),
                });
            }
        }
    }

    let fetched_added_dependency = remote.install()?;

    manifest.dependencies.insert(
        declared_name.clone(),
        DependencySpec::Remote(source.clone()),
    );
    fs::write(&layout.manifest_path, manifest.render()).map_err(|error| {
        PackageManagerError::FileWrite {
            path: layout.manifest_path.clone(),
            message: error.to_string(),
        }
    })?;

    let mut fetched_dependencies = sync_dependencies_with_fetcher(&layout.root_dir, fetcher)?;
    if fetched_added_dependency {
        fetched_dependencies.push(source.to_string());
        fetched_dependencies.sort();
        fetched_dependencies.dedup();
    }
    Ok(PackageManagerResult {
        added_dependency: Some(AddedDependencyStatus::Added {
            alias: declared_name,
            source: source.to_string(),
        }),
        fetched_dependencies,
    })
}

fn sync_dependencies_with_fetcher(
    package_path: &Path,
    fetcher: &impl RemoteFetcher,
) -> Result<Vec<String>, PackageManagerError> {
    let mut fetched_dependencies = BTreeSet::new();
    let mut handler = FetchMissingRemotePackageHandler {
        fetcher,
        fetched_dependencies: &mut fetched_dependencies,
    };
    PackageGraph::discover_from_path_with_handler(package_path, &mut handler)
        .map_err(PackageManagerError::Discovery)?;
    Ok(fetched_dependencies.into_iter().collect())
}

struct FetchMissingRemotePackageHandler<'a, F> {
    fetcher: &'a F,
    fetched_dependencies: &'a mut BTreeSet<String>,
}

impl<F: RemoteFetcher> MissingRemotePackageHandler for FetchMissingRemotePackageHandler<'_, F> {
    fn handle_missing_remote(
        &mut self,
        request: &MissingRemotePackageRequest,
    ) -> Result<(), WorkspaceDiscoveryError> {
        let source = RemoteDependencySource::parse(&request.dependency).map_err(|message| {
            WorkspaceDiscoveryError::RemoteDependencyMaterializationError {
                manifest_path: request.declaring_layout.manifest_path.clone(),
                alias: request.alias.clone(),
                dependency: request.dependency.clone(),
                path: request.destination.clone(),
                message,
            }
        })?;
        match PreparedRemotePackage::prepare(self.fetcher, &source, &request.destination)
            .and_then(|mut package| package.install())
        {
            Ok(true) => {
                self.fetched_dependencies.insert(source.to_string());
                Ok(())
            }
            Ok(false) => Ok(()),
            Err(error) => Err(
                WorkspaceDiscoveryError::RemoteDependencyMaterializationError {
                    manifest_path: request.declaring_layout.manifest_path.clone(),
                    alias: request.alias.clone(),
                    dependency: request.dependency.clone(),
                    path: request.destination.clone(),
                    message: error.to_string(),
                },
            ),
        }
    }
}

fn find_existing_remote_alias(
    dependencies: &std::collections::BTreeMap<String, DependencySpec>,
    source: &RemoteDependencySource,
) -> Option<String> {
    dependencies
        .iter()
        .find_map(|(alias, dependency)| match dependency {
            DependencySpec::Remote(existing_source) if existing_source == source => {
                Some(alias.clone())
            }
            _ => None,
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn temp_dir(prefix: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("par-package-manager-{prefix}-{unique}"));
        fs::create_dir_all(&root).expect("failed to create temp root");
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

    fn copy_dir_all(from: &Path, to: &Path) {
        fs::create_dir_all(to).expect("failed to create destination directory");
        for entry in fs::read_dir(from).expect("failed to read source directory") {
            let entry = entry.expect("failed to read dir entry");
            let file_type = entry.file_type().expect("failed to read file type");
            let destination = to.join(entry.file_name());
            if file_type.is_dir() {
                copy_dir_all(&entry.path(), &destination);
            } else {
                fs::copy(entry.path(), destination).expect("failed to copy file");
            }
        }
    }

    struct FakeRemoteFetcher {
        packages: BTreeMap<String, PathBuf>,
        failures: BTreeMap<String, String>,
    }

    impl FakeRemoteFetcher {
        fn with_package(mut self, source: &str, path: PathBuf) -> Self {
            self.packages.insert(source.to_string(), path);
            self
        }

        fn with_failure(mut self, source: &str, message: &str) -> Self {
            self.failures
                .insert(source.to_string(), message.to_string());
            self
        }
    }

    impl Default for FakeRemoteFetcher {
        fn default() -> Self {
            Self {
                packages: BTreeMap::new(),
                failures: BTreeMap::new(),
            }
        }
    }

    impl RemoteFetcher for FakeRemoteFetcher {
        fn fetch(&self, source: &str, destination: &Path) -> Result<(), String> {
            if let Some(message) = self.failures.get(source) {
                return Err(message.clone());
            }
            let source_path = self
                .packages
                .get(source)
                .unwrap_or_else(|| panic!("no fake package for source {source}"));
            copy_dir_all(source_path, destination);
            Ok(())
        }
    }

    #[test]
    fn add_remote_dependency_uses_declared_manifest_name() {
        let root = temp_dir("add-by-name");
        write_package(
            &root,
            "[package]\nname = \"root\"\n",
            &[("src/Main.par", "module Main\n")],
        );

        let remote = temp_dir("remote-add");
        write_package(
            &remote,
            "[package]\nname = \"pkg\"\n",
            &[("src/Lib.par", "module Lib\n")],
        );

        let fetcher = FakeRemoteFetcher::default().with_package("github.com/example/pkg", remote);
        let result = add_with_fetcher(&root, Some("github.com/example/pkg"), &fetcher).unwrap();

        assert_eq!(
            result.added_dependency,
            Some(AddedDependencyStatus::Added {
                alias: String::from("pkg"),
                source: String::from("github.com/example/pkg"),
            })
        );
        let manifest = fs::read_to_string(root.join("Par.toml")).unwrap();
        assert!(manifest.contains("pkg = \"github.com/example/pkg\""));
    }

    #[test]
    fn add_remote_dependency_rejects_alias_conflicts() {
        let root = temp_dir("add-conflict");
        write_package(
            &root,
            "\
[package]
name = \"root\"

[dependencies]
pkg = \"github.com/example/existing\"
",
            &[("src/Main.par", "module Main\n")],
        );

        let remote = temp_dir("remote-conflict");
        write_package(
            &remote,
            "[package]\nname = \"pkg\"\n",
            &[("src/Lib.par", "module Lib\n")],
        );

        let fetcher = FakeRemoteFetcher::default().with_package("github.com/example/pkg", remote);
        let error = add_with_fetcher(&root, Some("github.com/example/pkg"), &fetcher).unwrap_err();
        assert!(matches!(
            error,
            PackageManagerError::DependencyAliasConflict { alias, .. } if alias == "pkg"
        ));
    }

    #[test]
    fn add_remote_dependency_fetches_transitive_dependencies() {
        let root = temp_dir("add-transitive");
        write_package(
            &root,
            "[package]\nname = \"root\"\n",
            &[("src/Main.par", "module Main\n")],
        );

        let remote_pkg = temp_dir("remote-transitive-pkg");
        write_package(
            &remote_pkg,
            "\
[package]
name = \"pkg\"

[dependencies]
dep = \"github.com/example/dep\"
",
            &[("src/Pkg.par", "module Pkg\n")],
        );
        let remote_dep = temp_dir("remote-transitive-dep");
        write_package(
            &remote_dep,
            "[package]\nname = \"dep\"\n",
            &[("src/Dep.par", "module Dep\n")],
        );

        let fetcher = FakeRemoteFetcher::default()
            .with_package("github.com/example/pkg", remote_pkg)
            .with_package("github.com/example/dep", remote_dep);
        let result = add_with_fetcher(&root, Some("github.com/example/pkg"), &fetcher).unwrap();

        assert_eq!(
            result.fetched_dependencies,
            vec![
                String::from("github.com/example/dep"),
                String::from("github.com/example/pkg"),
            ]
        );
        assert!(
            root.join("dependencies")
                .join("github.com")
                .join("example")
                .join("dep")
                .join("Par.toml")
                .exists()
        );
    }

    #[test]
    fn add_is_idempotent_when_remote_already_present() {
        let root = temp_dir("add-idempotent");
        write_package(
            &root,
            "[package]\nname = \"root\"\n",
            &[("src/Main.par", "module Main\n")],
        );

        let remote = temp_dir("remote-idempotent");
        write_package(
            &remote,
            "[package]\nname = \"pkg\"\n",
            &[("src/Lib.par", "module Lib\n")],
        );

        let fetcher =
            FakeRemoteFetcher::default().with_package("github.com/example/pkg", remote.clone());
        let first = add_with_fetcher(&root, Some("github.com/example/pkg"), &fetcher).unwrap();
        assert_eq!(
            first.fetched_dependencies,
            vec![String::from("github.com/example/pkg")]
        );

        let second = add_with_fetcher(&root, Some("github.com/example/pkg"), &fetcher).unwrap();
        assert_eq!(
            second.added_dependency,
            Some(AddedDependencyStatus::AlreadyPresent {
                alias: String::from("pkg"),
                source: String::from("github.com/example/pkg"),
            })
        );
        assert!(second.fetched_dependencies.is_empty());
    }

    #[test]
    fn update_replaces_managed_dependency_tree() {
        let root = temp_dir("update");
        write_package(
            &root,
            "\
[package]
name = \"root\"

[dependencies]
pkg = \"github.com/example/pkg\"
",
            &[("src/Main.par", "module Main\n")],
        );
        fs::create_dir_all(root.join("dependencies").join("old")).unwrap();
        fs::write(
            root.join("dependencies").join("old").join("stale.txt"),
            "stale",
        )
        .unwrap();

        let remote = temp_dir("remote-update");
        write_package(
            &remote,
            "[package]\nname = \"pkg\"\n",
            &[("src/Lib.par", "module Lib\n")],
        );

        let fetcher = FakeRemoteFetcher::default().with_package("github.com/example/pkg", remote);
        let result = update_with_fetcher(&root, &fetcher).unwrap();
        assert_eq!(
            result.fetched_dependencies,
            vec![String::from("github.com/example/pkg")]
        );
        assert!(!root.join("dependencies").join("old").exists());
        assert!(
            root.join("dependencies")
                .join("github.com")
                .join("example")
                .join("pkg")
                .join("Par.toml")
                .exists()
        );
    }

    #[test]
    fn fetch_failures_are_reported_clearly() {
        let root = temp_dir("fetch-failure");
        write_package(
            &root,
            "\
[package]
name = \"root\"

[dependencies]
pkg = \"github.com/example/pkg\"
",
            &[("src/Main.par", "module Main\n")],
        );

        let fetcher =
            FakeRemoteFetcher::default().with_failure("github.com/example/pkg", "git not found");
        let error = add_with_fetcher(&root, None, &fetcher).unwrap_err();
        assert!(matches!(
            error,
            PackageManagerError::Discovery(
                WorkspaceDiscoveryError::RemoteDependencyMaterializationError { message, .. }
            ) if message.contains("git not found")
        ));
    }
}
