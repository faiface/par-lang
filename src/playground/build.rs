use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::package_utils::SourceLookup;
use crate::workspace_support::{
    CheckedWorkspaceBuild, ScopedTypeError, WorkspaceBuildError, checked_workspace_from_path,
    checked_workspace_from_single_file,
};
use par_core::frontend::DefinitionBody;
use par_core::{
    runtime::{Compiled, RuntimeCompilerError},
    workspace::{CheckedWorkspace, WorkspaceDiscoveryError, WorkspaceError},
};
use par_runtime::linker::Linked;

#[derive(Clone)]
pub(super) enum BuildError {
    Discovery(WorkspaceDiscoveryError),
    Workspace(WorkspaceError),
    Type {
        errors: Vec<ScopedTypeError>,
        sources: SourceLookup,
    },
    InetCompile(RuntimeCompilerError),
}

impl BuildError {
    pub(super) fn display(&self, code: Arc<str>) -> String {
        match self {
            Self::Discovery(error) => error.to_string(),
            Self::Workspace(error) => error.to_string(),
            Self::Type { errors, sources } => errors
                .iter()
                .map(|error| format!("{:?}", error.to_report(sources)))
                .collect::<Vec<_>>()
                .join("\n"),
            Self::InetCompile(error) => format!("inet compilation error: {}", error.display(&code)),
        }
    }
}

#[derive(Clone)]
pub(super) enum BuildResult {
    None,
    DiscoveryError {
        error: WorkspaceDiscoveryError,
    },
    WorkspaceError {
        error: WorkspaceError,
    },
    TypeError {
        pretty: String,
        checked: Arc<CheckedWorkspace>,
        errors: Vec<ScopedTypeError>,
        sources: SourceLookup,
    },
    InetError {
        pretty: String,
        checked: Arc<CheckedWorkspace>,
        error: RuntimeCompilerError,
    },
    Ok {
        pretty: String,
        checked: Arc<CheckedWorkspace>,
        rt_compiled: Compiled<Linked>,
    },
}

impl BuildResult {
    pub(super) fn error(&self) -> Option<BuildError> {
        match self {
            Self::None => None,
            Self::DiscoveryError { error } => Some(BuildError::Discovery(error.clone())),
            Self::WorkspaceError { error } => Some(BuildError::Workspace(error.clone())),
            Self::TypeError {
                errors, sources, ..
            } => Some(BuildError::Type {
                errors: errors.clone(),
                sources: sources.clone(),
            }),
            Self::InetError { error, .. } => Some(BuildError::InetCompile(error.clone())),
            Self::Ok { .. } => None,
        }
    }

    pub(super) fn pretty(&self) -> Option<&str> {
        match self {
            Self::TypeError { pretty, .. }
            | Self::InetError { pretty, .. }
            | Self::Ok { pretty, .. } => Some(pretty),
            Self::None | Self::DiscoveryError { .. } | Self::WorkspaceError { .. } => None,
        }
    }

    pub(super) fn checked(&self) -> Option<Arc<CheckedWorkspace>> {
        match self {
            Self::TypeError { checked, .. }
            | Self::InetError { checked, .. }
            | Self::Ok { checked, .. } => Some(Arc::clone(checked)),
            Self::None | Self::DiscoveryError { .. } | Self::WorkspaceError { .. } => None,
        }
    }

    pub(super) fn rt_compiled(&self) -> Option<&Compiled<Linked>> {
        match self {
            Self::Ok { rt_compiled, .. } => Some(rt_compiled),
            Self::None
            | Self::DiscoveryError { .. }
            | Self::WorkspaceError { .. }
            | Self::TypeError { .. }
            | Self::InetError { .. } => None,
        }
    }

    pub(super) fn from_single_file_package(
        source: &str,
        file_path: PathBuf,
        max_interactions: u32,
    ) -> Self {
        match checked_workspace_from_single_file(&file_path, "Main.par", source) {
            Ok(build) => Self::from_checked_build(build, max_interactions),
            Err(WorkspaceBuildError::Discovery(error)) => Self::DiscoveryError { error },
            Err(WorkspaceBuildError::Workspace(error)) => Self::WorkspaceError { error },
        }
    }

    pub(super) fn from_package_active_file(
        active_file_path: &Path,
        active_source: &str,
        max_interactions: u32,
    ) -> Self {
        let mut overrides = std::collections::HashMap::new();
        overrides.insert(active_file_path.to_path_buf(), active_source.to_owned());

        match checked_workspace_from_path(active_file_path, Some(&overrides)) {
            Ok(build) => Self::from_checked_build(build, max_interactions),
            Err(WorkspaceBuildError::Discovery(WorkspaceDiscoveryError::PackageRootNotFound {
                ..
            })) => Self::from_single_file_package(
                active_source,
                active_file_path.to_path_buf(),
                max_interactions,
            ),
            Err(WorkspaceBuildError::Discovery(error)) => Self::DiscoveryError { error },
            Err(WorkspaceBuildError::Workspace(error)) => Self::WorkspaceError { error },
        }
    }

    fn from_checked_build(build: CheckedWorkspaceBuild, max_interactions: u32) -> Self {
        let pretty = build
            .checked
            .workspace()
            .lowered_module()
            .definitions
            .iter()
            .map(
                |par_core::frontend::Definition {
                     span: _,
                     name,
                     body,
                 }| {
                    let mut buf = String::new();
                    write!(&mut buf, "def {name} = ").expect("write failed");
                    match body {
                        DefinitionBody::Par(expr) => {
                            expr.pretty(&mut buf, 0).expect("write failed");
                        }
                        DefinitionBody::External(_) => {
                            write!(&mut buf, "<external>").expect("write failed");
                        }
                    }
                    write!(&mut buf, "\n\n").expect("write failed");
                    buf
                },
            )
            .collect();

        if !build.type_errors.is_empty() {
            return Self::TypeError {
                pretty,
                checked: Arc::new(build.checked),
                errors: build.type_errors,
                sources: build.sources,
            };
        }
        let (checked, rt_compiled, _) = match build.compile_linked(max_interactions) {
            Ok(build) => build,
            Err((checked, error)) => {
                return Self::InetError {
                    pretty,
                    checked: Arc::new(checked),
                    error,
                };
            }
        };
        Self::Ok {
            pretty,
            checked: Arc::new(checked),
            rt_compiled,
        }
    }
}
