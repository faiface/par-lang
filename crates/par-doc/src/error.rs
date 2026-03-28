use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use par_core::workspace::{WorkspaceDiscoveryError, WorkspaceError};

#[derive(Debug, Clone)]
pub enum DocError {
    Discovery(WorkspaceDiscoveryError),
    Workspace(WorkspaceError),
    TypeCheck(Vec<String>),
    Template(String),
    Io { path: PathBuf, message: String },
}

impl DocError {
    pub(crate) fn template(error: impl Display) -> Self {
        Self::Template(error.to_string())
    }

    pub(crate) fn io(path: impl AsRef<Path>, error: impl Display) -> Self {
        Self::Io {
            path: path.as_ref().to_path_buf(),
            message: error.to_string(),
        }
    }
}

impl Display for DocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Discovery(error) => write!(f, "{error}"),
            Self::Workspace(error) => write!(f, "{error}"),
            Self::TypeCheck(reports) => write!(f, "{}", reports.join("\n")),
            Self::Template(message) => write!(f, "Failed to render documentation HTML: {message}"),
            Self::Io { path, message } => {
                write!(
                    f,
                    "Failed to write documentation file {}: {message}",
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for DocError {}
