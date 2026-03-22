use std::path::Path;

use par_builtin::builtin_packages;
use par_core::frontend::language::PackageId;
use par_core::workspace::{
    ParsedPackage, Workspace, WorkspacePackage, load_workspace, parse_package,
};

pub fn default_workspace_from_path(
    start: impl AsRef<Path>,
) -> Result<Workspace, par_core::workspace::WorkspaceError> {
    let local = parse_package(start).map_err(par_core::workspace::WorkspaceError::Load)?;
    default_workspace_from_parsed(local)
}

pub fn default_workspace_from_parsed(
    local: ParsedPackage,
) -> Result<Workspace, par_core::workspace::WorkspaceError> {
    let mut packages = builtin_packages();

    let mut local_package = WorkspacePackage::new(PackageId::Local, local);
    for builtin in &packages {
        if let PackageId::Package(name) = &builtin.id {
            local_package = local_package.with_dependency(name.as_str(), builtin.id.clone());
        }
    }
    packages.push(local_package);

    load_workspace(packages)
}
