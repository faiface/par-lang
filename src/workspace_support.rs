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
    let mut packages = Vec::new();

    let builtin_packages = builtin_packages().packages;
    if let Some(core) = builtin_packages.get("core").cloned() {
        packages.push(
            WorkspacePackage::new(PackageId::Package("core".to_string()), core.parsed)
                .with_externals(core.externals),
        );
    }

    if let Some(basic) = builtin_packages.get("basic").cloned() {
        packages.push(
            WorkspacePackage::new(PackageId::Package("basic".to_string()), basic.parsed)
                .with_dependency("core", PackageId::Package("core".to_string()))
                .with_externals(basic.externals),
        );
    }

    let mut local_package = WorkspacePackage::new(PackageId::Local, local)
        .with_dependency("core", PackageId::Package("core".to_string()));
    if builtin_packages.contains_key("basic") {
        local_package =
            local_package.with_dependency("basic", PackageId::Package("basic".to_string()));
    }
    packages.push(local_package);

    load_workspace(packages)
}
