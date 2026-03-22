use std::path::Path;

use par_builtin::{BuiltinModulePath, builtin_packages};
use par_core::frontend::language::PackageId;
use par_core::workspace::{
    ModulePath, ParsedPackage, Workspace, WorkspaceInput, WorkspacePackage, parse_package,
};

fn module_path_from_builtin(path: BuiltinModulePath) -> ModulePath {
    ModulePath {
        directories: path
            .directories
            .into_iter()
            .map(|directory| directory.to_lowercase())
            .collect(),
        module: path.module,
    }
}

pub fn default_workspace_from_path(
    start: impl AsRef<Path>,
) -> Result<Workspace, par_core::workspace::WorkspaceError> {
    let local = parse_package(start).map_err(par_core::workspace::WorkspaceError::Load)?;
    default_workspace_from_parsed(local)
}

pub fn default_workspace_from_parsed(
    local: ParsedPackage,
) -> Result<Workspace, par_core::workspace::WorkspaceError> {
    let mut input = WorkspaceInput::new();

    let builtin_packages = builtin_packages().packages;
    if let Some(core) = builtin_packages.get("core").cloned() {
        input.add_package(
            WorkspacePackage::from_parsed(PackageId::Package("core".to_string()), core.parsed)
                .with_externals(
                    core.externals.into_iter().map(|(module_path, module)| {
                        (module_path_from_builtin(module_path), module)
                    }),
                )
                .collect_sources(false),
        );
    }

    if let Some(basic) = builtin_packages.get("basic").cloned() {
        input.add_package(
            WorkspacePackage::from_parsed(PackageId::Package("basic".to_string()), basic.parsed)
                .with_dependency("core", PackageId::Package("core".to_string()))
                .with_externals(
                    basic.externals.into_iter().map(|(module_path, module)| {
                        (module_path_from_builtin(module_path), module)
                    }),
                )
                .collect_sources(false),
        );
    }

    let mut local_package = WorkspacePackage::new(PackageId::Local, local)
        .with_dependency("core", PackageId::Package("core".to_string()));
    if builtin_packages.contains_key("basic") {
        local_package =
            local_package.with_dependency("basic", PackageId::Package("basic".to_string()));
    }
    input.add_root_package(local_package);

    input.build()
}
