use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use par_builtin::builtin_packages;
use par_core::frontend::TypeError;
use par_core::frontend::language::{PackageId, Universal};
use par_core::workspace::{
    PackageGraph, ParsedModule, Workspace, WorkspaceDiscoveryError, WorkspacePackage,
    WorkspacePackages, assemble_workspace,
};

use crate::error::DocError;
use crate::model::{
    ItemKind, ItemModel, LoadedSite, ModuleModel, PackageKind, PackageModel, SiteModel,
    SiteSections,
};

#[derive(Debug, Clone)]
struct PackageMeta {
    name: String,
    identity: String,
    kind: PackageKind,
    is_root: bool,
    module_exports: BTreeMap<par_core::workspace::ModulePath, bool>,
}

#[derive(Debug, Clone)]
struct RawItem {
    kind: ItemKind,
    name: par_core::frontend::language::GlobalName<Universal>,
    params: Vec<String>,
    typ: par_core::frontend::Type<Universal>,
    doc_markdown: Option<String>,
    exported: bool,
}

pub fn load_site(start: &Path) -> Result<LoadedSite, DocError> {
    match PackageGraph::discover_from_path(start) {
        Ok(graph) => load_workspace_site(graph),
        Err(WorkspaceDiscoveryError::PackageRootNotFound { .. }) => load_builtin_site(start),
        Err(error) => Err(DocError::Discovery(error)),
    }
}

fn load_workspace_site(graph: PackageGraph) -> Result<LoadedSite, DocError> {
    let root_package = graph.root_package.clone();
    let root_package_data = graph
        .packages
        .iter()
        .find(|package| package.id == root_package)
        .expect("discovered graph must contain the root package");
    let direct_dependencies = root_package_data
        .dependencies
        .values()
        .cloned()
        .collect::<BTreeSet<_>>();
    let default_out_dir = root_package_data.layout.root_dir.join("doc");

    let mut workspace_packages = graph
        .clone()
        .into_workspace_packages(None)
        .map_err(DocError::Discovery)?;
    inject_builtin_packages(&mut workspace_packages)?;

    let package_meta = build_workspace_meta(
        &graph,
        &workspace_packages,
        Some(&root_package),
        &direct_dependencies,
    );
    let workspace = assemble_workspace(workspace_packages).map_err(DocError::Workspace)?;
    ensure_type_checked(&workspace)?;

    Ok(LoadedSite {
        model: build_site_model(&workspace, package_meta, Some(root_package)),
        default_out_dir,
    })
}

fn load_builtin_site(start: &Path) -> Result<LoadedSite, DocError> {
    let workspace_packages = WorkspacePackages {
        root_package: PackageId::Special("__par_doc__".into()),
        packages: builtin_packages(),
    };
    let package_meta = build_builtin_meta(&workspace_packages);
    let workspace = assemble_workspace(workspace_packages).map_err(DocError::Workspace)?;
    ensure_type_checked(&workspace)?;

    Ok(LoadedSite {
        model: build_site_model(&workspace, package_meta, None),
        default_out_dir: fallback_out_dir(start),
    })
}

fn ensure_type_checked(workspace: &Workspace) -> Result<(), DocError> {
    let (_checked, type_errors) = workspace.type_check();
    if type_errors.is_empty() {
        Ok(())
    } else {
        Err(DocError::TypeCheck(render_type_errors(
            workspace,
            type_errors,
        )))
    }
}

fn render_type_errors(workspace: &Workspace, errors: Vec<TypeError<Universal>>) -> Vec<String> {
    errors
        .into_iter()
        .map(|error| {
            let (span, _) = error.spans();
            let source = span
                .file()
                .and_then(|file| workspace.sources().get(&file).cloned())
                .unwrap_or_else(|| fallback_source(workspace.sources()));
            let scope = span.file().and_then(|file| workspace.import_scope(&file));
            format!("{:?}", error.to_report(source, scope))
        })
        .collect()
}

fn fallback_source(sources: &HashMap<par_core::source::FileName, Arc<str>>) -> Arc<str> {
    sources
        .values()
        .next()
        .cloned()
        .unwrap_or_else(|| Arc::from(""))
}

fn build_workspace_meta(
    graph: &PackageGraph,
    workspace_packages: &WorkspacePackages,
    root_package: Option<&PackageId>,
    direct_dependencies: &BTreeSet<PackageId>,
) -> BTreeMap<PackageId, PackageMeta> {
    let discovered = graph
        .packages
        .iter()
        .map(|package| (package.id.clone(), package))
        .collect::<BTreeMap<_, _>>();

    workspace_packages
        .packages
        .iter()
        .map(|package| {
            let kind = package_kind(&package.id, root_package, direct_dependencies);
            let (name, identity) = match discovered.get(&package.id) {
                Some(discovered) => (
                    discovered.manifest.name.clone(),
                    package_identity(&package.id),
                ),
                None => (
                    builtin_package_name(&package.id),
                    String::from("built-in package"),
                ),
            };
            (
                package.id.clone(),
                PackageMeta {
                    name,
                    identity,
                    kind,
                    is_root: root_package == Some(&package.id),
                    module_exports: module_exports_for(package),
                },
            )
        })
        .collect()
}

fn build_builtin_meta(workspace_packages: &WorkspacePackages) -> BTreeMap<PackageId, PackageMeta> {
    workspace_packages
        .packages
        .iter()
        .map(|package| {
            (
                package.id.clone(),
                PackageMeta {
                    name: builtin_package_name(&package.id),
                    identity: String::from("built-in package"),
                    kind: PackageKind::BuiltIn,
                    is_root: false,
                    module_exports: module_exports_for(package),
                },
            )
        })
        .collect()
}

fn build_site_model(
    workspace: &Workspace,
    package_meta: BTreeMap<PackageId, PackageMeta>,
    _root_package: Option<PackageId>,
) -> SiteModel {
    let mut raw_items_by_module = BTreeMap::<Universal, Vec<RawItem>>::new();

    for type_def in &workspace.lowered_module().type_defs {
        raw_items_by_module
            .entry(type_def.name.module.clone())
            .or_default()
            .push(RawItem {
                kind: ItemKind::Type,
                name: type_def.name.clone(),
                params: type_def.params.iter().map(ToString::to_string).collect(),
                typ: type_def.typ.clone(),
                doc_markdown: type_def.doc.as_ref().map(|doc| doc.markdown.to_string()),
                exported: type_def.exported,
            });
    }
    for declaration in &workspace.lowered_module().declarations {
        raw_items_by_module
            .entry(declaration.name.module.clone())
            .or_default()
            .push(RawItem {
                kind: ItemKind::Declaration,
                name: declaration.name.clone(),
                params: Vec::new(),
                typ: declaration.typ.clone(),
                doc_markdown: declaration.doc.as_ref().map(|doc| doc.markdown.to_string()),
                exported: declaration.exported,
            });
    }

    let mut packages = BTreeMap::new();

    for (id, meta) in package_meta {
        let mut modules = workspace
            .modules_in_package(&id)
            .iter()
            .filter_map(|path| {
                let is_exported = meta.module_exports.get(path).copied().unwrap_or(true);
                if !meta.is_root && !is_exported {
                    return None;
                }

                let universal = Universal {
                    package: id.clone(),
                    directories: path.directories.clone(),
                    module: path.module.clone(),
                };

                let mut items = raw_items_by_module
                    .remove(&universal)
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(|raw_item| {
                        let is_public = is_exported && raw_item.exported;
                        if !meta.is_root && !is_public {
                            return None;
                        }
                        Some(ItemModel {
                            kind: raw_item.kind,
                            name: raw_item.name,
                            params: raw_item.params,
                            typ: raw_item.typ,
                            doc_markdown: raw_item.doc_markdown,
                            is_public,
                        })
                    })
                    .collect::<Vec<_>>();
                sort_items(&mut items);

                Some(ModuleModel {
                    universal,
                    path: path.clone(),
                    is_exported,
                    items,
                })
            })
            .collect::<Vec<_>>();
        sort_modules(&mut modules);

        packages.insert(
            id.clone(),
            PackageModel {
                id,
                name: meta.name,
                identity: meta.identity,
                kind: meta.kind,
                modules,
            },
        );
    }

    let sections = build_sections(&packages);
    SiteModel { packages, sections }
}

fn build_sections(packages: &BTreeMap<PackageId, PackageModel>) -> SiteSections {
    let mut sections = SiteSections::default();

    for package in packages.values() {
        match package.kind {
            PackageKind::Root => sections.root.push(package.id.clone()),
            PackageKind::BuiltIn => sections.built_in.push(package.id.clone()),
            PackageKind::DirectDependency => sections.direct_dependencies.push(package.id.clone()),
            PackageKind::IndirectDependency => {
                sections.indirect_dependencies.push(package.id.clone())
            }
        }
    }

    sort_package_ids(packages, &mut sections.root);
    sort_package_ids(packages, &mut sections.built_in);
    sort_package_ids(packages, &mut sections.direct_dependencies);
    sort_package_ids(packages, &mut sections.indirect_dependencies);

    sections
}

fn sort_package_ids(packages: &BTreeMap<PackageId, PackageModel>, ids: &mut [PackageId]) {
    ids.sort_by(|left, right| {
        let left = packages
            .get(left)
            .expect("package section references missing package");
        let right = packages
            .get(right)
            .expect("package section references missing package");
        left.name
            .to_lowercase()
            .cmp(&right.name.to_lowercase())
            .then_with(|| left.name.cmp(&right.name))
            .then_with(|| left.identity.cmp(&right.identity))
    });
}

fn sort_modules(modules: &mut [ModuleModel]) {
    modules.sort_by(|left, right| {
        let left_path = left.path.to_slash_path();
        let right_path = right.path.to_slash_path();
        left_path
            .to_lowercase()
            .cmp(&right_path.to_lowercase())
            .then_with(|| left_path.cmp(&right_path))
    });
}

fn sort_items(items: &mut [ItemModel]) {
    items.sort_by(|left, right| {
        left.kind
            .cmp(&right.kind)
            .then_with(|| {
                left.name
                    .primary
                    .to_lowercase()
                    .cmp(&right.name.primary.to_lowercase())
            })
            .then_with(|| left.name.primary.cmp(&right.name.primary))
    });
}

fn package_kind(
    id: &PackageId,
    root_package: Option<&PackageId>,
    direct_dependencies: &BTreeSet<PackageId>,
) -> PackageKind {
    if root_package == Some(id) {
        PackageKind::Root
    } else if matches!(id, PackageId::Special(_)) {
        PackageKind::BuiltIn
    } else if direct_dependencies.contains(id) {
        PackageKind::DirectDependency
    } else {
        PackageKind::IndirectDependency
    }
}

fn module_exports_for(
    package: &WorkspacePackage,
) -> BTreeMap<par_core::workspace::ModulePath, bool> {
    package
        .parsed
        .modules
        .iter()
        .map(|module| (module.path.clone(), is_module_exported(module)))
        .collect()
}

fn is_module_exported(module: &ParsedModule) -> bool {
    module.files.iter().any(|file| {
        file.source_file
            .module_decl
            .as_ref()
            .map(|decl| decl.exported)
            .unwrap_or(false)
    })
}

fn builtin_package_name(id: &PackageId) -> String {
    match id {
        PackageId::Special(name) => name.to_string(),
        PackageId::Local(name) | PackageId::Remote(name) => name.to_string(),
    }
}

fn package_identity(id: &PackageId) -> String {
    match id {
        PackageId::Special(_) => String::from("built-in package"),
        PackageId::Local(path) | PackageId::Remote(path) => path.to_string(),
    }
}

fn fallback_out_dir(start: &Path) -> PathBuf {
    if start.is_dir() {
        start.join("doc")
    } else {
        start.parent().unwrap_or_else(|| Path::new(".")).join("doc")
    }
}

fn inject_builtin_packages(workspace_packages: &mut WorkspacePackages) -> Result<(), DocError> {
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
        .collect::<Result<Vec<_>, _>>()
        .map_err(DocError::Discovery)?;

    for package in &mut workspace_packages.packages {
        for (alias, builtin_id) in &builtin_aliases {
            if package.dependencies.contains_key(alias) {
                return Err(DocError::Discovery(
                    WorkspaceDiscoveryError::DependencyAliasCollision {
                        package: package.id.clone(),
                        alias: alias.clone(),
                    },
                ));
            }
            package
                .dependencies
                .insert(alias.clone(), builtin_id.clone());
        }
    }

    workspace_packages.packages.extend(builtin_packages);
    Ok(())
}
