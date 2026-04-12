use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, Mutex},
};

use eframe::egui::{self, RichText};
use futures::task::{Spawn, SpawnExt};
use par_core::{
    frontend::{
        Type,
        language::{GlobalName, PackageId, Universal},
    },
    runtime::{Compiled, TypedHandle},
    source::FileName,
    workspace::{CheckedWorkspace, FileImportScope, ModulePath},
};
use par_runtime::linker::Linked;
use tokio_util::sync::CancellationToken;

use super::readback::Element;

#[derive(Default)]
struct ModuleMenuTree<'a> {
    directories: BTreeMap<&'a str, ModuleMenuTree<'a>>,
    modules: BTreeMap<&'a str, &'a ModulePath>,
}

impl<'a> ModuleMenuTree<'a> {
    fn from_modules(modules: &'a [ModulePath]) -> Self {
        let mut tree = Self::default();
        for module in modules {
            tree.insert(module, 0);
        }
        tree
    }

    fn insert(&mut self, module: &'a ModulePath, depth: usize) {
        if let Some(directory) = module.directories.get(depth) {
            self.directories
                .entry(directory.as_str())
                .or_default()
                .insert(module, depth + 1);
        } else {
            self.modules.insert(module.module.as_str(), module);
        }
    }
}

fn run_definition(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    name: &GlobalName<Universal>,
    display_scope: Option<FileImportScope<Universal>>,
    ctx: &egui::Context,
) {
    if let Some(cancel_token) = cancel_token {
        cancel_token.cancel();
    }
    let token = CancellationToken::new();
    *cancel_token = Some(token.clone());

    let ty = name_to_ty.get(name).unwrap();
    let package = compiled.code.get_with_name(name).unwrap();
    let (handle, reducer_future) =
        par_runtime::start_and_instantiate(spawner.clone(), compiled.code.arena.clone(), package);

    let repaint_ctx = ctx.clone();
    *element = Some(Element::new(
        Arc::new(move || {
            repaint_ctx.request_repaint();
        }),
        spawner.clone(),
        display_scope,
        TypedHandle::new(
            program.checked_module().type_defs.clone(),
            ty.clone(),
            handle,
        ),
    ));
    let repaint_ctx = ctx.clone();
    let _ = spawner.spawn(async move {
        tokio::select! {
            _ = token.cancelled() => {
                println!("Note: Reducer cancelled.");
            }
            _ = reducer_future => {
                repaint_ctx.request_repaint();
                println!("Note: Reducer completed.");
            }
        }
    });
}

fn show_definition_item(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    ui: &mut egui::Ui,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    display_scope: Option<FileImportScope<Universal>>,
    name: &GlobalName<Universal>,
    label: &str,
) {
    if ui.button(label).clicked() {
        run_definition(
            spawner,
            cancel_token,
            element,
            program,
            compiled,
            name_to_ty,
            name,
            display_scope,
            ui.ctx(),
        );
        ui.close();
    }
}

fn show_module_definitions(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    ui: &mut egui::Ui,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    display_scope: Option<FileImportScope<Universal>>,
    package: &PackageId,
    module: &ModulePath,
) {
    let mut has_definitions = false;
    for name in program.checked_module().definitions.keys().filter(|name| {
        name.module.package == *package
            && name.module.directories == module.directories
            && name.module.module == module.module
    }) {
        has_definitions = true;
        show_definition_item(
            spawner.clone(),
            cancel_token,
            element,
            ui,
            program.clone(),
            compiled,
            name_to_ty,
            display_scope.clone(),
            name,
            &name.primary,
        );
    }

    if !has_definitions {
        ui.label(RichText::new("No definitions").italics());
    }
}

fn show_module_tree(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    ui: &mut egui::Ui,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    display_scope: Option<FileImportScope<Universal>>,
    package: &PackageId,
    tree: &ModuleMenuTree<'_>,
) {
    for (directory, subtree) in &tree.directories {
        ui.menu_button(*directory, |ui| {
            show_module_tree(
                spawner.clone(),
                cancel_token,
                element,
                ui,
                program.clone(),
                compiled,
                name_to_ty,
                display_scope.clone(),
                package,
                subtree,
            );
        });
    }

    for (module_name, module) in &tree.modules {
        ui.menu_button(*module_name, |ui| {
            show_module_definitions(
                spawner.clone(),
                cancel_token,
                element,
                ui,
                program.clone(),
                compiled,
                name_to_ty,
                display_scope.clone(),
                package,
                module,
            );
        });
    }
}

fn show_package_modules(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    ui: &mut egui::Ui,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    display_scope: Option<FileImportScope<Universal>>,
    package: &PackageId,
    exclude_module: Option<&ModulePath>,
) {
    let modules = program
        .workspace()
        .modules_in_package(package)
        .iter()
        .filter(|module| exclude_module != Some(*module))
        .cloned()
        .collect::<Vec<_>>();

    if modules.is_empty() {
        let empty_label = if exclude_module.is_some() {
            "No other modules"
        } else {
            "No modules"
        };
        ui.label(RichText::new(empty_label).italics());
        return;
    }

    let tree = ModuleMenuTree::from_modules(&modules);
    show_module_tree(
        spawner,
        cancel_token,
        element,
        ui,
        program,
        compiled,
        name_to_ty,
        display_scope,
        package,
        &tree,
    );
}

fn package_label(root_package: &PackageId, package: &PackageId) -> String {
    if package == root_package {
        return String::from("This package");
    }

    match package {
        PackageId::Special(name) | PackageId::Local(name) | PackageId::Remote(name) => {
            format!("@{name}")
        }
    }
}

pub(super) fn show_run_menu(
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: &mut Option<CancellationToken>,
    element: &mut Option<Arc<Mutex<Element>>>,
    ui: &mut egui::Ui,
    active_file: &FileName,
    program: Arc<CheckedWorkspace>,
    compiled: &Compiled<Linked>,
    name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
) {
    let current_scope = program.workspace().import_scope(active_file).cloned();
    let current_package = current_scope
        .as_ref()
        .map(|scope| scope.current_module.package.clone());
    let current_module = current_scope
        .as_ref()
        .map(|scope| scope.current_module.clone());
    let current_module_path = current_module.as_ref().map(|module| ModulePath {
        directories: module.directories.clone(),
        module: module.module.clone(),
    });
    let other_packages = program
        .workspace()
        .packages()
        .into_iter()
        .filter(|package| current_package.as_ref() != Some(package))
        .collect::<Vec<_>>();

    ui.menu_button("Packages", |ui| {
        if other_packages.is_empty() {
            ui.label(RichText::new("No other packages").italics());
        }

        for package in &other_packages {
            ui.menu_button(
                package_label(program.workspace().root_package(), package),
                |ui| {
                    show_package_modules(
                        spawner.clone(),
                        cancel_token,
                        element,
                        ui,
                        program.clone(),
                        compiled,
                        name_to_ty,
                        current_scope.clone(),
                        package,
                        None,
                    );
                },
            );
        }
    });

    if let Some(package) = current_package.as_ref() {
        ui.menu_button("Modules", |ui| {
            show_package_modules(
                spawner.clone(),
                cancel_token,
                element,
                ui,
                program.clone(),
                compiled,
                name_to_ty,
                current_scope.clone(),
                package,
                current_module_path.as_ref(),
            );
        });
    }

    if let Some(current_module) = current_module.as_ref() {
        let current_definitions = program
            .checked_module()
            .definitions
            .keys()
            .filter(|name| name.module == *current_module)
            .collect::<Vec<_>>();

        if !current_definitions.is_empty() {
            ui.separator();
        }

        for name in current_definitions {
            let label = program.render_global_in_file(active_file, name);
            show_definition_item(
                spawner.clone(),
                cancel_token,
                element,
                ui,
                program.clone(),
                compiled,
                name_to_ty,
                current_scope.clone(),
                name,
                &label,
            );
        }
    }
}
