use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fs::File,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    time::SystemTime,
};

use crate::package_utils::format_with_source_span;
use crate::workspace_support::{
    assemble_default_workspace, default_workspace_packages_from_parsed,
    default_workspace_packages_from_path,
};
use eframe::egui::{self, RichText, Theme};
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};
use futures::task::{Spawn, SpawnExt};

use crate::readback::Element;
#[cfg(target_family = "wasm")]
use crate::wasm_spawn::WasmSpawn;
use core::time::Duration;
use par_core::frontend::DefinitionBody;
use par_core::{
    frontend::{
        Type, TypeError,
        language::{GlobalName, PackageId, Universal},
    },
    runtime::{Compiled, RuntimeCompilerError, TypedHandle},
    source::FileName,
    workspace::{
        CheckedWorkspace, LoadedPackageFile, ModulePath, PackageLoadError, WorkspaceDiscoveryError,
        WorkspaceError, parse_loaded_files,
    },
};
use par_runtime::linker::Linked;
use par_runtime::spawn::TokioSpawn;
use std::fmt::Write;
use tokio_util::sync::CancellationToken;

#[derive(Debug, Clone)]
enum BuildError {
    Discovery(WorkspaceDiscoveryError),
    Workspace(WorkspaceError),
    Type(Vec<TypeError<Universal>>),
    InetCompile(RuntimeCompilerError),
}

impl BuildError {
    fn display(&self, code: Arc<str>) -> String {
        match self {
            Self::Discovery(WorkspaceDiscoveryError::Load(PackageLoadError::ParseError {
                source,
                error,
                ..
            })) => format!(
                "{:?}",
                miette::Report::from(error.to_owned()).with_source_code(source.clone())
            ),
            Self::Discovery(error) => error.to_string(),
            Self::Workspace(WorkspaceError::LowerError { source, error, .. }) => {
                format!("{:?}", error.to_report(source.clone()))
            }
            Self::Workspace(error @ WorkspaceError::UnknownDependency { source, span, .. })
            | Self::Workspace(
                error @ WorkspaceError::ImportedModuleNotFound { source, span, .. },
            )
            | Self::Workspace(error @ WorkspaceError::DuplicateImportAlias { source, span, .. })
            | Self::Workspace(
                error @ WorkspaceError::BindingNameConflictsWithImportAlias { source, span, .. },
            )
            | Self::Workspace(
                error @ WorkspaceError::UnknownModuleQualifier { source, span, .. },
            )
            | Self::Workspace(
                error @ WorkspaceError::QualifiedCurrentModuleReference { source, span, .. },
            ) => format_with_source_span(source.clone(), span, error.to_string()),
            Self::Workspace(error) => error.to_string(),
            Self::Type(errors) => errors
                .iter()
                .map(|error| format!("{:?}", error.to_report(code.clone())))
                .collect::<Vec<_>>()
                .join("\n"),
            Self::InetCompile(error) => format!("inet compilation error: {}", error.display(&code)),
        }
    }
}

#[derive(Clone)]
enum BuildResult {
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
        errors: Vec<TypeError<Universal>>,
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
    fn error(&self) -> Option<BuildError> {
        match self {
            Self::None => None,
            Self::DiscoveryError { error } => Some(BuildError::Discovery(error.clone())),
            Self::WorkspaceError { error } => Some(BuildError::Workspace(error.clone())),
            Self::TypeError { errors, .. } => Some(BuildError::Type(errors.clone())),
            Self::InetError { error, .. } => Some(BuildError::InetCompile(error.clone())),
            Self::Ok { .. } => None,
        }
    }

    fn pretty(&self) -> Option<&str> {
        match self {
            Self::TypeError { pretty, .. }
            | Self::InetError { pretty, .. }
            | Self::Ok { pretty, .. } => Some(pretty),
            Self::None | Self::DiscoveryError { .. } | Self::WorkspaceError { .. } => None,
        }
    }

    fn checked(&self) -> Option<Arc<CheckedWorkspace>> {
        match self {
            Self::TypeError { checked, .. }
            | Self::InetError { checked, .. }
            | Self::Ok { checked, .. } => Some(Arc::clone(checked)),
            Self::None | Self::DiscoveryError { .. } | Self::WorkspaceError { .. } => None,
        }
    }

    fn rt_compiled(&self) -> Option<&Compiled<Linked>> {
        match self {
            Self::Ok { rt_compiled, .. } => Some(rt_compiled),
            Self::None
            | Self::DiscoveryError { .. }
            | Self::WorkspaceError { .. }
            | Self::TypeError { .. }
            | Self::InetError { .. } => None,
        }
    }

    fn from_single_file_package(source: &str, file_path: PathBuf, max_interactions: u32) -> Self {
        let relative_path_from_src = file_path
            .file_name()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("Main.par"));
        let parsed = match parse_loaded_files(vec![LoadedPackageFile {
            name: FileName::from(file_path.as_path()),
            relative_path_from_src,
            source: source.to_owned(),
        }]) {
            Ok(parsed) => parsed,
            Err(error) => {
                return Self::DiscoveryError {
                    error: WorkspaceDiscoveryError::Load(error),
                };
            }
        };

        let workspace_packages = default_workspace_packages_from_parsed(parsed);
        let workspace = match assemble_default_workspace(workspace_packages) {
            Ok(workspace) => workspace,
            Err(error) => return Self::WorkspaceError { error },
        };
        Self::from_workspace(workspace, max_interactions)
    }

    fn from_package_active_file(
        active_file_path: &Path,
        active_source: &str,
        max_interactions: u32,
    ) -> Self {
        let mut overrides = std::collections::HashMap::new();
        overrides.insert(active_file_path.to_path_buf(), active_source.to_owned());

        let workspace_packages =
            match default_workspace_packages_from_path(active_file_path, Some(&overrides)) {
                Ok(workspace_packages) => workspace_packages,
                Err(WorkspaceDiscoveryError::PackageRootNotFound { .. }) => {
                    return Self::from_single_file_package(
                        active_source,
                        active_file_path.to_path_buf(),
                        max_interactions,
                    );
                }
                Err(error) => {
                    return Self::DiscoveryError { error };
                }
            };
        let workspace = match assemble_default_workspace(workspace_packages) {
            Ok(workspace) => workspace,
            Err(error) => return Self::WorkspaceError { error },
        };
        Self::from_workspace(workspace, max_interactions)
    }

    fn from_workspace(workspace: par_core::workspace::Workspace, max_interactions: u32) -> Self {
        let pretty = workspace
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
                    write!(&mut buf, "def {} = ", name).expect("write failed");
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

        let (checked, type_errors) = workspace.type_check();
        let checked = Arc::new(checked);
        if !type_errors.is_empty() {
            return Self::TypeError {
                pretty,
                checked,
                errors: type_errors,
            };
        }
        Self::from_checked(pretty, checked, max_interactions)
    }

    fn from_checked(pretty: String, checked: Arc<CheckedWorkspace>, max_interactions: u32) -> Self {
        let rt_compiled = match checked
            .compile_runtime(max_interactions)
            .and_then(|compiled| compiled.link())
        {
            Ok(rt_compiled) => rt_compiled,
            Err(error) => {
                return Self::InetError {
                    pretty,
                    checked,
                    error,
                };
            }
        };
        Self::Ok {
            pretty,
            checked,
            rt_compiled,
        }
    }
}

pub struct Playground {
    file_path: Option<PathBuf>,
    code: String,
    build: BuildResult,
    built_code: Arc<str>,
    editor_font_size: f32,
    show_compiled: bool,
    show_ic: bool,
    element: Option<Arc<Mutex<Element>>>,
    cursor_pos: (u32, u32),
    theme_mode: ThemeMode,
    #[cfg(not(target_arch = "wasm32"))]
    _rt: tokio::runtime::Runtime,
    spawner: Arc<dyn Spawn + Send + Sync + 'static>,
    cancel_token: Option<CancellationToken>,
    file_old_mtime: Option<SystemTime>,
    max_interactions: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThemeMode {
    System,
    Light,
    Dark,
}

impl ThemeMode {
    pub fn is_dark(&self, system_dark: bool) -> bool {
        match self {
            Self::System => system_dark,
            Self::Dark => true,
            Self::Light => false,
        }
    }

    pub fn display_name(&self) -> &'static str {
        match self {
            Self::System => "System",
            Self::Light => "Light",
            Self::Dark => "Dark",
        }
    }

    pub fn all() -> &'static [Self] {
        &[Self::System, Self::Light, Self::Dark]
    }
}

impl Default for ThemeMode {
    fn default() -> Self {
        Self::System
    }
}

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

impl Playground {
    pub fn new(
        cc: &eframe::CreationContext<'_>,
        file_path: Option<PathBuf>,
        max_interactions: u32,
    ) -> Box<Self> {
        let system_dark = cc
            .egui_ctx
            .input(|ri| ri.raw.system_theme.map(|t| t == Theme::Dark))
            .unwrap_or(false);
        let initial_is_dark = ThemeMode::default().is_dark(system_dark);

        // set visuals on first run
        cc.egui_ctx.set_visuals(if initial_is_dark {
            egui::Visuals::dark()
        } else {
            egui::Visuals::light()
        });

        // cc.egui_ctx.set_visuals(egui::Visuals::light());
        cc.egui_ctx.all_styles_mut(|style| {
            style.text_styles.extend([
                (egui::TextStyle::Monospace, egui::FontId::monospace(16.0)),
                (egui::TextStyle::Button, egui::FontId::proportional(18.0)),
                (egui::TextStyle::Body, egui::FontId::proportional(16.0)),
            ]);
            style.visuals.code_bg_color = egui::Color32::TRANSPARENT;
            style.wrap_mode = Some(egui::TextWrapMode::Extend);
        });

        #[cfg(not(target_family = "wasm"))]
        let runtime =
            crate::tokio_factory::create_runtime().expect("Failed to create Tokio runtime");
        #[cfg(not(target_family = "wasm"))]
        let spawner = Arc::new(TokioSpawn::from_handle(runtime.handle().clone()));

        #[cfg(target_family = "wasm")]
        let spawner = Arc::new(WasmSpawn::new());

        let mut playground = Box::new(Self {
            file_path: file_path.clone(),
            code: "".to_owned(),
            build: BuildResult::None,
            built_code: Arc::from(""),
            editor_font_size: 16.0,
            show_compiled: false,
            show_ic: false,
            element: None,
            cursor_pos: (0, 0),
            theme_mode: ThemeMode::System,
            _rt: runtime,
            spawner,
            cancel_token: None,
            file_old_mtime: None,
            max_interactions: max_interactions,
        });

        if let Some(path) = file_path {
            playground.open(path);
        }

        playground
    }
}

impl eframe::App for Playground {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Try to backup current playground code contents in case of panic.
        if let Ok(e) = &mut crate::CRASH_STR.try_lock() {
            **e = Some(self.code.clone());
        }

        // Set overall UI style based on theme mode
        let system_dark = ctx
            .input(|ri| ri.raw.system_theme.map(|t| t == egui::Theme::Dark))
            .unwrap_or(false);
        let is_dark = self.theme_mode.is_dark(system_dark);

        // Always apply theme settings
        let mut visuals = if is_dark {
            egui::Visuals::dark()
        } else {
            egui::Visuals::light()
        };
        visuals.code_bg_color = egui::Color32::TRANSPARENT;
        ctx.set_visuals(visuals);

        if let Some(mtime) = self.file_mtime() {
            match self.file_old_mtime {
                None => (),
                Some(old_mtime) => {
                    if matches!(
                        mtime
                            .duration_since(old_mtime)
                            .map(|x| x > Duration::new(0, 0)),
                        Ok(true)
                    ) {
                        if let Ok(code) = self
                            .file_path
                            .as_ref()
                            .ok_or(())
                            .and_then(|path| self.reopen(&path).map_err(|_| ()))
                        {
                            self.code = code
                        };
                        self.file_old_mtime = Some(mtime);
                    }
                }
            }
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::SidePanel::left("interaction")
                .resizable(true)
                .show_separator_line(true)
                .default_width(16.0 * 32.0)
                .show_inside(ui, |ui| {
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.horizontal(|ui| {
                            if ui.button(egui::RichText::new("-").monospace()).clicked() {
                                self.editor_font_size = (self.editor_font_size - 1.0).max(8.0);
                            }
                            ui.label(
                                egui::RichText::new(self.editor_font_size.to_string()).strong(),
                            );
                            if ui.button(egui::RichText::new("+").monospace()).clicked() {
                                self.editor_font_size = (self.editor_font_size + 1.0).min(320.0);
                            }

                            ui.add_space(5.0);

                            egui::containers::menu::MenuButton::from_button(egui::Button::new(
                                egui::RichText::new("File").strong(),
                            ))
                            .ui(ui, |ui| {
                                if ui.button(egui::RichText::new("Open...").strong()).clicked() {
                                    self.file_old_mtime = None;
                                    self.open_file();
                                    ui.close();
                                }

                                if let Some(path) = self.file_path.clone() {
                                    if ui.button(egui::RichText::new("Save").strong()).clicked() {
                                        self.save_file(&path);
                                        ui.close();
                                    }

                                    let mut do_reload = self.file_old_mtime.is_some();
                                    if ui
                                        .checkbox(
                                            &mut do_reload,
                                            egui::RichText::new("Reload").strong(),
                                        )
                                        .clicked()
                                    {
                                        self.file_old_mtime = match self.file_old_mtime {
                                            None => self.file_mtime(),
                                            Some(_) => None,
                                        };
                                        ui.close();
                                    }
                                }

                                if ui
                                    .button(egui::RichText::new("Save as...").strong())
                                    .clicked()
                                {
                                    self.save_file_as();
                                    ui.close();
                                }
                            });

                            ui.add_space(5.0);

                            // create dropdown menu for theme mode selection
                            egui::containers::menu::MenuButton::from_button(egui::Button::new(
                                egui::RichText::new("Theme").strong(),
                            ))
                            .ui(ui, |ui| {
                                for &mode in ThemeMode::all() {
                                    if ui
                                        .radio(self.theme_mode == mode, mode.display_name())
                                        .clicked()
                                    {
                                        self.theme_mode = mode;
                                        ui.close();
                                    }
                                }
                            });

                            ui.add_space(5.0);

                            if let Some(file_name) =
                                self.file_path.as_ref().and_then(|p| p.file_name())
                            {
                                ui.label(
                                    egui::RichText::new(format!(
                                        "{}",
                                        file_name.to_str().unwrap_or("")
                                    ))
                                    .strong(),
                                );
                            }
                        });

                        ui.separator();

                        let editor = CodeEditor::default()
                            .id_source("code")
                            .with_syntax(par_syntax())
                            .with_rows(32)
                            .with_fontsize(self.editor_font_size)
                            .with_theme(self.get_theme(ui))
                            .with_numlines(true)
                            .show(ui, &mut self.code);

                        if let Some(cursor) = editor.cursor_range {
                            self.cursor_pos = row_and_column(&self.code, cursor.primary.index);
                        }

                        if let (Some(checked), Some(hover_pos)) =
                            (self.build.checked(), editor_hover_pos(&editor))
                        {
                            let hover_file_name = self.active_file_name();
                            if let Some(name_info) =
                                checked.hover_at(&hover_file_name, hover_pos.0, hover_pos.1)
                            {
                                let signature = checked
                                    .render_hover_signature_in_file(&hover_file_name, &name_info);
                                editor.response.on_hover_ui_at_pointer(|ui| {
                                    ui.label(RichText::new(signature).code());
                                    if let Some(doc) = name_info.doc() {
                                        ui.separator();
                                        ui.label(doc.markdown.as_str());
                                    }
                                });
                            }
                        }
                    });
                });

            self.show_interaction(ui);
        });
    }
}

fn row_and_column(source: &str, index: usize) -> (u32, u32) {
    let (mut row, mut col) = (0, 0);
    assert!(u32::try_from(index).is_ok(), "file size is too large");
    for c in source.chars().take(index) {
        if c == '\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (row, col)
}

fn editor_hover_pos(output: &egui::text_edit::TextEditOutput) -> Option<(u32, u32)> {
    let hover_pos = output.response.hover_pos()?;
    let galley_rect = egui::Rect::from_min_size(output.galley_pos, output.galley.size());
    if !galley_rect.contains(hover_pos) {
        return None;
    }

    let hover_cursor = output.galley.cursor_from_pos(hover_pos - output.galley_pos);
    Some(row_and_column(&output.galley.job.text, hover_cursor.index))
}

impl Playground {
    #[cfg(not(target_family = "wasm"))]
    fn open_file(&mut self) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            self.open(path);
        }
    }

    #[cfg(target_family = "wasm")]
    fn open_file(&mut self) {}

    fn open(&mut self, file_path: PathBuf) {
        if let Ok(file_content) = File::open(&file_path).and_then(|mut file| {
            use std::io::Read;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            Ok(buf)
        }) {
            self.file_path = Some(std::fs::canonicalize(&file_path).unwrap_or(file_path));
            self.code = file_content;
        }
    }

    fn reopen(&self, file_path: impl AsRef<Path>) -> Result<String, std::io::Error> {
        let file_path = file_path.as_ref();
        File::open(file_path).and_then(|mut file| {
            use std::io::Read;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            Ok(buf)
        })
    }

    #[cfg(not(target_family = "wasm"))]
    fn save_file_as(&mut self) {
        if let Some(path) = rfd::FileDialog::new()
            .set_can_create_directories(true)
            .save_file()
        {
            self.save_file(&path);
            self.file_path = Some(path);
        }
    }

    #[cfg(target_family = "wasm")]
    fn save_file_as(&mut self) {}

    fn save_file(&mut self, path: &Path) {
        let _ = File::create(&path).and_then(|mut file| {
            use std::io::Write;
            file.write_all(self.code.as_bytes())
        });
    }

    fn file_mtime(&self) -> Option<SystemTime> {
        let Ok(metadata) = std::fs::metadata(self.file_path.as_ref()?) else {
            return None;
        };
        let Ok(mtime) = metadata.modified() else {
            return None;
        };
        Some(mtime)
    }

    fn get_theme(&self, ui: &egui::Ui) -> ColorTheme {
        let is_dark = self.theme_mode.is_dark(ui.visuals().dark_mode);

        if is_dark {
            fix_dark_theme(ColorTheme::GRUVBOX_DARK)
        } else {
            fix_light_theme(ColorTheme::GITHUB_LIGHT)
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
        ctx: &egui::Context,
    ) {
        if let Some(cancel_token) = cancel_token {
            cancel_token.cancel();
        }
        let token = CancellationToken::new();
        *cancel_token = Some(token.clone());

        let ty = name_to_ty.get(name).unwrap();
        let package = compiled.code.get_with_name(name).unwrap();
        let (handle, reducer_future) = par_runtime::start_and_instantiate(
            spawner.clone(),
            compiled.code.arena.clone(),
            package,
        );

        let repaint_ctx = ctx.clone();
        *element = Some(Element::new(
            Arc::new(move || {
                repaint_ctx.request_repaint();
            }),
            spawner.clone(),
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
        name: &GlobalName<Universal>,
        label: &str,
    ) -> () {
        if ui.button(label).clicked() {
            Self::run_definition(
                spawner,
                cancel_token,
                element,
                program,
                compiled,
                name_to_ty,
                name,
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
            Self::show_definition_item(
                spawner.clone(),
                cancel_token,
                element,
                ui,
                program.clone(),
                compiled,
                name_to_ty,
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
        package: &PackageId,
        tree: &ModuleMenuTree<'_>,
    ) {
        for (directory, subtree) in &tree.directories {
            ui.menu_button(*directory, |ui| {
                Self::show_module_tree(
                    spawner.clone(),
                    cancel_token,
                    element,
                    ui,
                    program.clone(),
                    compiled,
                    name_to_ty,
                    package,
                    subtree,
                );
            });
        }

        for (module_name, module) in &tree.modules {
            ui.menu_button(*module_name, |ui| {
                Self::show_module_definitions(
                    spawner.clone(),
                    cancel_token,
                    element,
                    ui,
                    program.clone(),
                    compiled,
                    name_to_ty,
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
        Self::show_module_tree(
            spawner,
            cancel_token,
            element,
            ui,
            program,
            compiled,
            name_to_ty,
            package,
            &tree,
        );
    }

    fn package_label(root_package: &PackageId, package: &PackageId) -> String {
        if package == root_package {
            return String::from("This package");
        }

        match package {
            PackageId::Special(name) | PackageId::Package(name) => format!("@{name}"),
        }
    }

    fn show_run_menu(
        spawner: Arc<dyn Spawn + Send + Sync + 'static>,
        cancel_token: &mut Option<CancellationToken>,
        element: &mut Option<Arc<Mutex<Element>>>,
        ui: &mut egui::Ui,
        active_file: &FileName,
        program: Arc<CheckedWorkspace>,
        compiled: &Compiled<Linked>,
        name_to_ty: &HashMap<GlobalName<Universal>, Type<Universal>>,
    ) {
        let current_scope = program.workspace().import_scope(&active_file);
        let current_package = current_scope.map(|scope| scope.current_module.package.clone());
        let current_module = current_scope.map(|scope| scope.current_module.clone());
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
                    Self::package_label(program.workspace().root_package(), package),
                    |ui| {
                        Self::show_package_modules(
                            spawner.clone(),
                            cancel_token,
                            element,
                            ui,
                            program.clone(),
                            compiled,
                            name_to_ty,
                            package,
                            None,
                        );
                    },
                );
            }
        });

        if let Some(package) = current_package.as_ref() {
            ui.menu_button("Modules", |ui| {
                Self::show_package_modules(
                    spawner.clone(),
                    cancel_token,
                    element,
                    ui,
                    program.clone(),
                    compiled,
                    name_to_ty,
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
                let label = program.render_global_in_file(&active_file, name);
                Self::show_definition_item(
                    spawner.clone(),
                    cancel_token,
                    element,
                    ui,
                    program.clone(),
                    compiled,
                    name_to_ty,
                    name,
                    &label,
                );
            }
        }
    }

    const FILE_NAME: FileName = FileName(arcstr::literal!("Playground.par"));

    fn active_file_name(&self) -> FileName {
        self.file_path
            .as_ref()
            .cloned()
            .map(FileName::from)
            .unwrap_or(Self::FILE_NAME)
    }

    fn recompile(&mut self) {
        {
            self.build = match self.file_path.as_ref() {
                Some(file_path) => BuildResult::from_package_active_file(
                    file_path,
                    self.code.as_str(),
                    self.max_interactions,
                ),
                None => BuildResult::from_single_file_package(
                    self.code.as_str(),
                    PathBuf::from("Playground.par"),
                    self.max_interactions,
                ),
            };
        };
        self.built_code = Arc::from(self.code.as_str());
    }

    fn show_interaction(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal_top(|ui| {
                ui.add_space(5.0);

                if ui.button(egui::RichText::new("Compile").strong()).clicked() {
                    self.recompile();
                }

                if self.build.pretty().is_some() {
                    ui.checkbox(
                        &mut self.show_compiled,
                        egui::RichText::new("Show compiled"),
                    );
                    ui.checkbox(&mut self.show_ic, egui::RichText::new("Show IC"));

                    if let (Some(checked), Some(rt_compiled)) =
                        (self.build.checked(), self.build.rt_compiled())
                    {
                        let active_file = self.active_file_name();
                        let spawner = self.spawner.clone();
                        let cancel_token = &mut self.cancel_token;
                        let element = &mut self.element;
                        let name_to_ty = &rt_compiled.name_to_ty;
                        egui::containers::menu::MenuButton::from_button(
                            egui::Button::new(
                                egui::RichText::new("Run")
                                    .strong()
                                    .color(egui::Color32::BLACK),
                            )
                            .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                        )
                        .ui(ui, |ui| {
                            egui::ScrollArea::vertical().show(ui, |ui| {
                                Self::show_run_menu(
                                    spawner.clone(),
                                    cancel_token,
                                    element,
                                    ui,
                                    &active_file,
                                    checked.clone(),
                                    rt_compiled,
                                    name_to_ty,
                                );
                            })
                        });
                    }
                }
            });

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::both().show(ui, |ui| {
                    if let Some(error) = self.build.error() {
                        ui.label(
                            egui::RichText::new(error.display(self.built_code.clone()))
                                .color(red())
                                .code(),
                        );
                    }

                    let theme = self.get_theme(ui);

                    if self.show_compiled {
                        if let Some(pretty) = self.build.pretty() {
                            CodeEditor::default()
                                .id_source("compiled")
                                .with_syntax(par_syntax())
                                .with_rows(32)
                                .with_fontsize(self.editor_font_size)
                                .with_theme(theme)
                                .with_numlines(true)
                                .show(ui, &mut String::from(pretty));
                        }
                    }

                    if self.show_ic {
                        if let Some(rt_compiled) = self.build.rt_compiled() {
                            CodeEditor::default()
                                .id_source("rt_compiled")
                                .with_rows(32)
                                .with_fontsize(self.editor_font_size)
                                .with_theme(theme)
                                .with_numlines(true)
                                .show(ui, &mut format!("{}", rt_compiled));
                        }
                    }

                    if !self.show_compiled && !self.show_ic {
                        if let Some(element) = &mut self.element {
                            element.lock().unwrap().show(ui);
                        }
                    }
                });
            });
        });
    }
}

fn par_syntax() -> Syntax {
    Syntax {
        language: "Par",
        case_sensitive: true,
        comment: "//",
        comment_multiline: [r#"/*"#, r#"*/"#],
        hyperlinks: BTreeSet::from([]),
        keywords: BTreeSet::from([
            "block",
            "goto",
            "dec",
            "def",
            "type",
            "chan",
            "dual",
            "let",
            "do",
            "in",
            "case",
            "begin",
            "unfounded",
            "loop",
            "module",
            "telltypes",
            "import",
            "as",
            "export",
            "either",
            "choice",
            "recursive",
            "iterative",
            "self",
            "box",
            "catch",
            "try",
            "throw",
            "default",
            "else",
            "if",
            "is",
            "and",
            "or",
            "not",
            "poll",
            "repoll",
            "submit",
        ]),
        types: BTreeSet::from([]),
        special: BTreeSet::from(["<>"]),
    }
}

fn fix_dark_theme(mut theme: ColorTheme) -> ColorTheme {
    theme.bg = "#1F1F1F";
    theme.functions = theme.literals;
    theme
}

fn fix_light_theme(mut theme: ColorTheme) -> ColorTheme {
    theme.bg = "#F9F9F9";
    theme.functions = theme.literals;
    theme
}

#[allow(unused)]
fn red() -> egui::Color32 {
    egui::Color32::from_hex("#DE3C4B").unwrap()
}

#[allow(unused)]
fn green() -> egui::Color32 {
    egui::Color32::from_hex("#7ac74f").unwrap()
}

#[allow(unused)]
fn blue() -> egui::Color32 {
    egui::Color32::from_hex("#118ab2").unwrap()
}
