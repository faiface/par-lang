use std::{
    collections::BTreeSet,
    fmt::Write,
    fs::File,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
};

use eframe::egui::{self, RichText, Theme};
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};

use crate::{
    icombs::readback::TypedHandle,
    location::{FileName, Span},
    par::{
        builtin::import_builtins,
        program::{CheckedModule, Definition, Module, ParseAndCompileError, TypeOnHover},
    },
    readback::Element,
    spawn::TokioSpawn,
};
use crate::{
    icombs::{compile_file, IcCompiled},
    par::{language::CompileError, parse::SyntaxError, process::Expression, types::TypeError},
};
use miette::{LabeledSpan, SourceOffset, SourceSpan};
use tokio_util::sync::CancellationToken;

pub struct Playground {
    file_path: Option<PathBuf>,
    code: String,
    compiled: Option<Result<Compiled, Error>>,
    compiled_code: Arc<str>,
    editor_font_size: f32,
    show_compiled: bool,
    show_ic: bool,
    element: Option<Arc<Mutex<Element>>>,
    cursor_pos: (u32, u32),
    theme_mode: ThemeMode,
    rt: tokio::runtime::Runtime,
    cancel_token: Option<CancellationToken>,
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

#[derive(Clone)]
pub(crate) struct Compiled {
    pub(crate) pretty: String,
    pub(crate) checked: Result<Checked, Error>,
}

impl Compiled {
    pub(crate) fn from_string(source: &str, file: FileName) -> Result<Compiled, Error> {
        let mut module = Module::parse_and_compile(source, file)?;
        import_builtins(&mut module);
        Ok(Compiled::from_module(module)?)
    }

    pub(crate) fn from_module(program: Module<Arc<Expression<()>>>) -> Result<Self, Error> {
        let pretty = program
            .definitions
            .iter()
            .map(
                |Definition {
                     span: _,
                     file: _,
                     name,
                     expression,
                 }| {
                    let mut buf = String::new();
                    write!(&mut buf, "def {} = ", name).expect("write failed");
                    expression.pretty(&mut buf, 0).expect("write failed");
                    write!(&mut buf, "\n\n").expect("write failed");
                    buf
                },
            )
            .collect();

        // attempt to type check
        let checked_program = match program.type_check() {
            Ok(checked) => checked,
            Err(error) => return Err(Error::Type(error)),
        };
        return Ok(Compiled {
            pretty,
            checked: Checked::from_program(checked_program).map_err(|err| Error::InetCompile(err)),
        });
    }
}

#[derive(Clone)]
pub(crate) struct Checked {
    pub(crate) program: Arc<CheckedModule>,
    pub(crate) type_on_hover: Arc<TypeOnHover>,
    pub(crate) ic_compiled: Option<crate::icombs::IcCompiled>,
}

impl Checked {
    pub(crate) fn from_program(
        program: CheckedModule,
    ) -> Result<Self, crate::icombs::compiler::Error> {
        // attempt to compile to interaction combinators
        Ok(Checked {
            ic_compiled: Some(compile_file(&program)?),
            type_on_hover: Arc::new(TypeOnHover::new(&program)),
            program: Arc::new(program),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Error {
    Parse(SyntaxError),
    Compile(CompileError),
    InetCompile(crate::icombs::compiler::Error),
    Type(TypeError),
}

impl From<ParseAndCompileError> for Error {
    fn from(error: ParseAndCompileError) -> Self {
        match error {
            ParseAndCompileError::Parse(error) => Self::Parse(error),
            ParseAndCompileError::Compile(error) => Self::Compile(error),
        }
    }
}

impl Playground {
    pub fn new(cc: &eframe::CreationContext<'_>, file_path: Option<PathBuf>) -> Box<Self> {
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

        let mut playground = Box::new(Self {
            file_path: file_path.clone(),
            code: "".to_owned(),
            compiled: None,
            compiled_code: Arc::from(""),
            editor_font_size: 16.0,
            show_compiled: false,
            show_ic: false,
            element: None,
            cursor_pos: (0, 0),
            theme_mode: ThemeMode::System,
            rt: tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime"),
            cancel_token: None,
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
                                    self.open_file();
                                    ui.close();
                                }

                                if let Some(path) = self.file_path.clone() {
                                    if ui.button(egui::RichText::new("Save").strong()).clicked() {
                                        self.save_file(&path);
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

                        let cursor = CodeEditor::default()
                            .id_source("code")
                            .with_syntax(par_syntax())
                            .with_rows(32)
                            .with_fontsize(self.editor_font_size)
                            .with_theme(self.get_theme(ui))
                            .with_numlines(true)
                            .show(ui, &mut self.code)
                            .cursor_range;

                        if let Some(cursor) = cursor {
                            self.cursor_pos = row_and_column(&self.code, cursor.primary.index);
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

impl Playground {
    fn open_file(&mut self) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            self.open(path);
        }
    }

    fn open(&mut self, file_path: PathBuf) {
        if let Ok(file_content) = File::open(&file_path).and_then(|mut file| {
            use std::io::Read;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            Ok(buf)
        }) {
            self.file_path = Some(file_path);
            self.code = file_content;
        }
    }

    fn save_file_as(&mut self) {
        if let Some(path) = rfd::FileDialog::new()
            .set_can_create_directories(true)
            .save_file()
        {
            self.save_file(&path);
            self.file_path = Some(path);
        }
    }

    fn save_file(&mut self, path: &Path) {
        let _ = File::create(&path).and_then(|mut file| {
            use std::io::Write;
            file.write_all(self.code.as_bytes())
        });
    }

    fn get_theme(&self, ui: &egui::Ui) -> ColorTheme {
        let is_dark = self.theme_mode.is_dark(ui.visuals().dark_mode);

        if is_dark {
            fix_dark_theme(ColorTheme::GRUVBOX_DARK)
        } else {
            fix_light_theme(ColorTheme::GITHUB_LIGHT)
        }
    }

    fn readback(
        tokio: tokio::runtime::Handle,
        cancel_token: &mut Option<CancellationToken>,
        element: &mut Option<Arc<Mutex<Element>>>,
        ui: &mut egui::Ui,
        program: Arc<CheckedModule>,
        compiled: &IcCompiled,
    ) {
        for (name, _) in &program.definitions {
            if ui.button(format!("{}", name)).clicked() {
                if let Some(cancel_token) = cancel_token {
                    cancel_token.cancel();
                }
                let token = CancellationToken::new();
                *cancel_token = Some(token.clone());

                let ty = compiled.get_type_of(name).unwrap();
                let mut net = compiled.create_net();
                let child_net = compiled.get_with_name(name).unwrap();
                let tree = net.inject_net(child_net).with_type(ty.clone());
                let (net, _future) =
                    net.start_reducer(Arc::new(TokioSpawn::from_handle(tokio.clone())));

                let ctx = ui.ctx().clone();
                *element = Some(Element::new(
                    Arc::new(move || {
                        ctx.request_repaint();
                    }),
                    Arc::new(TokioSpawn::from_handle(tokio.clone())),
                    TypedHandle::from_wrapper(program.type_defs.clone(), net, tree),
                ));
                let tokio_clone = tokio.clone();
                thread::spawn(move || {
                    tokio_clone.block_on(async {
                        tokio::select! {
                            _ = token.cancelled() => {
                                println!("Note: Reducer cancelled.");
                            }
                            _ = _future => {
                                println!("Note: Reducer completed.");
                            }
                        }
                    });
                });
                break;
            }
        }
    }

    const FILE_NAME: FileName = FileName::Path(arcstr::literal!("Playground.par"));

    fn recompile(&mut self) {
        stacker::grow(32 * 1024 * 1024, || {
            self.compiled = Some(Compiled::from_string(self.code.as_str(), Self::FILE_NAME));
        });
        self.compiled_code = Arc::from(self.code.as_str());
    }

    fn show_interaction(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal_top(|ui| {
                ui.add_space(5.0);

                if ui.button(egui::RichText::new("Compile").strong()).clicked() {
                    self.recompile();
                }

                if let Some(Ok(Compiled { checked, .. })) = &mut self.compiled {
                    ui.checkbox(
                        &mut self.show_compiled,
                        egui::RichText::new("Show compiled"),
                    );
                    ui.checkbox(&mut self.show_ic, egui::RichText::new("Show IC"));

                    if let Ok(checked) = checked {
                        if let Some(ic_compiled) = checked.ic_compiled.as_ref() {
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
                                    Self::readback(
                                        self.rt.handle().clone(),
                                        &mut self.cancel_token,
                                        &mut self.element,
                                        ui,
                                        checked.program.clone(),
                                        ic_compiled,
                                    );
                                })
                            });
                        }
                    }
                }
            });

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::both().show(ui, |ui| {
                    if let Some(Err(error)) = &self.compiled {
                        ui.label(
                            egui::RichText::new(error.display(self.compiled_code.clone()))
                                .color(red())
                                .code(),
                        );
                    }

                    let theme = self.get_theme(ui);
                    if let Some(Ok(Compiled {
                        pretty, checked, ..
                    })) = &mut self.compiled
                    {
                        if let Ok(checked) = checked {
                            if let Some(name_info) = checked.type_on_hover.query(
                                &Self::FILE_NAME,
                                self.cursor_pos.0,
                                self.cursor_pos.1,
                            ) {
                                ui.horizontal(|ui| {
                                    let mut buf = String::new();
                                    name_info.typ.pretty(&mut buf, 0).unwrap();
                                    ui.label(RichText::new(buf).code().color(green()));
                                });
                            }
                        }

                        if self.show_compiled {
                            CodeEditor::default()
                                .id_source("compiled")
                                .with_syntax(par_syntax())
                                .with_rows(32)
                                .with_fontsize(self.editor_font_size)
                                .with_theme(theme)
                                .with_numlines(true)
                                .show(ui, pretty);
                        } else if let Ok(checked) = checked {
                            if let Some(ic_compiled) = checked.ic_compiled.as_ref() {
                                if self.show_ic {
                                    CodeEditor::default()
                                        .id_source("ic_compiled")
                                        .with_rows(32)
                                        .with_fontsize(self.editor_font_size)
                                        .with_theme(theme)
                                        .with_numlines(true)
                                        .show(ui, &mut format!("{}", ic_compiled));
                                }

                                if !self.show_compiled && !self.show_ic {
                                    if let Some(element) = &mut self.element {
                                        element.lock().unwrap().show(ui);
                                    }
                                }
                            }
                        } else if let Err(err) = checked {
                            let error = err.display(self.compiled_code.clone());
                            ui.label(egui::RichText::new(error).color(red()).code());
                        }
                    }
                });
            });
        });
    }
}

/// Create a `LabeledSpan` without a label at `span`
pub fn labels_from_span(_code: &str, span: &Span) -> Vec<LabeledSpan> {
    span.start()
        .into_iter()
        .map(|start| {
            LabeledSpan::new_with_span(
                None,
                SourceSpan::new(
                    SourceOffset::from(start.offset as usize),
                    span.len() as usize,
                ),
            )
        })
        .collect()
}

impl Error {
    pub fn display(&self, code: Arc<str>) -> String {
        match self {
            Self::Parse(error) => {
                // Show syntax error with miette's formatting
                format!(
                    "{:?}",
                    miette::Report::from(error.to_owned()).with_source_code(code)
                )
            }

            Self::Compile(CompileError::MustEndProcess(loc)) => {
                let labels = labels_from_span(&code, loc);
                let code = if labels.is_empty() {
                    "<UI>".into()
                } else {
                    code
                };
                let error = miette::miette! {
                    labels = labels,
                    "This process must end."
                }
                .with_source_code(code);
                format!("{error:?}")
            }

            Self::Type(error) => format!("{:?}", error.to_report(code)),

            Self::InetCompile(err) => {
                format!("inet compilation error: {:?}", err)
            }
        }
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
            "telltypes",
            "either",
            "choice",
            "recursive",
            "iterative",
            "self",
            "box",
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

#[cfg(test)]
mod playground_test {
    use super::*;

    #[test]
    fn test_issue_44() {
        let code = r#"
def Pair = [type _] [(x)y] (x) y
"#;

        let result = std::panic::catch_unwind(|| {
            stacker::grow(32 * 1024 * 1024, || {
                Compiled::from_string(code, "test".into())
            })
        });

        match result {
            Ok(Ok(_)) => {
                panic!("Expected compilation to fail, but it succeeded");
            }
            Ok(Err(err)) => {
                // Check that we get the expected error type
                match err {
                    Error::Type(type_err) => {
                        let error_msg = format!("{:?}", type_err.to_report(Arc::from(code)));

                        assert!(
                            error_msg.contains("Type annotation required for pattern matching")
                                || error_msg.contains("pattern matching"),
                            "Error message should mention pattern matching, got: {}",
                            error_msg
                        );

                        assert!(
                            error_msg.contains("Consider adding a type annotation"),
                            "Error should contain help text about adding type annotations"
                        );

                        assert!(
                            !error_msg.contains("Type of parameter `#match0`"),
                            "Error should not expose internal variable names like #match0"
                        );
                    }
                    _ => panic!("Expected TypeError, got {:?}", err),
                }
            }
            Err(_) => {
                panic!("Compiler panicked unexpectedly");
            }
        }
    }
}
