use std::{
    collections::BTreeSet,
    fmt::Write,
    fs::File,
    path::{Path, PathBuf},
    sync::Arc,
};

use eframe::egui;
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};
use indexmap::IndexMap;

use crate::{
    icombs::{compile_file, IcCompiled},
    par::{
        language::{CompileError, Internal},
        parse::{parse_program, Loc, Name, Program, SyntaxError},
        process::Expression,
        types::{self, Type, TypeDefs, TypeError},
    },
    readback::ReadbackState,
    spawn::TokioSpawn,
};
use miette::{LabeledSpan, SourceOffset};

pub struct Playground {
    file_path: Option<PathBuf>,
    code: String,
    compiled: Option<Result<Compiled, Error>>,
    compiled_code: Arc<str>,
    editor_font_size: f32,
    show_compiled: bool,
    show_ic: bool,
    readback_state: Option<crate::readback::ReadbackState>,
}

#[derive(Clone)]
pub(crate) struct Compiled {
    pub(crate) pretty: String,
    pub(crate) checked: Result<Checked, Error>,
}

impl Compiled {
    pub(crate) fn from_string(source: &str) -> Result<Compiled, Error> {
        parse_program(source)
            .map_err(Error::Parse)
            .and_then(|program| {
                let type_defs = program
                    .type_defs
                    .into_iter()
                    .map(|(loc, name, params, typ)| {
                        (
                            loc,
                            Internal::Original(name),
                            params.into_iter().map(|x| Internal::Original(x)).collect(),
                            typ.map_names(&mut Internal::Original),
                        )
                    })
                    .collect();
                let declarations = program
                    .declarations
                    .into_iter()
                    .map(|(loc, name, typ)| {
                        (
                            loc,
                            Internal::Original(name),
                            typ.map_names(&mut Internal::Original),
                        )
                    })
                    .collect();
                let compile_result = program
                    .definitions
                    .into_iter()
                    .map(|(loc, name, def)| {
                        def.compile().map(|compiled| {
                            (
                                loc,
                                Internal::Original(name.clone()),
                                compiled.optimize().fix_captures(&IndexMap::new()).0,
                            )
                        })
                    })
                    .collect::<Result<_, CompileError<Loc>>>();
                compile_result
                    .map_err(|error| Error::Compile(error))
                    .and_then(|compiled| {
                        Ok(Compiled::from_program(Program {
                            type_defs,
                            declarations,
                            definitions: compiled,
                        })?)
                    })
            })
    }

    pub(crate) fn from_program(
        program: Program<Loc, Internal<Name>, Arc<Expression<Loc, Internal<Name>, ()>>>,
    ) -> Result<Self, Error> {
        let pretty = program
            .definitions
            .iter()
            .map(|(_, name, def)| {
                let mut buf = String::new();
                write!(&mut buf, "def {} = ", name).expect("write failed");
                def.pretty(&mut buf, 0).expect("write failed");
                write!(&mut buf, "\n\n").expect("write failed");
                buf
            })
            .collect();

        // attempt to type check
        let ctx = match types::Context::new_with_type_checking(&program) {
            Ok(context) => context,
            Err(error) => return Err(Error::Type(error)),
        };
        let new_program = CheckedProgram {
            type_defs: ctx.get_type_defs().clone(),
            declarations: ctx.get_declarations().clone(),
            definitions: ctx.get_checked_definitions().clone(),
        };
        return Ok(Compiled {
            pretty,
            checked: Checked::from_program(new_program).map_err(|err| Error::InetCompile(err)),
        });
    }
}

#[derive(Debug, Default)]
pub struct CheckedProgram {
    pub type_defs: TypeDefs<Loc, Internal<Name>>,
    pub declarations: IndexMap<Internal<Name>, (Loc, Type<Loc, Internal<Name>>)>,
    pub definitions: IndexMap<
        Internal<Name>,
        (
            Loc,
            Arc<Expression<Loc, Internal<Name>, Type<Loc, Internal<Name>>>>,
        ),
    >,
}

#[derive(Clone)]
pub(crate) struct Checked {
    pub(crate) program: Arc<CheckedProgram>,
    pub(crate) ic_compiled: Option<crate::icombs::IcCompiled>,
}

impl Checked {
    pub(crate) fn from_program(
        program: CheckedProgram,
    ) -> Result<Self, crate::icombs::compiler::Error> {
        // attempt to compile to interaction combinators
        Ok(Checked {
            ic_compiled: Some(compile_file(&program)?),
            program: Arc::new(program),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Error {
    Parse(SyntaxError),
    Compile(CompileError<Loc>),
    InetCompile(crate::icombs::compiler::Error),
    Type(TypeError<Loc, Internal<Name>>),
}

impl Playground {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Box<Self> {
        cc.egui_ctx.all_styles_mut(|style| {
            style.text_styles.extend([
                (egui::TextStyle::Monospace, egui::FontId::monospace(16.0)),
                (egui::TextStyle::Button, egui::FontId::proportional(18.0)),
                (egui::TextStyle::Body, egui::FontId::proportional(16.0)),
            ]);
            style.visuals.code_bg_color = egui::Color32::TRANSPARENT;
            style.wrap_mode = Some(egui::TextWrapMode::Extend);
        });
        let default_code = DEFAULT_CODE.to_string();
        Box::new(Self {
            file_path: None,
            code: default_code.clone(),
            compiled: None,
            compiled_code: Arc::from(default_code),
            editor_font_size: 16.0,
            show_compiled: false,
            show_ic: false,
            readback_state: Default::default(),
        })
    }
}

impl eframe::App for Playground {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
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

                            egui::menu::menu_custom_button(
                                ui,
                                egui::Button::new(egui::RichText::new("File").strong()),
                                |ui| {
                                    if ui.button(egui::RichText::new("Open...").strong()).clicked()
                                    {
                                        self.open_file();
                                        ui.close_menu();
                                    }

                                    if let Some(path) = self.file_path.clone() {
                                        if ui.button(egui::RichText::new("Save").strong()).clicked()
                                        {
                                            self.save_file(&path);
                                            ui.close_menu();
                                        }
                                    }

                                    if ui
                                        .button(egui::RichText::new("Save as...").strong())
                                        .clicked()
                                    {
                                        self.save_file_as();
                                        ui.close_menu();
                                    }
                                },
                            );

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

                        CodeEditor::default()
                            .id_source("code")
                            .with_syntax(par_syntax())
                            .with_rows(32)
                            .with_fontsize(self.editor_font_size)
                            .with_theme(self.get_theme(ui))
                            .with_numlines(true)
                            .show(ui, &mut self.code);
                    });
                });

            self.show_interaction(ui);
        });
    }
}

impl Playground {
    fn open_file(&mut self) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            if let Ok(file_content) = File::open(&path).and_then(|mut file| {
                use std::io::Read;
                let mut buf = String::new();
                file.read_to_string(&mut buf)?;
                Ok(buf)
            }) {
                self.file_path = Some(path);
                self.code = file_content;
            }
        }
    }

    fn save_file_as(&mut self) {
        if let Some(path) = rfd::FileDialog::new()
            .set_can_create_directories(true)
            .save_file()
        {
            self.save_file(&path);
        }
    }

    fn save_file(&mut self, path: &Path) {
        let _ = File::create(&path).and_then(|mut file| {
            use std::io::Write;
            file.write_all(self.code.as_bytes())
        });
    }

    fn get_theme(&self, ui: &egui::Ui) -> ColorTheme {
        if ui.visuals().dark_mode {
            fix_dark_theme(ColorTheme::GRUVBOX_DARK)
        } else {
            fix_light_theme(ColorTheme::GITHUB_LIGHT)
        }
    }

    fn readback(
        readback_state: &mut Option<ReadbackState>,
        ui: &mut egui::Ui,
        program: Arc<CheckedProgram>,
        compiled: &IcCompiled,
    ) {
        for (internal_name, _) in &program.definitions {
            if let Internal::Original(name) = internal_name {
                if ui.button(&name.string).clicked() {
                    let ty = compiled
                        .get_type_of(&Internal::Original(name.clone()))
                        .unwrap();
                    let mut net = compiled.create_net();
                    let child_net = compiled.get_with_name(&internal_name).unwrap();
                    let tree = net.inject_net(child_net).with_type(ty.clone());
                    *readback_state = Some(ReadbackState::initialize(
                        ui,
                        net,
                        tree,
                        Arc::new(TokioSpawn),
                        &program,
                    ));
                }
            }
        }
    }

    fn recompile(&mut self) {
        stacker::grow(32 * 1024 * 1024, || {
            self.compiled = Some(Compiled::from_string(self.code.as_str()));
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
                            egui::menu::menu_custom_button(
                                ui,
                                egui::Button::new(
                                    egui::RichText::new("Run")
                                        .strong()
                                        .color(egui::Color32::BLACK),
                                )
                                .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                                |ui| {
                                    egui::ScrollArea::vertical().show(ui, |ui| {
                                        Self::readback(
                                            &mut self.readback_state,
                                            ui,
                                            checked.program.clone(),
                                            ic_compiled,
                                        );
                                    })
                                },
                            );
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
                                    if let Some(rb) = &mut self.readback_state {
                                        rb.show_readback(ui, checked.program.clone())
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

/// Create a `LabeledSpan` without a label at `loc`
pub fn labels_from_loc<'s>(code: &'s str, loc: &Loc) -> Vec<LabeledSpan> {
    match loc {
        Loc::Code { line, column } => vec![LabeledSpan::new_with_span(
            None,
            SourceOffset::from_location(&code, *line, *column),
        )],
        Loc::External => vec![],
    }
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
                let labels = labels_from_loc(&code, loc);
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

            Self::Type(error) => format!("{:?}", error.into_report(code)),

            Self::InetCompile(err) => {
                format!("inet compilation error: {}", err.display(&code))
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
            "let",
            "do",
            "in",
            "pass",
            "begin",
            "unfounded",
            "loop",
            "telltypes",
            "either",
            "recursive",
            "iterative",
            "self",
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

fn red() -> egui::Color32 {
    egui::Color32::from_hex("#DE3C4B").unwrap()
}

fn green() -> egui::Color32 {
    egui::Color32::from_hex("#7ac74f").unwrap()
}

fn blue() -> egui::Color32 {
    egui::Color32::from_hex("#118ab2").unwrap()
}

static DEFAULT_CODE: &str = include_str!("../examples/sample.par");
