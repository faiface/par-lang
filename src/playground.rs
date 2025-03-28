use std::{
    collections::BTreeSet,
    fmt::Write,
    fs::File,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use eframe::egui;
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};
use indexmap::IndexMap;

use crate::{
    interact::{Event, Handle, Request},
    par::{
        language::{CompileError, Internal, Name, Program},
        parse::{parse_program, SyntaxError},
        process,
        runtime::{self, Context, Operation},
        types::{self, Type, TypeError},
    },
    spawn::TokioSpawn,
};
use miette::{LabeledSpan, SourceOffset, SourceSpan};
use crate::location::Span;
use crate::par::language::{Declaration, Definition, TypeDef};

pub struct Playground {
    file_path: Option<PathBuf>,
    code: String,
    compiled: Option<Result<Compiled, Error>>,
    compiled_code: Arc<str>,
    interact: Option<Interact>,
    editor_font_size: f32,
    show_compiled: bool,
}

#[derive(Clone)]
pub(crate) struct Compiled {
    pub(crate) program: Program<Internal<Name>, Arc<process::Expression<Internal<Name>, ()>>>,
    pub(crate) pretty: String,
    pub(crate) checked: Result<Checked, TypeError<Internal<Name>>>,
}

impl Compiled {
    pub(crate) fn from_string(source: &str) -> Result<Compiled, Error> {
        parse_program(source)
            .map_err(Error::Parse)
            .and_then(|program| {
                let type_defs = program
                    .type_defs
                    .into_iter()
                    .map(|TypeDef { span, name, params, typ }| {
                        TypeDef {
                            span,
                            name: Internal::Original(name),
                            params: params.into_iter().map(Internal::Original).collect(),
                            typ: typ.map_names(&mut Internal::Original),
                        }
                    })
                    .collect();
                let declarations = program
                    .declarations
                    .into_iter()
                    .map(|Declaration { span, name, typ }| {
                        Declaration {
                            span,
                            name: Internal::Original(name),
                            typ: typ.map_names(&mut Internal::Original),
                        }
                    })
                    .collect();
                let compile_result = program
                    .definitions
                    .into_iter()
                    .map(|Definition { span, name, expression }| {
                        expression.compile().map(|compiled| {
                            Definition {
                                span,
                                name: Internal::Original(name.clone()),
                                expression: compiled.optimize().fix_captures(&IndexMap::new()).0,
                            }
                        })
                    })
                    .collect::<Result<_, CompileError>>();
                match compile_result {
                    Ok(compiled) => Ok(Compiled::from_program(Program {
                        type_defs,
                        declarations,
                        definitions: compiled,
                    })),
                    Err(error) => Err(Error::Compile(error)),
                }
            })
    }

    pub(crate) fn from_program(
        program: Program<Internal<Name>, Arc<process::Expression<Internal<Name>, ()>>>,
    ) -> Self {
        let pretty = program
            .definitions
            .iter()
            .map(|Definition { name, expression: def, .. }| {
                let mut buf = String::new();
                write!(&mut buf, "define {} = ", name).expect("write failed");
                def.pretty(&mut buf, 0).expect("write failed");
                write!(&mut buf, "\n\n").expect("write failed");
                buf
            })
            .collect();

        // attempt to type check
        let definitions = match types::Context::new_with_type_checking(&program) {
            Ok(context) => context.get_checked_definitions(),
            Err(error) => {
                return Compiled {
                    program,
                    pretty,
                    checked: Err(error),
                }
            }
        };
        let new_program = Program {
            type_defs: program.type_defs.clone(),
            declarations: program.declarations.clone(),
            definitions,
        };
        Compiled {
            program,
            pretty,
            checked: Ok(Checked::from_program(new_program)),
        }
    }
}

type CheckedProgram = Program<Internal<Name>, Arc<process::Expression<Internal<Name>, Type<Internal<Name>>>>>;

#[derive(Clone)]
pub(crate) struct Checked {
    pub(crate) program: CheckedProgram,
}

impl Checked {
    pub(crate) fn from_program(
        // not used for anything, so there's no reason to store it ATM.
        program: CheckedProgram,
    ) -> Self {
        Self {
            program
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Error {
    Parse(SyntaxError),
    Compile(CompileError),
    Type(TypeError<Internal<Name>>),
    Runtime(runtime::Error<Span, Internal<Name>>),
}

#[derive(Clone)]
pub(crate) struct Interact {
    pub(crate) code: Arc<str>,
    pub(crate) handle: Arc<Mutex<Handle<Span, Internal<Name>, ()>>>,
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
            interact: None,
            editor_font_size: 16.0,
            show_compiled: false,
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
            fix_dark_theme(ColorTheme::GITHUB_DARK)
        } else {
            fix_light_theme(ColorTheme::GITHUB_LIGHT)
        }
    }

    fn run(
        interact: &mut Option<Interact>,
        ui: &mut egui::Ui,
        program: &Program<Internal<Name>, Arc<process::Expression<Internal<Name>, ()>>>,
        compiled_code: Arc<str>,
    ) {
        egui::ScrollArea::vertical().show(ui, |ui| {
            for Definition { name: internal_name, expression, .. } in &program.definitions {
                if let Internal::Original(name) = internal_name {
                    if ui.button(&name.string).clicked() {
                        if let Some(int) = interact.take() {
                            int.handle.lock().expect("lock failed").cancel();
                        }
                        *interact = Some(Interact {
                            code: Arc::clone(&compiled_code),
                            handle: Handle::start_expression(
                                Arc::new({
                                    let ctx = ui.ctx().clone();
                                    move || ctx.request_repaint()
                                }),
                                Context::new(
                                    Arc::new(TokioSpawn),
                                    Arc::new(
                                        program
                                            .definitions
                                            .iter()
                                            .map(|Definition { name, expression, .. }| (name.clone(), expression.clone()))
                                            .collect(),
                                    ),
                                ),
                                expression,
                            ),
                        });
                        ui.close_menu();
                    }
                }
            }
        });
    }

    fn recompile(&mut self) {
        self.compiled = stacker::grow(32 * 1024 * 1024, || {
            Some(Compiled::from_string(self.code.as_str()))
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

                if let Some(Ok(Compiled { program, .. })) = &mut self.compiled {
                    ui.checkbox(
                        &mut self.show_compiled,
                        egui::RichText::new("Show compiled"),
                    );

                    if !self.show_compiled {
                        egui::menu::menu_custom_button(
                            ui,
                            egui::Button::new(
                                egui::RichText::new("Run")
                                    .strong()
                                    .color(egui::Color32::BLACK),
                            )
                            .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                            |ui| {
                                Self::run(
                                    &mut self.interact,
                                    ui,
                                    program,
                                    self.compiled_code.clone(),
                                );
                            },
                        );
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
                        } else if let Ok(_) = checked {
                            // :)
                            ui.label(
                                egui::RichText::new("Type checking successful").color(green()),
                            );
                        } else if let Err(err) = checked {
                            let error =
                                Error::Type(err.clone()).display(self.compiled_code.clone());

                            ui.label(egui::RichText::new(error).color(red()).code());
                        }
                    }
                    if !self.show_compiled {
                        if let Some(int) = &self.interact {
                            self.show_interact(ui, int.clone());
                        }
                    }
                });
            });
        });
    }

    fn show_interact(&mut self, ui: &mut egui::Ui, int: Interact) {
        let handle = int.handle.lock().expect("lock failed");

        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4))
            .outer_margin(egui::Margin::same(2))
            .show(ui, |ui| {
                ui.horizontal_top(|ui| {
                    let mut to_the_side = Vec::new();

                    ui.vertical(|ui| {
                        for event in handle.events() {
                            match event {
                                Event::Send(_, argument) => {
                                    self.show_interact(
                                        ui,
                                        Interact {
                                            code: Arc::clone(&int.code),
                                            handle: Arc::clone(&argument),
                                        },
                                    );
                                }

                                Event::Receive(_, parameter) => {
                                    to_the_side.push(Arc::clone(&parameter))
                                }

                                Event::Choose(_, chosen) => {
                                    ui.horizontal(|ui| {
                                        ui.label(
                                            egui::RichText::new("+").strong().code().color(blue()),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!("{}", chosen))
                                                .strong()
                                                .code(),
                                        );
                                    });
                                }

                                Event::Either(_, chosen) => {
                                    ui.horizontal(|ui| {
                                        ui.label(
                                            egui::RichText::new(">").strong().code().color(green()),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!("{}", chosen))
                                                .strong()
                                                .code(),
                                        );
                                    });
                                }

                                Event::Break(_) => {
                                    ui.horizontal(|ui| {
                                        ui.label(egui::RichText::new("break").italics().code());
                                    });
                                }

                                Event::Continue(_) => {
                                    ui.horizontal(|ui| {
                                        ui.label(egui::RichText::new("continue").italics().code());
                                    });
                                }
                            }
                        }

                        if let Some(result) = handle.interaction() {
                            ui.horizontal(|ui| match result {
                                Ok(Request::Dynamic(_)) => {
                                    ui.horizontal(|ui| {
                                        drop(handle);
                                        ui.label(
                                            egui::RichText::new("<UI>")
                                                .strong()
                                                .code()
                                                .color(red()),
                                        );
                                    });
                                }
                                Ok(Request::Either(loc, choices)) => {
                                    ui.vertical(|ui| {
                                        drop(handle);
                                        for choice in choices.iter() {
                                            if ui
                                                .button(
                                                    egui::RichText::new(format!("{}", choice))
                                                        .strong(),
                                                )
                                                .clicked()
                                            {
                                                Handle::choose(
                                                    Arc::clone(&int.handle),
                                                    loc.clone(),
                                                    choice.clone(),
                                                );
                                            }
                                        }
                                    });
                                }
                                Err(error) => {
                                    ui.label(
                                        egui::RichText::new(
                                            Error::Runtime(error).display(int.code.clone()),
                                        )
                                        .color(red())
                                        .code(),
                                    );
                                }
                            });
                        }
                    });

                    for side in to_the_side {
                        self.show_interact(
                            ui,
                            Interact {
                                code: Arc::clone(&int.code),
                                handle: side,
                            },
                        );
                    }
                });
            });
    }
}

/// Create a `LabeledSpan` without a label at `span`
pub fn labels_from_span(code: &str, span: &Span) -> Vec<LabeledSpan> {
    vec![LabeledSpan::new_with_span(
        None,
        SourceSpan::new(SourceOffset::from(span.start.offset), span.len())
    )]
}
pub fn span_to_source_span(code: &str, span: &Span) -> Option<SourceSpan> {
    Some(SourceSpan::new(SourceOffset::from(span.start.offset), span.len()))
}

#[derive(Debug, miette::Diagnostic)]
struct RuntimeError {
    #[label]
    span: Option<SourceSpan>,
    #[label(collection)]
    others: Vec<LabeledSpan>,
    #[related]
    related: Vec<miette::ErrReport>,
    message: String,
}
impl core::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("Runtime Error: {}.", self.message).fmt(f)
    }
}
impl core::error::Error for RuntimeError {}

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

            Self::Runtime(error) => format!(
                "{:?}",
                miette::Report::from(Self::display_runtime_error(&code, error))
            ),
        }
    }

    fn display_runtime_error(
        code: &str,
        error: &runtime::Error<Span, Internal<Name>>,
    ) -> RuntimeError {
        use runtime::Error::*;
        match error {
            NameNotDefined(loc, name) => RuntimeError {
                span: span_to_source_span(code, loc),
                related: Vec::new(),
                others: Vec::new(),
                message: format!("`{}` is not defined.", name),
            },
            ShadowedObligation(loc, name) => RuntimeError {
                span: span_to_source_span(code, loc),
                related: Vec::new(),
                others: Vec::new(),
                message: format!("Cannot re-assign `{}` before handling it.", name),
            },
            UnfulfilledObligations(loc, names) => RuntimeError {
                span: span_to_source_span(code, loc),
                related: Vec::new(),
                others: Vec::new(),
                message: format!(
                    "Cannot end this process before handling {}.",
                    names
                        .iter()
                        .enumerate()
                        .map(|(i, name)| if i == 0 {
                            format!("`{}`", name)
                        } else {
                            format!(", `{}`", name)
                        })
                        .collect::<String>()
                ),
            },
            IncompatibleOperations(op1, op2) => RuntimeError {
                span: None,
                related: Vec::new(),
                others: [
                    Self::display_operation(code, op1),
                    Self::display_operation(code, op2),
                ]
                .into_iter()
                .flatten()
                .collect(),
                message: "These operations are incompatible.".to_owned(),
            },
            NoSuchLoopPoint(loc, _) => RuntimeError {
                span: span_to_source_span(code, loc),
                others: Vec::new(),
                related: Vec::new(),
                message: "There is no matching loop point in scope.".to_owned(),
            },
            Multiple(error1, error2) => RuntimeError {
                span: None,
                others: Vec::new(),
                related: vec![
                    miette::Report::from(Self::display_runtime_error(code, error1)),
                    miette::Report::from(Self::display_runtime_error(code, error2)),
                ],
                message: "multiple errors".to_owned(),
            },
        }
    }

    fn display_operation(code: &str, op: &Operation<Span, Internal<Name>>) -> Vec<LabeledSpan> {
        match op {
            Operation::Unknown(loc) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some("Unknown operation.".to_owned()));
                    x
                })
                .collect(),
            Operation::Send(loc) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some("This side is sending a value.".to_owned()));
                    x
                })
                .collect(),
            Operation::Receive(loc) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some("This side is receiving a value.".to_owned()));
                    x
                })
                .collect(),
            Operation::Choose(loc, chosen) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some(format!("This side is choosing `{}`.", chosen)));
                    x
                })
                .collect(),
            Operation::Match(loc, choices) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some(format!(
                        "This side is offering either of {}.",
                        choices
                            .iter()
                            .enumerate()
                            .map(|(i, name)| if i == 0 {
                                format!("`{}`", name)
                            } else {
                                format!(", `{}`", name)
                            })
                            .collect::<String>(),
                    )));
                    x
                })
                .collect(),
            Operation::Break(loc) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some("This side is breaking.".to_owned()));
                    x
                })
                .collect(),
            Operation::Continue(loc) => labels_from_span(code, loc)
                .into_iter()
                .map(|mut x| {
                    x.set_label(Some("This side is continuing.".to_owned()));
                    x
                })
                .collect(),
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
            "declare",
            "define",
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
