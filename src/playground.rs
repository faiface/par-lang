use std::{
    collections::BTreeSet,
    fmt::Write,
    sync::{Arc, Mutex},
};

use eframe::egui;
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};
use indexmap::IndexMap;

use crate::{
    interact::{Event, Handle, Request},
    par::{
        language::{CompileError, Internal},
        parse::{parse_program, Loc, Name, ParseError, Program},
        process::Expression,
        runtime::{self, Context, Operation},
        types::{self, TypeError},
    },
    spawn::TokioSpawn,
};

pub struct Playground {
    code: String,
    compiled: Option<Compiled>,
    interact: Option<Interact>,
    editor_font_size: f32,
    show_compiled: bool,
}

#[derive(Clone, Debug)]
enum Error {
    Parse(ParseError),
    Compile(CompileError<Loc>),
    Type(TypeError<Loc, Internal<Name>>),
    Runtime(runtime::Error<Loc, Internal<Name>>),
}

#[derive(Clone)]
struct Compiled {
    code: Arc<str>,
    program: Result<Program<Internal<Name>, Arc<Expression<Loc, Internal<Name>, ()>>>, Error>,
    pretty: Option<String>,
}

#[derive(Clone)]
struct Interact {
    code: Arc<str>,
    handle: Arc<Mutex<Handle<Loc, Internal<Name>, ()>>>,
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
        Box::new(Self {
            code: DEFAULT_CODE.to_string(),
            compiled: None,
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
    fn get_theme(&self, ui: &egui::Ui) -> ColorTheme {
        if ui.visuals().dark_mode {
            fix_dark_theme(ColorTheme::GRUVBOX)
        } else {
            fix_light_theme(ColorTheme::GITHUB_LIGHT)
        }
    }

    fn show_interaction(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal_top(|ui| {
                ui.add_space(5.0);

                if ui.button(egui::RichText::new("Compile").strong()).clicked() {
                    let program = parse_program(self.code.as_str())
                        .map_err(|error| Error::Parse(error))
                        .and_then(|program| {
                            let type_defs = program
                                .type_defs
                                .into_iter()
                                .map(|(name, (params, typ))| {
                                    (
                                        Internal::Original(name),
                                        (
                                            params.into_iter().map(Internal::Original).collect(),
                                            typ.map_names(&mut Internal::Original),
                                        ),
                                    )
                                })
                                .collect();
                            let declarations = program
                                .declarations
                                .into_iter()
                                .map(|(name, option_typ)| {
                                    (
                                        Internal::Original(name),
                                        option_typ
                                            .map(|typ| typ.map_names(&mut Internal::Original)),
                                    )
                                })
                                .collect();
                            let compile_result = program
                                .definitions
                                .into_iter()
                                .map(|(name, def)| {
                                    def.compile().map(|compiled| {
                                        (
                                            Internal::Original(name.clone()),
                                            compiled.optimize().fix_captures(&IndexMap::new()).0,
                                        )
                                    })
                                })
                                .collect::<Result<_, CompileError<Loc>>>();
                            match compile_result {
                                Ok(compiled) => Ok(Program {
                                    type_defs,
                                    declarations,
                                    definitions: compiled,
                                }),
                                Err(error) => Err(Error::Compile(error)),
                            }
                        });

                    let pretty = program.as_ref().ok().map(|program| {
                        program
                            .definitions
                            .iter()
                            .map(|(name, def)| {
                                let mut buf = String::new();
                                write!(&mut buf, "define {} = ", name).expect("write failed");
                                def.pretty(&mut buf, 0).expect("write failed");
                                write!(&mut buf, "\n\n").expect("write failed");
                                buf
                            })
                            .collect()
                    });

                    self.compiled = Some(Compiled {
                        code: Arc::from(self.code.as_str()),
                        program,
                        pretty,
                    });
                }

                if let Some(Compiled {
                    code,
                    program: program_result,
                    pretty,
                }) = &mut self.compiled
                {
                    if let Ok(mut program) = program_result.clone() {
                        if pretty.is_some() {
                            ui.checkbox(
                                &mut self.show_compiled,
                                egui::RichText::new("Show compiled"),
                            );
                        }

                        if ui
                            .button(egui::RichText::new("Typecheck").strong())
                            .clicked()
                        {
                            for (name, expression) in &program.definitions {
                                let mut context = types::Context::new(
                                    Arc::new(program.type_defs.clone()),
                                    Arc::new(types::Declarations(program.declarations.clone())),
                                );
                                match program.declarations.get(name) {
                                    Some(Some(declaration)) => {
                                        match context.check_expression(
                                            None,
                                            expression,
                                            declaration,
                                        ) {
                                            Ok(_) => continue,
                                            Err(error) => {
                                                *program_result = Err(Error::Type(error));
                                                break;
                                            }
                                        }
                                    }
                                    Some(None) | None => {
                                        match context.infer_expression(None, expression) {
                                            Ok((_, inferred_type)) => {
                                                program
                                                    .declarations
                                                    .insert(name.clone(), Some(inferred_type));
                                            }
                                            Err(error) => {
                                                *program_result = Err(Error::Type(error));
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        egui::menu::menu_custom_button(
                            ui,
                            egui::Button::new(
                                egui::RichText::new("Run")
                                    .strong()
                                    .color(egui::Color32::BLACK),
                            )
                            .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                            |ui| {
                                for (internal_name, expression) in &program.definitions {
                                    if let Internal::Original(name) = internal_name {
                                        if ui.button(&name.string).clicked() {
                                            if let Some(int) = self.interact.take() {
                                                int.handle.lock().expect("lock failed").cancel();
                                            }
                                            self.interact = Some(Interact {
                                                code: Arc::clone(&code),
                                                handle: Handle::start_expression(
                                                    Arc::new({
                                                        let ctx = ui.ctx().clone();
                                                        move || ctx.request_repaint()
                                                    }),
                                                    Context::new(
                                                        Arc::new(TokioSpawn),
                                                        Arc::new(program.definitions.clone()),
                                                    ),
                                                    expression,
                                                ),
                                            });
                                            ui.close_menu();
                                        }
                                    }
                                }
                            },
                        );
                    }
                }
            });

            ui.separator();

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::both().show(ui, |ui| {
                    if let Some(Compiled {
                        code,
                        program: Err(error),
                        ..
                    }) = &self.compiled
                    {
                        ui.label(egui::RichText::new(error.display(code)).color(red()).code());
                    }

                    let theme = self.get_theme(ui);
                    if let Some(Compiled {
                        pretty: Some(pretty),
                        ..
                    }) = &mut self.compiled
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
                        }
                    }

                    if let Some(int) = &self.interact {
                        self.show_interact(ui, int.clone());
                    }
                });
            });
        });
    }

    fn show_interact(&mut self, ui: &mut egui::Ui, int: Interact) {
        let handle = int.handle.lock().expect("lock failed");

        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4.0))
            .outer_margin(egui::Margin::same(2.0))
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
                                            Error::Runtime(error).display(&int.code),
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

impl Error {
    pub fn display(&self, code: &str) -> String {
        match self {
            Self::Parse(error) => {
                format!(
                    "{}\nSyntax error.",
                    Self::display_loc(code, &error.location)
                )
            }

            Self::Compile(CompileError::PassNotPossible(loc)) => {
                format!("{}\nNothing to `pass` to.", Self::display_loc(code, loc))
            }

            Self::Compile(CompileError::MustEndProcess(loc)) => {
                format!("{}\nThis process must end.", Self::display_loc(code, loc))
            }

            Self::Compile(CompileError::CannotEndInDoExpression(loc)) => {
                format!(
                    "{}\nCannot end process in `do` expression.",
                    Self::display_loc(code, loc)
                )
            }

            Self::Type(error) => error.pretty(|loc| Self::display_loc(code, loc)),

            Self::Runtime(error) => Self::display_runtime_error(code, error),
        }
    }

    fn display_runtime_error(code: &str, error: &runtime::Error<Loc, Internal<Name>>) -> String {
        use runtime::Error::*;
        match error {
            NameNotDefined(loc, name) => {
                format!(
                    "{}\n`{}` is not defined.",
                    Self::display_loc(code, loc),
                    name
                )
            }
            ShadowedObligation(loc, name) => {
                format!(
                    "{}\nCannot re-assign `{}` before handling it.",
                    Self::display_loc(code, loc),
                    name
                )
            }
            UnfulfilledObligations(loc, names) => {
                format!(
                    "{}\nCannot end this process before handling {}.",
                    Self::display_loc(code, loc),
                    names
                        .iter()
                        .enumerate()
                        .map(|(i, name)| if i == 0 {
                            format!("`{}`", name)
                        } else {
                            format!(", `{}`", name)
                        })
                        .collect::<String>()
                )
            }
            IncompatibleOperations(op1, op2) => {
                format!(
                    "{}\n\n{}\n\nThese operations are incompatible.",
                    Self::display_operation(code, op1),
                    Self::display_operation(code, op2),
                )
            }
            NoSuchLoopPoint(loc, _) => {
                format!(
                    "{}\nThere is no matching loop point in scope.",
                    Self::display_loc(code, loc),
                )
            }
            Multiple(error1, error2) => {
                format!(
                    "{}\n\n\n{}",
                    Self::display_runtime_error(code, error1),
                    Self::display_runtime_error(code, error2),
                )
            }
        }
    }

    fn display_operation(code: &str, op: &Operation<Loc, Internal<Name>>) -> String {
        match op {
            Operation::Unknown(loc) => {
                format!("{}\nUnknown operation.", Self::display_loc(code, loc))
            }
            Operation::Send(loc) => {
                format!(
                    "{}\nThis side is sending a value.",
                    Self::display_loc(code, loc),
                )
            }
            Operation::Receive(loc) => {
                format!(
                    "{}\nThis side is receiving a value.",
                    Self::display_loc(code, loc),
                )
            }
            Operation::Choose(loc, chosen) => {
                format!(
                    "{}\nThis side is choosing `{}`.",
                    Self::display_loc(code, loc),
                    chosen,
                )
            }
            Operation::Match(loc, choices) => {
                format!(
                    "{}\nThis side is offering either of {}.",
                    Self::display_loc(code, loc),
                    choices
                        .iter()
                        .enumerate()
                        .map(|(i, name)| if i == 0 {
                            format!("`{}`", name)
                        } else {
                            format!(", `{}`", name)
                        })
                        .collect::<String>(),
                )
            }
            Operation::Break(loc) => {
                format!("{}\nThis side is breaking.", Self::display_loc(code, loc))
            }
            Operation::Continue(loc) => {
                format!("{}\nThis side is continuing.", Self::display_loc(code, loc),)
            }
        }
    }

    fn display_loc(code: &str, loc: &Loc) -> String {
        match loc {
            Loc::External => format!("<UI>"),
            Loc::Code { line, column } => {
                let line_of_code = match code.lines().nth(line - 1) {
                    Some(loc) => loc,
                    None => return format!("<invalid location {}:{}>", line, column),
                };
                let line_number = format!("{}| ", line);
                format!(
                    "{}{}\n{}{}^",
                    line_number,
                    line_of_code,
                    " ".repeat(line_number.len()),
                    " ".repeat(column - 1),
                )
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
            "type",
            "declare",
            "define",
            "chan",
            "let",
            "do",
            "in",
            "pass",
            "begin",
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

static DEFAULT_CODE: &str = include_str!("sample.par");
