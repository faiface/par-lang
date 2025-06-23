use crate::playground::Playground;
use clap::{arg, command, value_parser, Command};
use eframe::egui;
use std::path::PathBuf;

mod icombs;
mod language_server;
mod location;
mod par;
mod playground;
mod readback;
mod spawn;

fn main() {
    let matches = command!()
        .subcommand_required(true)
        .subcommand(
            Command::new("playground")
                .about("Start the Par playground")
                .arg(
                    arg!([file] "Open a Par file in the playground")
                        .value_parser(value_parser!(PathBuf)),
                ),
        )
        .subcommand(
            Command::new("run")
                .about("Run a Par file in the playground")
                .arg(arg!(<file> "The Par file to run").value_parser(value_parser!(PathBuf)))
                .arg(arg!([function] "The function to run").default_value("main")),
        )
        .subcommand(
            Command::new("lsp").about("Start the Par language server for editor integration"),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("playground", args)) => {
            let file = args.get_one::<PathBuf>("file");
            run_playground(file.cloned());
        }
        Some(("run", args)) => {
            let file = args.get_one::<PathBuf>("file").unwrap().clone();
            let function = args.get_one::<String>("function").unwrap().clone();
            run_function(file, function);
        }
        Some(("lsp", _)) => run_language_server(),
        _ => unreachable!(),
    }
}

fn run_playground(file: Option<PathBuf>) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        let options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
            ..Default::default()
        };

        par::parse::set_miette_hook();

        eframe::run_native(
            "⅋layground",
            options,
            Box::new(|cc| Ok(Playground::new(cc, file))),
        )
        .expect("egui crashed");
    });
}

// todo: this does not work
fn run_function(_file: PathBuf, _function: String) {
    /*let Ok(code) = File::open(file).and_then(|mut file| {
        use std::io::Read;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Ok(buf)
    }) else {
        println!("{}", "Could not read file".bright_red());
        return;
    };

    let mut interact: Option<playground::Interact> = None;

    let Ok(compiled) = stacker::grow(32 * 1024 * 1024, || Compiled::from_string(&code)) else {
        println!("Compilation failed");
        return;
    };
    let compiled_code = code.into();
    let program = compiled.program.clone();
    let Some(definition) = program
        .definitions
        .iter()
        .find(|definition| match &definition.name {
            Internal::Original(name) => name.string == function,
            _ => false,
        })
        .cloned()
    else {
        println!("{}: {}", "Function not found".bright_red(), function);
        return;
    };

    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        let options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
            ..Default::default()
        };

        eframe::run_simple_native(
            "⅋layground - Run",
            options,
            move |ctx, _frame| {
                egui::CentralPanel::default().show(ctx, |ui| {
                    let name = definition.name.to_string();
                    if ui.button(&name).clicked() {
                        if let Some(int) = interact.take() {
                            int.handle.lock().expect("lock failed").cancel();
                        }
                        interact = Some(playground::Interact {
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
                                &definition.expression,
                            ),
                        });
                    }
                });
            }
            // 417, 339, 508
        )
            .expect("egui crashed");
    })*/
}

fn run_language_server() {
    language_server::language_server_main::main()
}
