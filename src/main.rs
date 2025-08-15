use crate::icombs::readback::{TypedHandle, TypedReadback};
use crate::par::types::Type;
use crate::playground::{Compiled, Playground};
use crate::spawn::TokioSpawn;
use clap::{arg, command, value_parser, Command};
use colored::Colorize;
use eframe::egui;
use futures::task::SpawnExt;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;

mod icombs;
mod language_server;
mod location;
mod par;
mod playground;
mod readback;
mod spawn;
mod test_assertion;
mod test_runner;

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
                .about("Run a Par file in the CLI")
                .arg(arg!(<file> "The Par file to run").value_parser(value_parser!(PathBuf)))
                .arg(arg!([definition] "The definition to run").default_value("Main")),
        )
        .subcommand(
            Command::new("check")
                .about("Type check a Par file in the CLI")
                .arg(
                    arg!(<file> "The Par file(s) to check")
                        .num_args(1..)
                        .value_parser(value_parser!(PathBuf)),
                ),
        )
        .subcommand(
            Command::new("lsp").about("Start the Par language server for editor integration"),
        )
        .subcommand(
            Command::new("test")
                .about("Run Par tests")
                .arg(
                    arg!([file] "Run tests in a specific file")
                        .value_parser(value_parser!(PathBuf)),
                )
                .arg(arg!(--filter <FILTER> "Only run tests matching this filter").required(false)),
        )
        .get_matches_from(wild::args());

    match matches.subcommand() {
        Some(("playground", args)) => {
            let file = args.get_one::<PathBuf>("file");
            run_playground(file.cloned());
        }
        Some(("run", args)) => {
            let file = args.get_one::<PathBuf>("file").unwrap().clone();
            let definition = args.get_one::<String>("definition").unwrap().clone();
            run_definition(file, definition);
        }
        Some(("check", args)) => {
            let files = args.get_many::<PathBuf>("file").unwrap().clone();
            for file in files {
                println!("Checking file: {}", file.display());
                check(file.clone());
            }
        }
        Some(("lsp", _)) => run_language_server(),
        Some(("test", args)) => {
            let file = args.get_one::<PathBuf>("file");
            let filter = args.get_one::<String>("filter");
            run_tests(file.cloned(), filter.cloned());
        }
        _ => unreachable!(),
    }
}

/// String to save on crash. Used by the playground to avoid losing everything on panic.
static CRASH_STR: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

fn run_playground(file: Option<PathBuf>) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
        ..Default::default()
    };

    // Set hook for pretty-printer on error.
    par::parse::set_miette_hook();
    // Add hook to try printing current playground contents to stderr on error.
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let mut stderr = std::io::stderr().lock();
        if let Ok(crash_str) = CRASH_STR.lock() {
            if let Some(crash_str) = &*crash_str {
                // Ignore the error. We are already panicking.
                let _ = write!(
                    stderr,
                    "Panic in progress. This is a bug, please file an issue containing your code:\n```par\n{}\n```",
                    crash_str
                );
            }
        }
        hook(info)
    }));

    eframe::run_native(
        "⅋layground",
        options,
        Box::new(|cc| Ok(Playground::new(cc, file))),
    )
    .expect("egui crashed");
}

fn run_definition(file: PathBuf, definition: String) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        let Ok(code) = File::open(&file).and_then(|mut file| {
            use std::io::Read;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            Ok(buf)
        }) else {
            println!("{} {}", "Could not read file:".bright_red(), file.display());
            return;
        };

        let compiled = match stacker::grow(32 * 1024 * 1024, || Compiled::from_string(&code)) {
            Ok(compiled) => compiled,
            Err(err) => {
                println!("{}", err.display(Arc::from(code.as_str())).bright_red());
                return;
            }
        };

        let Ok(checked) = compiled.checked else {
            println!("Type check failed");
            return;
        };

        let program = checked.program;

        let Some((name, _)) = program
            .definitions
            .iter()
            .find(|(name, _)| name.primary == definition)
            .clone()
        else {
            println!("{}: {}", "Definition not found".bright_red(), definition);
            return;
        };

        let Some(ic_compiled) = checked.ic_compiled else {
            println!("{}: {}", "IC compilation failed".bright_red(), definition);
            return;
        };

        let ty = ic_compiled.get_type_of(name).unwrap();

        let Type::Break(_) = ty else {
            println!(
                "{}: {}",
                "Definition does not have the unit (!) type".bright_red(),
                definition
            );
            return;
        };

        let mut net = ic_compiled.create_net();
        let child_net = ic_compiled.get_with_name(name).unwrap();
        let tree = net.inject_net(child_net).with_type(ty.clone());

        let (net_wrapper, reducer_future) = net.start_reducer(Arc::new(TokioSpawn::new()));

        let spawner = Arc::new(TokioSpawn::new());
        let readback_future = spawner
            .spawn_with_handle(async move {
                let handle =
                    TypedHandle::from_wrapper(program.type_defs.clone(), net_wrapper, tree);
                loop {
                    match handle.readback().await {
                        TypedReadback::Break => {
                            break;
                        }
                        _ => {
                            panic!("Unexpected readback from a unit definition.");
                        }
                    }
                }
            })
            .unwrap();

        readback_future.await;
        reducer_future.await;
    });
}

fn check(file: PathBuf) {
    let Ok(code) = File::open(&file).and_then(|mut file| {
        use std::io::Read;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Ok(buf)
    }) else {
        println!("{} {}", "Could not read file:".bright_red(), file.display());
        return;
    };

    match stacker::grow(32 * 1024 * 1024, || Compiled::from_string(&code)) {
        Ok(_compiled) => (),
        Err(err) => {
            println!("{}", err.display(Arc::from(code.as_str())).bright_red());
            return;
        }
    };
}

fn run_language_server() {
    language_server::language_server_main::main()
}

fn run_tests(file: Option<PathBuf>, filter: Option<String>) {
    test_runner::run_tests(file, filter);
}
