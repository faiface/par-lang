use crate::par::build_result::BuildConfig;
use crate::par::build_result::BuildResult;
use crate::par::types::Type;
#[cfg(feature = "playground")]
use crate::playground::Playground;
use crate::runtime::compiler::Backend;
use crate::spawn::TokioSpawn;
use clap::{arg, command, value_parser, Command};
use colored::Colorize;
#[cfg(feature = "playground")]
use eframe::egui;
use tokio::time::Instant;

use std::fs::File;
#[cfg(feature = "playground")]
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;

mod language_server;
mod location;
mod par;
#[cfg(feature = "playground")]
mod playground;
#[cfg(feature = "playground")]
mod readback;
pub mod runtime;
mod spawn;
mod test;
mod test_assertion;
mod test_runner;

fn main() {
    let matches = command!()
        .subcommand_required(true)
        .subcommand(
            Command::new("playground")
                .about(if cfg!(feature = "playground") {
                    "Start the Par playground"
                } else {
                    "Disabled in build"
                })
                .arg(
                    arg!([file] "Open a Par file in the playground")
                        .value_parser(value_parser!(PathBuf)),
                ),
        )
        .subcommand(
            Command::new("run")
                .about("Run a Par file in the CLI")
                .arg(arg!(<file> "The Par file to run").value_parser(value_parser!(PathBuf)))
                .arg(arg!([definition] "The definition to run").default_value("Main"))
                .arg(arg!(-f --flag <FLAG> ... "Set a flag")),
        )
        .subcommand(
            Command::new("check")
                .about("Type check a Par file in the CLI")
                .arg(
                    arg!(<file> "The Par file(s) to check")
                        .num_args(1..)
                        .value_parser(value_parser!(PathBuf)),
                )
                .arg(arg!(-f --flag <FLAG> ... "Set a flag")),
        )
        .subcommand(
            Command::new("lsp")
                .about("Start the Par language server for editor integration")
                .arg(arg!(--stdio "Run lsp over stdio (default behavior)")),
        )
        .subcommand(
            Command::new("test")
                .about("Run Par tests")
                .arg(
                    arg!([file] "Run tests in a specific file")
                        .value_parser(value_parser!(PathBuf)),
                )
                .arg(arg!(--filter <FILTER> "Only run tests matching this filter").required(false))
                .arg(arg!(-f --flag <FLAG> ... "Set a flag")),
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
            let flags = args.get_many::<String>("flag").into_iter().flatten();
            run_definition(&flags.into(), file, definition);
        }
        Some(("check", args)) => {
            let files = args.get_many::<PathBuf>("file").unwrap().clone();
            let flags = args.get_many::<String>("flag").into_iter().flatten();
            let flags = BuildConfig::from(flags);
            for file in files {
                println!("Checking file: {}", file.display());
                let _ = check(&flags, file.clone());
            }
        }
        Some(("lsp", _)) => run_language_server(),
        Some(("test", args)) => {
            let file = args.get_one::<PathBuf>("file");
            let filter = args.get_one::<String>("filter");
            let flags = args.get_many::<String>("flag").into_iter().flatten();
            run_tests(&flags.into(), file.cloned(), filter.cloned());
        }
        _ => unreachable!(),
    }
}

#[cfg(feature = "playground")]
/// String to save on crash. Used by the playground to avoid losing everything on panic.
static CRASH_STR: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

#[cfg(not(feature = "playground"))]
fn run_playground(_: Option<PathBuf>) {
    eprintln!("Playground was disabled when building Par")
}

#[cfg(feature = "playground")]
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

fn run_definition(config: &BuildConfig, file: PathBuf, definition: String) {
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

        let build = stacker::grow(32 * 1024 * 1024, || {
            BuildResult::from_source(config, &code, file.into())
        });
        if let Some(error) = build.error() {
            println!("{}", error.display(Arc::from(code.as_str())).bright_red());
            return;
        }

        let Some(checked) = build.checked() else {
            println!("Type check failed");
            return;
        };

        let Some((name, _)) = checked
            .definitions
            .iter()
            .find(|(name, _)| name.primary == definition)
            .clone()
        else {
            println!("{}: {}", "Definition not found".bright_red(), definition);
            return;
        };

        let Some(rt_compiled) = build.rt_compiled() else {
            println!(
                "{}: {}",
                "Runtime compilation failed".bright_red(),
                definition
            );
            return;
        };

        let Some(Type::Break(_)) = rt_compiled.get_type_of(name) else {
            println!(
                "{}: {}",
                "Definition does not have the unit (!) type".bright_red(),
                definition
            );
            return;
        };

        eprintln!("Running...");
        let start = Instant::now();

        let (root, reducer_future) = rt_compiled.start_and_instantiate(name).await;

        root.continue_().await;
        reducer_future.await;
        let end = Instant::now();
        eprintln!("Took: {:?}", end - start);
        if let Backend::New(new) = &rt_compiled.backend {
            eprintln!("Arena size: {}", new.arena.memory_size());
        }
        eprintln!("Done :)");
    });
}

fn check(config: &BuildConfig, file: PathBuf) -> Result<(), String> {
    let Ok(code) = File::open(&file).and_then(|mut file| {
        use std::io::Read;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Ok(buf)
    }) else {
        println!("{} {}", "Could not read file:".bright_red(), file.display());
        return Err(format!("Could not read file: {}", file.display()));
    };

    let build = stacker::grow(32 * 1024 * 1024, || {
        BuildResult::from_source(&config, &code, file.into())
    });
    if let Some(error) = build.error() {
        let error_string = error.display(Arc::from(code.as_str()));
        println!("{}", error_string.bright_red());
        return Err(error_string);
    }
    Ok(())
}

fn run_language_server() {
    language_server::language_server_main::main()
}

fn run_tests(config: &BuildConfig, file: Option<PathBuf>, filter: Option<String>) {
    test_runner::run_tests(config, file, filter);
}
