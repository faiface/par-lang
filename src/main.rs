#[cfg(feature = "playground")]
use crate::playground::Playground;
use clap::{Command, arg, command, value_parser};
use colored::Colorize;
#[cfg(feature = "playground")]
use eframe::egui;
use par_builtin::import_builtins;
use par_core::{
    frontend::{
        ParseAndCompileError, Type, TypeError, compile_runtime, lower, parse, set_miette_hook,
        type_check,
    },
    runtime,
    runtime::RuntimeCompilerError,
};
use tokio::time::Instant;

#[cfg(feature = "playground")]
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::{fs::File, process::ExitCode};

mod language_server;
#[cfg(feature = "playground")]
mod playground;
#[cfg(feature = "playground")]
mod readback;
mod test;
mod test_runner;

const MAX_INTERACTIONS_DEFAULT: u32 = 10_000;

#[derive(Debug, Clone)]
enum BuildError {
    ParseAndCompile(ParseAndCompileError),
    Type(TypeError),
    InetCompile(RuntimeCompilerError),
}

impl BuildError {
    fn display(&self, code: Arc<str>) -> String {
        match self {
            Self::ParseAndCompile(ParseAndCompileError::Parse(error)) => {
                format!(
                    "{:?}",
                    miette::Report::from(error.to_owned()).with_source_code(code)
                )
            }
            Self::ParseAndCompile(ParseAndCompileError::Compile(error)) => {
                format!("{:?}", error.to_report(code))
            }
            Self::Type(error) => format!("{:?}", error.to_report(code)),
            Self::InetCompile(error) => format!("inet compilation error: {}", error.display(&code)),
        }
    }
}

fn build_checked(
    code: &str,
    file: &PathBuf,
) -> Result<par_core::frontend::CheckedModule, BuildError> {
    let parsed = parse(code, file.clone().into())
        .map_err(|error| BuildError::ParseAndCompile(ParseAndCompileError::Parse(error)))?;
    let mut lowered = lower(parsed)
        .map_err(|error| BuildError::ParseAndCompile(ParseAndCompileError::Compile(error)))?;
    import_builtins(&mut lowered);
    type_check(&lowered).map_err(BuildError::Type)
}

fn build_runtime(
    code: &str,
    file: &PathBuf,
    max_interactions: u32,
) -> Result<
    (
        par_core::frontend::CheckedModule,
        par_core::runtime::Compiled,
    ),
    BuildError,
> {
    let checked = build_checked(code, file)?;
    let rt_compiled =
        compile_runtime(&checked, max_interactions).map_err(BuildError::InetCompile)?;
    Ok((checked, rt_compiled))
}

fn main() -> ExitCode {
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
                )
                .arg(arg!(--max_interactions <MAX_INTERACTIONS> ... "Maximum number of interactions during compilation")
            .value_parser(value_parser!(u32))),
        )
        .subcommand(
            Command::new("run")
                .about("Run a Par file in the CLI")
                .arg(arg!(--stats "Print statistics after running the definition"))
                .arg(arg!(<file> "The Par file to run").value_parser(value_parser!(PathBuf)))
                .arg(arg!([definition] "The definition to run").default_value("Main"))
                .arg(arg!(-f --flag <FLAG> ... "Set a flag"))
                .arg(arg!(--max_interactions <MAX_INTERACTIONS> ... "Maximum number of interactions during compilation")
            .value_parser(value_parser!(u32))),
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
                .arg(arg!(-f --flag <FLAG> ... "Set a flag"))
                .arg(arg!(--max_interactions <MAX_INTERACTIONS> ... "Maximum number of interactions during compilation")
            .value_parser(value_parser!(u32))),
        )
        .get_matches_from(wild::args());

    match matches.subcommand() {
        Some(("playground", args)) => {
            let file = args.get_one::<PathBuf>("file");
            let max_interactions = args
                .get_one::<u32>("max_interactions")
                .cloned()
                .unwrap_or(MAX_INTERACTIONS_DEFAULT);
            run_playground(file.cloned(), max_interactions);
        }
        Some(("run", args)) => {
            let stats = *args.get_one::<bool>("stats").unwrap();
            let file = args.get_one::<PathBuf>("file").unwrap().clone();
            let definition = args.get_one::<String>("definition").unwrap().clone();
            let max_interactions = args
                .get_one::<u32>("max_interactions")
                .cloned()
                .unwrap_or(MAX_INTERACTIONS_DEFAULT);
            run_definition(file, definition, stats, max_interactions);
        }
        Some(("check", args)) => {
            let files = args.get_many::<PathBuf>("file").unwrap().clone();

            let mut results = Vec::new();
            for file in files {
                println!("Checking file: {}", file.display());
                results.push(check(file.clone()));
            }
            if results.iter().any(|r| r.is_err()) {
                return ExitCode::FAILURE;
            }
        }
        Some(("lsp", _)) => run_language_server(),
        Some(("test", args)) => {
            let file = args.get_one::<PathBuf>("file");
            let filter = args.get_one::<String>("filter");
            let max_interactions = args
                .get_one::<u32>("max_interactions")
                .cloned()
                .unwrap_or(MAX_INTERACTIONS_DEFAULT);
            if !run_tests(file.cloned(), filter.cloned(), max_interactions) {
                return ExitCode::FAILURE;
            }
        }
        _ => unreachable!(),
    }

    ExitCode::SUCCESS
}

#[cfg(feature = "playground")]
/// String to save on crash. Used by the playground to avoid losing everything on panic.
static CRASH_STR: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

#[cfg(not(feature = "playground"))]
fn run_playground(_: Option<PathBuf>) {
    eprintln!("Playground was disabled when building Par")
}

#[cfg(feature = "playground")]
fn run_playground(file: Option<PathBuf>, max_interactions: u32) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
        ..Default::default()
    };

    // Set hook for pretty-printer on error.
    set_miette_hook();
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
        Box::new(|cc| Ok(Playground::new(cc, file, max_interactions))),
    )
    .expect("egui crashed");
}

fn run_definition(file: PathBuf, definition: String, print_stats: bool, max_interactions: u32) {
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

        let (checked, rt_compiled) = match stacker::grow(32 * 1024 * 1024, || {
            build_runtime(&code, &file, max_interactions)
        }) {
            Ok(result) => result,
            Err(error) => {
                println!("{}", error.display(Arc::from(code.as_str())).bright_red());
                return;
            }
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

        let Some(Type::Break(_)) = rt_compiled.get_type_of(name) else {
            println!(
                "{}: {}",
                "Definition does not have the unit (!) type".bright_red(),
                definition
            );
            return;
        };

        let start = Instant::now();

        let (root, reducer_future) = runtime::start_and_instantiate(&rt_compiled, name).await;

        root.continue_();
        let stats = reducer_future.await;

        if print_stats {
            eprintln!("{}", stats.show(start.elapsed()));
            eprintln!("\tArena size: {}", rt_compiled.code.arena.memory_size());
        }
    });
}

fn check(file: PathBuf) -> Result<(), String> {
    let Ok(code) = File::open(&file).and_then(|mut file| {
        use std::io::Read;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Ok(buf)
    }) else {
        println!("{} {}", "Could not read file:".bright_red(), file.display());
        return Err(format!("Could not read file: {}", file.display()));
    };

    let checked_result = stacker::grow(32 * 1024 * 1024, || build_checked(&code, &file));
    if let Err(error) = checked_result {
        let error_string = error.display(Arc::from(code.as_str()));
        eprintln!("{}", error_string.bright_red());
        return Err(error_string);
    }
    Ok(())
}

fn run_language_server() {
    language_server::language_server_main::main()
}

fn run_tests(file: Option<PathBuf>, filter: Option<String>, max_interactions: u32) -> bool {
    test_runner::run_tests(file, filter, max_interactions)
}
