use crate::package_utils::{
    SourceLookup, find_local_module, format_with_source_span, local_module_slash_path,
    parse_target, source_for_fallback, source_for_type_error,
};
#[cfg(feature = "playground")]
use crate::playground::Playground;
use crate::workspace_support::default_workspace_from_path;
use clap::{Command, arg, command, value_parser};
use colored::Colorize;
#[cfg(feature = "playground")]
use eframe::egui;
use par_core::frontend::language::Universal;
use par_core::{
    frontend::{Type, TypeError, set_miette_hook},
    runtime::RuntimeCompilerError,
    workspace::{CheckedWorkspace, FileImportScope, ModulePath, PackageLoadError, WorkspaceError},
};
use tokio::time::Instant;

use par_runtime::linker::Linked;
use par_runtime::spawn::TokioSpawn;
use std::fmt::Display;
#[cfg(feature = "playground")]
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;
use std::sync::Arc;

#[cfg(not(target_family = "wasm"))]
mod language_server;
mod package_utils;
#[cfg(feature = "playground")]
mod playground;
#[cfg(feature = "playground")]
mod readback;
mod test;
mod test_runner;
mod tokio_factory;
#[cfg(target_family = "wasm")]
mod wasm_spawn;
mod workspace_support;

const MAX_INTERACTIONS_DEFAULT: u32 = 10_000;

#[derive(Debug, Clone)]
enum BuildError {
    Workspace(WorkspaceError),
    Type {
        error: TypeError<Universal>,
        file_scope: Option<FileImportScope<Universal>>,
        sources: SourceLookup,
    },
    InetCompile {
        error: RuntimeCompilerError,
        sources: SourceLookup,
    },
}

impl BuildError {
    fn display(&self) -> String {
        match self {
            Self::Workspace(WorkspaceError::Load(PackageLoadError::ParseError {
                source,
                error,
                ..
            })) => format!(
                "{:?}",
                miette::Report::from(error.to_owned()).with_source_code(source.clone())
            ),
            Self::Workspace(WorkspaceError::LowerError { source, error, .. }) => {
                format!("{:?}", error.to_report(source.clone()))
            }
            Self::Workspace(WorkspaceError::UnknownDependency { source, span, .. })
            | Self::Workspace(WorkspaceError::ImportedModuleNotFound { source, span, .. })
            | Self::Workspace(WorkspaceError::DuplicateImportAlias { source, span, .. })
            | Self::Workspace(WorkspaceError::BindingNameConflictsWithImportAlias {
                source,
                span,
                ..
            })
            | Self::Workspace(WorkspaceError::UnknownModuleQualifier { source, span, .. })
            | Self::Workspace(WorkspaceError::QualifiedCurrentModuleReference {
                source,
                span,
                ..
            }) => format_with_source_span(source.clone(), span, self.to_string()),
            Self::Workspace(error) => error.to_string(),
            Self::Type {
                error,
                file_scope,
                sources,
            } => {
                format!(
                    "{:?}",
                    error.to_report_in_scope(
                        source_for_type_error(error, sources),
                        file_scope.as_ref()
                    )
                )
            }
            Self::InetCompile { error, sources } => format!(
                "inet compilation error: {}",
                error.display(&source_for_fallback(sources))
            ),
        }
    }
}

impl Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Workspace(error) => write!(f, "{error}"),
            Self::Type { error, .. } => write!(f, "{error:?}"),
            Self::InetCompile { error, .. } => write!(f, "{error:?}"),
        }
    }
}

fn build_checked_package(
    package_path: &PathBuf,
) -> Result<(CheckedWorkspace, Vec<ModulePath>, SourceLookup), BuildError> {
    let workspace = default_workspace_from_path(package_path).map_err(BuildError::Workspace)?;
    let sources = workspace.sources().clone();
    let checked = workspace.type_check().map_err(|error| BuildError::Type {
        file_scope: error
            .spans()
            .0
            .file()
            .and_then(|file| workspace.import_scope(&file).cloned()),
        error,
        sources: sources.clone(),
    })?;
    let root_modules = checked.workspace().root_modules();
    Ok((checked, root_modules, sources))
}

fn build_runtime_package(
    package_path: &PathBuf,
    max_interactions: u32,
) -> Result<
    (
        CheckedWorkspace,
        par_core::runtime::Compiled<Linked>,
        Vec<ModulePath>,
    ),
    BuildError,
> {
    let (checked, local_modules, sources) = build_checked_package(package_path)?;
    let rt_compiled = checked
        .compile_runtime(max_interactions)
        .and_then(|compiled| compiled.link())
        .map_err(|error| BuildError::InetCompile {
            error,
            sources: sources.clone(),
        })?;
    Ok((checked, rt_compiled, local_modules))
}

#[cfg(not(target_family = "wasm"))]
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
                .about("Run a definition in a Par package")
                .arg(arg!(--stats "Print statistics after running the definition"))
                .arg(
                    arg!(--package <PACKAGE> "Path to package directory (or any file/directory inside it)")
                        .value_parser(value_parser!(PathBuf))
                        .default_value("."),
                )
                .arg(arg!([target] "Target to run: `path/to/Module` or `path/to/Module.Def`"))
                .arg(arg!(-f --flag <FLAG> ... "Set a flag"))
                .arg(arg!(--max_interactions <MAX_INTERACTIONS> ... "Maximum number of interactions during compilation")
            .value_parser(value_parser!(u32))),
        )
        .subcommand(
            Command::new("check")
                .about("Type check a Par package in the CLI")
                .arg(
                    arg!(--package <PACKAGE> "Path to package directory (or any file/directory inside it)")
                        .value_parser(value_parser!(PathBuf))
                        .default_value("."),
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
                    arg!(--package <PACKAGE> "Path to package directory (or any file/directory inside it)")
                        .value_parser(value_parser!(PathBuf))
                        .default_value("."),
                )
                .arg(arg!([target] "Test target: `path/to/Module` or `path/to/Module.TestName`"))
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
            let package = args.get_one::<PathBuf>("package").unwrap().clone();
            let target = args.get_one::<String>("target").cloned();
            let max_interactions = args
                .get_one::<u32>("max_interactions")
                .cloned()
                .unwrap_or(MAX_INTERACTIONS_DEFAULT);
            run_definition(package, target, stats, max_interactions);
        }
        Some(("check", args)) => {
            let package = args.get_one::<PathBuf>("package").unwrap().clone();
            if check(package).is_err() {
                return ExitCode::FAILURE;
            }
        }
        Some(("lsp", _)) => run_language_server(),
        Some(("test", args)) => {
            let package = args.get_one::<PathBuf>("package").unwrap().clone();
            let target = args.get_one::<String>("target").cloned();
            let filter = args.get_one::<String>("filter");
            let max_interactions = args
                .get_one::<u32>("max_interactions")
                .cloned()
                .unwrap_or(MAX_INTERACTIONS_DEFAULT);
            if !run_tests(package, target, filter.cloned(), max_interactions) {
                return ExitCode::FAILURE;
            }
        }
        _ => unreachable!(),
    }

    ExitCode::SUCCESS
}

#[cfg(target_family = "wasm")]
fn main() {
    use eframe::wasm_bindgen::JsCast as _;

    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Debug).ok();

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        let document = web_sys::window()
            .expect("No window")
            .document()
            .expect("No document");

        let canvas = document
            .get_element_by_id("the_canvas_id")
            .expect("Failed to find the_canvas_id")
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .expect("the_canvas_id was not a HtmlCanvasElement");

        let file = None;

        let start_result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|cc| Ok(Playground::new(cc, file, MAX_INTERACTIONS_DEFAULT))),
            )
            .await;

        // Remove the loading text and spinner:
        if let Some(loading_text) = document.get_element_by_id("loading_text") {
            match start_result {
                Ok(_) => {
                    loading_text.remove();
                }
                Err(e) => {
                    loading_text.set_inner_html(
                        "<p> The app has crashed. See the developer console for details. </p>",
                    );
                    panic!("Failed to start eframe: {e:?}");
                }
            }
        }
    });
}

#[cfg(feature = "playground")]
/// String to save on crash. Used by the playground to avoid losing everything on panic.
static CRASH_STR: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

#[cfg(not(feature = "playground"))]
fn run_playground(_: Option<PathBuf>, _: u32) {
    eprintln!("Playground was disabled when building Par")
}

#[cfg(not(target_family = "wasm"))]
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
        "Par Playground",
        options,
        Box::new(|cc| Ok(Playground::new(cc, file, max_interactions))),
    )
    .expect("egui crashed");
}

fn run_definition(
    package_path: PathBuf,
    target: Option<String>,
    print_stats: bool,
    max_interactions: u32,
) {
    let runtime = tokio_factory::create_runtime().expect("Failed to create Tokio runtime");
    runtime.block_on(async {
        let (checked, rt_compiled, local_modules) =
            match build_runtime_package(&package_path, max_interactions) {
                Ok((checked, rt_compiled, local_modules)) => (checked, rt_compiled, local_modules),
                Err(error) => {
                    println!("{}", error.display().bright_red());
                    return;
                }
            };

        let Some(name) = resolve_target_definition(target.as_deref(), &checked, &local_modules)
        else {
            let target = target.unwrap_or_else(|| "Main.Main".to_string());
            println!("{}: {}", "Definition not found".bright_red(), target);
            return;
        };

        let Some(Type::Break(_)) = rt_compiled.get_type_of(name) else {
            println!(
                "{}: {}",
                "Definition does not have the unit (!) type".bright_red(),
                target.unwrap_or_else(|| "Main.Main".to_string())
            );
            return;
        };

        let start = Instant::now();
        let package_to_run = rt_compiled.code.get_with_name(name).unwrap();
        let (root, reducer_future) = par_runtime::start_and_instantiate(
            Arc::new(TokioSpawn::new()),
            rt_compiled.code.arena.clone(),
            package_to_run,
        );

        root.continue_();
        let stats = reducer_future.await;

        if print_stats {
            eprintln!("{}", stats.show(start.elapsed()));
            eprintln!("\tArena size: {}", rt_compiled.code.arena.memory_size());
        }
    });
}

fn resolve_target_definition<'a>(
    target: Option<&str>,
    checked: &'a CheckedWorkspace,
    local_modules: &[ModulePath],
) -> Option<&'a par_core::frontend::language::GlobalName<par_core::frontend::language::Universal>> {
    let target = target.unwrap_or("Main.Main");
    let parsed_target = parse_target(target);
    let definition_target = parsed_target
        .definition_name
        .unwrap_or_else(|| "Main".to_string());

    let canonical_module = find_local_module(&parsed_target.module_path, local_modules)?;
    let module_name = canonical_module.to_slash_path();

    checked
        .checked_module()
        .definitions
        .iter()
        .find(|(name, _)| {
            name.primary == definition_target
                && local_module_slash_path(&name.module).as_deref() == Some(module_name.as_str())
        })
        .map(|(name, _)| name)
}

fn check(package_path: PathBuf) -> Result<(), String> {
    println!("Checking package: {}", package_path.display());

    let build_result = build_runtime_package(&package_path, MAX_INTERACTIONS_DEFAULT);
    if let Err(error) = build_result {
        let error_string = error.display();
        eprintln!("{}", error_string.bright_red());
        return Err(error_string);
    }
    Ok(())
}

#[cfg(not(target_family = "wasm"))]
fn run_language_server() {
    language_server::language_server_main::main()
}

fn run_tests(
    package_path: PathBuf,
    target: Option<String>,
    filter: Option<String>,
    max_interactions: u32,
) -> bool {
    test_runner::run_tests(package_path, target, filter, max_interactions)
}
