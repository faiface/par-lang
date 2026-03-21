use std::collections::BTreeMap;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::mpsc;
use std::time::{Duration, Instant};

use colored::Colorize;
use par_core::{
    frontend::{
        Type, TypeError,
        language::{GlobalName, Universal},
        set_miette_hook,
    },
    runtime::{Compiled, RuntimeCompilerError},
    testing::{AssertionResult, provide_test},
    workspace::{CheckedWorkspace, FileImportScope, ModulePath, PackageLoadError, WorkspaceError},
};
use par_runtime::linker::Linked;
use par_runtime::spawn::TokioSpawn;

use crate::package_utils::{
    SourceLookup, find_local_module, format_with_source_span, local_module_slash_path,
    parse_target, source_for_fallback, source_for_type_error,
};
use crate::workspace_support::default_workspace_from_path;

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

fn build_for_run(
    package_path: &Path,
    max_interactions: u32,
) -> Result<(CheckedWorkspace, Compiled<Linked>, Vec<ModulePath>), BuildError> {
    let workspace = default_workspace_from_path(package_path).map_err(BuildError::Workspace)?;
    let sources = workspace.sources.clone();
    let checked = workspace.type_check().map_err(|error| BuildError::Type {
        file_scope: error
            .spans()
            .0
            .file()
            .and_then(|file| workspace.import_scope(&file).cloned()),
        error,
        sources: sources.clone(),
    })?;
    let compiled = checked
        .compile_runtime(max_interactions)
        .and_then(|compiled| compiled.link())
        .map_err(|error| BuildError::InetCompile {
            error,
            sources: sources.clone(),
        })?;
    Ok((checked.clone(), compiled, checked.root_modules()))
}

#[derive(Debug)]
pub enum TestStatus {
    Passed,
    PassedWithNoAssertions,
    Failed(String),
    FailedWithAssertions(Vec<AssertionResult>),
}

impl TestStatus {
    pub fn is_passed(&self) -> bool {
        matches!(
            self,
            TestStatus::Passed | TestStatus::PassedWithNoAssertions
        )
    }
}

impl Display for TestStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestStatus::Passed => write!(f, "passed"),
            TestStatus::PassedWithNoAssertions => write!(f, "passed with no assertions"),
            TestStatus::Failed(msg) => write!(f, "failed: {msg}"),
            TestStatus::FailedWithAssertions(v) => {
                write!(f, "failed with {} assertion(s)", v.len())
            }
        }
    }
}

#[derive(Debug)]
pub struct TestResult {
    name: String,
    duration: Duration,
    pub status: TestStatus,
}

pub fn run_tests(
    package_path: PathBuf,
    target: Option<String>,
    filter: Option<String>,
    max_interactions: u32,
) -> bool {
    set_miette_hook();
    println!(
        "{} {}",
        "Running tests in package:".bright_blue(),
        package_path.display()
    );
    println!();

    let (checked, rt_compiled, local_modules) =
        match build_for_run(&package_path, max_interactions) {
            Ok(result) => result,
            Err(error) => {
                eprintln!("{}", error.display().bright_red());
                return false;
            }
        };

    let parsed_target = target.as_deref().map(parse_target);
    let module_selector = parsed_target
        .as_ref()
        .map(|parsed| parsed.module_path.as_str());
    let name_selector = parsed_target
        .as_ref()
        .and_then(|parsed| parsed.definition_name.as_deref());
    let selected_module = module_selector.as_deref().and_then(|selector| {
        find_local_module(selector, &local_modules).map(ModulePath::to_slash_path)
    });

    if module_selector.is_some() && selected_module.is_none() {
        eprintln!(
            "{} {}",
            "No such local module for test target:".bright_red(),
            target.unwrap_or_default()
        );
        return false;
    }

    let selected_module = selected_module.as_deref();
    let selected_name = name_selector.as_deref();

    let mut grouped_results: BTreeMap<String, Vec<TestResult>> = BTreeMap::new();
    let mut total_tests = 0usize;
    let mut passed_tests = 0usize;
    let start_time = Instant::now();

    let tests = collect_test_definitions(
        &checked,
        &local_modules,
        selected_module,
        selected_name,
        filter.as_deref(),
    );

    if tests.is_empty() {
        println!("{}", "No test definitions found".yellow());
        return false;
    }

    for (name, kind) in tests {
        let result = match kind {
            DefinitionKind::Test => test_single_definition(&checked, &rt_compiled, &name),
            DefinitionKind::Run => run_single_definition(&checked, &rt_compiled, &name),
        };
        total_tests += 1;
        if result.status.is_passed() {
            passed_tests += 1;
        }
        let module =
            local_module_slash_path(&name.module).unwrap_or_else(|| "<unknown>".to_string());
        grouped_results.entry(module).or_default().push(result);
    }

    for (module, results) in &grouped_results {
        print_test_results(module, results);
    }

    let duration = start_time.elapsed();
    println!();
    print_summary(total_tests, passed_tests, duration);
    passed_tests == total_tests
}

#[derive(Debug, Clone, Copy)]
enum DefinitionKind {
    Test,
    Run,
}

fn collect_test_definitions(
    checked: &CheckedWorkspace,
    local_modules: &[ModulePath],
    selected_module: Option<&str>,
    selected_name: Option<&str>,
    filter: Option<&str>,
) -> Vec<(GlobalName<Universal>, DefinitionKind)> {
    checked
        .checked_module()
        .definitions
        .iter()
        .filter_map(|(name, _)| {
            let module = local_module_slash_path(&name.module)?;
            if !is_local_module(module.as_str(), local_modules) {
                return None;
            }

            if let Some(selected_module) = selected_module
                && module != selected_module
            {
                return None;
            }
            if let Some(selected_name) = selected_name
                && name.primary != selected_name
            {
                return None;
            }
            if let Some(filter) = filter
                && !name.primary.contains(filter)
            {
                return None;
            }

            if name.primary.starts_with("Test") && name.primary != "Test" {
                return Some((name.clone(), DefinitionKind::Test));
            }
            if name.primary.starts_with("Run") && name.primary != "Run" {
                return Some((name.clone(), DefinitionKind::Run));
            }
            None
        })
        .collect()
}

fn is_local_module(module: &str, local_modules: &[ModulePath]) -> bool {
    local_modules
        .iter()
        .any(|candidate| candidate.to_slash_path() == module)
}

fn test_single_definition(
    _program: &CheckedWorkspace,
    rt_compiled: &Compiled<Linked>,
    test_name: &GlobalName<Universal>,
) -> TestResult {
    let start = Instant::now();
    let name_label = test_name.to_string();
    let runtime = match crate::tokio_factory::create_runtime() {
        Ok(rt) => rt,
        Err(e) => {
            return TestResult {
                name: name_label,
                duration: start.elapsed(),
                status: TestStatus::Failed(format!("Failed to create runtime: {}", e)),
            };
        }
    };

    let result = runtime.block_on(async {
        let ty = rt_compiled
            .get_type_of(test_name)
            .ok_or_else(|| format!("Type not found for test '{}'", test_name))?;
        run_test_with_test_type(rt_compiled, test_name, &ty).await
    });

    let duration = start.elapsed();
    let final_result = match result {
        Ok(status) => status,
        Err(msg) => TestStatus::Failed(msg),
    };

    TestResult {
        name: name_label,
        duration,
        status: final_result,
    }
}

fn run_single_definition(
    _program: &CheckedWorkspace,
    rt_compiled: &Compiled<Linked>,
    run_name: &GlobalName<Universal>,
) -> TestResult {
    let start = Instant::now();
    let name_label = run_name.to_string();
    let runtime = match crate::tokio_factory::create_runtime() {
        Ok(rt) => rt,
        Err(e) => {
            return TestResult {
                name: name_label,
                duration: start.elapsed(),
                status: TestStatus::Failed(format!("Failed to create runtime: {}", e)),
            };
        }
    };

    let result = runtime.block_on(async {
        let _ty = rt_compiled
            .get_type_of(run_name)
            .ok_or_else(|| format!("Type not found for test '{}'", run_name))?;
        let package = rt_compiled.code.get_with_name(run_name).unwrap();

        let (handle, fut) = par_runtime::start_and_instantiate(
            Arc::new(TokioSpawn::new()),
            rt_compiled.code.arena.clone(),
            package,
        );
        handle.continue_();
        fut.await;
        Ok(TestStatus::PassedWithNoAssertions)
    });

    let duration = start.elapsed();
    let final_result = match result {
        Ok(status) => status,
        Err(msg) => TestStatus::Failed(msg),
    };

    TestResult {
        name: name_label,
        duration,
        status: final_result,
    }
}

async fn run_test_with_test_type(
    rt_compiled: &Compiled<Linked>,
    name: &GlobalName<Universal>,
    _ty: &Type<Universal>,
) -> Result<TestStatus, String> {
    let (sender, receiver) = mpsc::channel();

    let package = rt_compiled.code.get_with_name(name).unwrap();
    let (mut root, reducer_future) = par_runtime::start_and_instantiate(
        Arc::new(TokioSpawn::new()),
        rt_compiled.code.arena.clone(),
        package,
    );

    let test_handle = root.send();
    provide_test(test_handle, sender).await;
    root.continue_();
    reducer_future.await;

    let mut results = vec![];
    while let Ok(result) = receiver.try_recv() {
        results.push(result);
    }
    let failed_assertions: Vec<_> = results.iter().filter(|r| !r.passed).cloned().collect();

    if failed_assertions.is_empty() && !results.is_empty() {
        Ok(TestStatus::Passed)
    } else if !failed_assertions.is_empty() {
        Ok(TestStatus::FailedWithAssertions(failed_assertions))
    } else {
        Ok(TestStatus::PassedWithNoAssertions)
    }
}

const PASSED: &str = "[PASS]";
const FAILED: &str = "[FAIL]";

fn print_test_results(module: &str, results: &[TestResult]) {
    let all_passed = results.iter().all(|r| r.status.is_passed());
    let icon = if all_passed {
        PASSED.green()
    } else {
        FAILED.red()
    };

    println!("{} {}", icon, module.bright_white());

    for r in results {
        let icon = if r.status.is_passed() {
            PASSED.green()
        } else {
            FAILED.red()
        };
        let duration = format!("({:.3}s)", r.duration.as_secs_f32()).dimmed();
        println!("  {} {} {}", icon, r.name, duration);

        match &r.status {
            TestStatus::Failed(msg) => {
                println!("    {}", msg.red());
            }
            TestStatus::FailedWithAssertions(assertions) => {
                for a in assertions {
                    println!(
                        "    {} {}: {}",
                        FAILED.red(),
                        a.description,
                        "assertion failed".red()
                    );
                }
            }
            TestStatus::PassedWithNoAssertions => {}
            TestStatus::Passed => {}
        }
    }
}

fn print_summary(total: usize, passed: usize, duration: std::time::Duration) {
    let failed = total - passed;

    let summary = if failed == 0 {
        format!(
            "Summary: {} passed ({:.3}s)",
            passed,
            duration.as_secs_f32()
        )
        .green()
    } else {
        format!(
            "Summary: {} passed, {} failed ({:.3}s)",
            passed,
            failed,
            duration.as_secs_f32()
        )
        .red()
    };

    println!("{}", summary);
}
