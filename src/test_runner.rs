use colored::Colorize;
use par_builtin::import_builtins;
use par_core::{
    frontend::{
        CheckedModule, ParseAndCompileError, Type, TypeError, compile_runtime,
        language::GlobalName, lower, parse, set_miette_hook, type_check,
    },
    runtime,
    runtime::{Compiled, RuntimeCompilerError},
    testing::{AssertionResult, provide_test},
};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::mpsc;
use std::time::{Duration, Instant};
use std::{fmt::Display, fs};
use tokio::runtime::Runtime;

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

fn build_for_run(
    code: &str,
    file: &Path,
    max_interactions: u32,
) -> Result<(CheckedModule, Compiled), BuildError> {
    let parsed = parse(code, file.to_path_buf().into())
        .map_err(|error| BuildError::ParseAndCompile(ParseAndCompileError::Parse(error)))?;
    let mut lowered = lower(parsed)
        .map_err(|error| BuildError::ParseAndCompile(ParseAndCompileError::Compile(error)))?;
    import_builtins(&mut lowered);
    let checked = type_check(&lowered).map_err(BuildError::Type)?;
    let rt_compiled =
        compile_runtime(&checked, max_interactions).map_err(BuildError::InetCompile)?;
    Ok((checked, rt_compiled))
}

#[derive(Debug)]
pub enum TestStatus {
    Passed,
    PassedWithNoAssertions,
    Failed(String),
    FailedWithAssertions(Vec<AssertionResult>),
    CompileError(String),
    TypeError(String),
    ReadError(String),
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
            TestStatus::CompileError(msg) => write!(f, "compile error: {msg}"),
            TestStatus::TypeError(msg) => write!(f, "type error: {msg}"),
            TestStatus::ReadError(msg) => write!(f, "read error: {msg}"),
        }
    }
}

#[derive(Debug)]
pub struct TestResult {
    name: String,
    duration: Duration,
    pub status: TestStatus,
}

pub fn run_tests(file: Option<PathBuf>, filter: Option<String>, max_interactions: u32) -> bool {
    set_miette_hook();

    let test_files = match file {
        Some(f) => vec![f],
        None => find_test_files(),
    };

    if test_files.is_empty() {
        println!("{}", "No test files found".yellow());
        return false;
    }

    // TODO: placeholder for indicating that we are running tests
    println!("{}", "Running tests...".bright_blue());
    println!();

    let mut total_tests = 0;
    let mut passed_tests = 0;
    let start_time = Instant::now();

    for test_file in test_files {
        let results = run_test_file(&test_file, &filter, max_interactions);
        print_test_results(&test_file, &results);

        total_tests += results.len();
        passed_tests += results.iter().filter(|r| r.status.is_passed()).count();
    }

    let duration = start_time.elapsed();
    println!();
    print_summary(total_tests, passed_tests, duration);
    return passed_tests == total_tests;
}

fn find_test_files() -> Vec<PathBuf> {
    let mut test_files = Vec::new();

    // TODO: For now, just look for .par files in the current directory
    // In the future, when multi-file is supported, we can add back directory scanning
    if let Ok(entries) = fs::read_dir(".") {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().map(|e| e == "par").unwrap_or(false) {
                test_files.push(path);
            }
        }
    }

    test_files.sort();
    test_files
}

pub fn run_test_file(
    file: &Path,
    filter: &Option<String>,
    max_interactions: u32,
) -> Vec<TestResult> {
    let mut results = Vec::new();

    let Ok(code) = fs::read_to_string(file) else {
        results.push(TestResult {
            name: file.to_string_lossy().to_string(),
            duration: Duration::ZERO,
            status: TestStatus::ReadError(format!("Failed to read file: {}", file.display())),
        });
        return results;
    };

    let (checked, rt_compiled) = match stacker::grow(32 * 1024 * 1024, || {
        build_for_run(&code, file, max_interactions)
    }) {
        Ok(result) => result,
        Err(error) => {
            let message = error.display(Arc::from(code.as_str())).to_string();
            let status = match error {
                BuildError::Type(_) => TestStatus::TypeError(message),
                _ => TestStatus::CompileError(message),
            };
            results.push(TestResult {
                name: file.to_string_lossy().to_string(),
                duration: Duration::ZERO,
                status,
            });
            return results;
        }
    };

    // Look for definitions that start with Test (case-sensitive)
    let test_definitions: Vec<_> = checked
        .definitions
        .iter()
        .filter(|(name, _)| name.primary.starts_with("Test") && name.primary != "Test")
        .filter(|(name, _)| {
            filter
                .as_ref()
                .map(|f| name.primary.contains(f))
                .unwrap_or(true)
        })
        .collect();

    // Look for definitions that start with

    for (name, _) in test_definitions {
        let result = test_single_definition(&checked, &rt_compiled, name.primary.clone());
        results.push(result);
    }

    let run_definitions: Vec<_> = checked
        .definitions
        .iter()
        .filter(|(name, _)| name.primary.starts_with("Run") && name.primary != "Run")
        .filter(|(name, _)| {
            filter
                .as_ref()
                .map(|f| name.primary.contains(f))
                .unwrap_or(true)
        })
        .collect();

    for (name, _) in run_definitions {
        let result = run_single_definition(&checked, &rt_compiled, name.primary.clone());
        results.push(result);
    }

    results
}

fn test_single_definition(
    program: &CheckedModule,
    rt_compiled: &Compiled,
    test_name: String,
) -> TestResult {
    let start = Instant::now();

    let runtime = match Runtime::new() {
        Ok(rt) => rt,
        Err(e) => {
            return TestResult {
                name: test_name,
                duration: start.elapsed(),
                status: TestStatus::Failed(format!("Failed to create runtime: {}", e)),
            };
        }
    };

    let result = runtime.block_on(async {
        let name = program
            .definitions
            .iter()
            .find(|(n, _)| n.primary == test_name)
            .map(|(n, _)| n)
            .ok_or_else(|| format!("Test definition '{}' not found", test_name))?;

        let ty = rt_compiled
            .get_type_of(name)
            .ok_or_else(|| format!("Type not found for test '{}'", test_name))?;

        run_test_with_test_type(program, rt_compiled, name, &ty).await
    });

    let duration = start.elapsed();
    let final_result = match result {
        Ok(status) => status,
        Err(msg) => TestStatus::Failed(msg),
    };

    TestResult {
        name: test_name,
        duration,
        status: final_result,
    }
}

fn run_single_definition(
    program: &CheckedModule,
    rt_compiled: &Compiled,
    test_name: String,
) -> TestResult {
    let start = Instant::now();

    let runtime = match Runtime::new() {
        Ok(rt) => rt,
        Err(e) => {
            return TestResult {
                name: test_name,
                duration: start.elapsed(),
                status: TestStatus::Failed(format!("Failed to create runtime: {}", e)),
            };
        }
    };

    let result = runtime.block_on(async {
        let name = program
            .definitions
            .iter()
            .find(|(n, _)| n.primary == test_name)
            .map(|(n, _)| n)
            .ok_or_else(|| format!("Test definition '{}' not found", test_name))?;

        let _ty = rt_compiled
            .get_type_of(name)
            .ok_or_else(|| format!("Type not found for test '{}'", test_name))?;

        let (handle, fut) = runtime::start_and_instantiate(&rt_compiled, name).await;
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
        name: test_name,
        duration,
        status: final_result,
    }
}

async fn run_test_with_test_type(
    _program: &CheckedModule,
    rt_compiled: &Compiled,
    name: &GlobalName,
    _ty: &Type,
) -> Result<TestStatus, String> {
    let (sender, receiver) = mpsc::channel();

    let (mut root, reducer_future) = runtime::start_and_instantiate(&rt_compiled, name).await;

    // Spawn the test function execution
    //
    // The test function expects [Test.Test] !
    // We need to send a Test instance
    let test_handle = root.send();

    // Provide the Test instance with the sender directly
    provide_test(test_handle, sender).await;

    // Continue with the test execution
    root.continue_();
    // collect results from the receiver
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

const PASSED: &str = "✓";
const FAILED: &str = "✗";

// TODO: Current implementation only outputting the top-level test suite name, but should output sub-cases as well.
fn print_test_results(file: &Path, results: &[TestResult]) {
    let file_label = file
        .file_name()
        .map(|n| n.to_string_lossy())
        .unwrap_or_else(|| file.to_string_lossy());
    let all_passed = results.iter().all(|r| r.status.is_passed());
    let icon = if all_passed {
        PASSED.green()
    } else {
        FAILED.red()
    };

    println!("{} {}", icon, file_label.bright_white());

    for r in results {
        let icon = if r.status.is_passed() {
            PASSED.green()
        } else {
            FAILED.red()
        };
        let duration = format!("({:.3}s)", r.duration.as_secs_f32()).dimmed();
        println!("  {} {} {}", icon, r.name, duration);

        match &r.status {
            TestStatus::Failed(msg)
            | TestStatus::CompileError(msg)
            | TestStatus::TypeError(msg)
            | TestStatus::ReadError(msg) => {
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
