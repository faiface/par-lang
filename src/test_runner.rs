use crate::icombs::{
    readback::{TypedHandle, TypedReadback},
    IcCompiled,
};
use crate::par::{language, parse, program, types};
use crate::playground::Compiled;
use crate::spawn::TokioSpawn;
use crate::test_assertion::{create_assertion_channel, AssertionResult};
use colored::Colorize;
use futures::task::SpawnExt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

#[derive(Debug)]
enum TestStatus {
    Passed,
    Failed(String),
    FailedWithAssertions(Vec<AssertionResult>),
    CompileError(String),
    TypeError,
    ReadError,
}

#[derive(Debug)]
struct TestResult {
    name: String,
    duration: Duration,
    status: TestStatus,
}

pub fn run_tests(file: Option<PathBuf>, filter: Option<String>) {
    parse::set_miette_hook();

    let test_files = if let Some(file) = file {
        vec![file]
    } else {
        find_test_files()
    };

    if test_files.is_empty() {
        println!("{}", "No test files found".yellow());
        return;
    }

    println!("{}", "Running tests...".bright_blue());
    println!();

    let mut total_tests = 0;
    let mut passed_tests = 0;
    let start_time = Instant::now();

    for test_file in test_files {
        let results = run_test_file(&test_file, &filter);
        print_test_results(&test_file, &results);

        total_tests += results.len();
        passed_tests += results
            .iter()
            .filter(|r| matches!(r.status, TestStatus::Passed))
            .count();
    }

    let duration = start_time.elapsed();
    println!();
    print_summary(total_tests, passed_tests, duration);
}

fn find_test_files() -> Vec<PathBuf> {
    let mut test_files = Vec::new();

    // For now, just look for .par files in the current directory
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

fn run_test_file(file: &Path, filter: &Option<String>) -> Vec<TestResult> {
    let mut results = Vec::new();

    let Ok(code) = fs::read_to_string(file) else {
        results.push(TestResult {
            name: file.to_string_lossy().to_string(),
            duration: Duration::ZERO,
            status: TestStatus::ReadError,
        });
        return results;
    };

    let code_with_imports = code.clone();

    let compiled = match stacker::grow(32 * 1024 * 1024, || {
        Compiled::from_string(&code_with_imports)
    }) {
        Ok(compiled) => compiled,
        Err(err) => {
            results.push(TestResult {
                name: file.to_string_lossy().to_string(),
                duration: Duration::ZERO,
                status: TestStatus::CompileError(
                    err.display(Arc::from(code_with_imports.as_str()))
                        .to_string(),
                ),
            });
            return results;
        }
    };

    let Ok(checked) = compiled.checked else {
        results.push(TestResult {
            name: file.to_string_lossy().to_string(),
            duration: Duration::ZERO,
            status: TestStatus::TypeError,
        });
        return results;
    };

    let program = checked.program;
    let ic_compiled = checked.ic_compiled.unwrap();

    for (name, _) in &program.definitions {
        // Look for definitions that start with Test (case-sensitive)
        if name.primary.starts_with("Test") && name.primary != "Test" {
            if let Some(filter) = filter {
                if !name.primary.contains(filter) {
                    continue;
                }
            }

            let result = run_single_test(&program, &ic_compiled, name.primary.clone());
            results.push(result);
        }
    }

    results
}

fn run_single_test(
    program: &crate::par::program::CheckedModule,
    ic_compiled: &crate::icombs::IcCompiled,
    test_name: String,
) -> TestResult {
    let start = Instant::now();
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let result = runtime.block_on(async {
        let name = program
            .definitions
            .iter()
            .find(|(n, _)| n.primary == test_name)
            .map(|(n, _)| n)
            .ok_or_else(|| "Definition not found")?;

        let ty = ic_compiled.get_type_of(name).ok_or("Type not found")?;

        run_test_with_test_type(program, ic_compiled, name, &ty).await
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
    program: &crate::par::program::CheckedModule,
    ic_compiled: &IcCompiled,
    name: &crate::par::language::GlobalName,
    ty: &crate::par::types::Type,
) -> Result<TestStatus, String> {
    let (sender, receiver) = create_assertion_channel();

    let mut net = ic_compiled.create_net();
    let child_net = ic_compiled.get_with_name(name).ok_or("Net not found")?;

    let tree = net.inject_net(child_net).with_type(ty.clone());

    let (net_wrapper, reducer_future) = net.start_reducer(Arc::new(TokioSpawn::new()));

    let type_defs = program.type_defs.clone();
    let spawner = Arc::new(TokioSpawn::new());
    let sender_clone = sender.clone();

    // Spawn the test function execution
    let test_future = spawner
        .spawn_with_handle(async move {
            let handle = TypedHandle::from_wrapper(type_defs, net_wrapper, tree);

            // The test function expects [Test.Test] !
            // We need to send a Test instance
            let (test_handle, continue_handle) = handle.send();

            // Convert TypedHandle to Handle and provide Test instance directly
            let untyped_handle = test_handle.into_untyped();

            // Provide the Test instance with the sender directly
            use crate::par::builtin::test::provide_test;
            provide_test(untyped_handle, sender_clone);

            // Continue with the test execution
            continue_handle.readback().await
        })
        .map_err(|_| "Spawn failed")?;

    // collect results from the receiver
    let mut results = Vec::new();
    let readback_result = test_future.await;
    reducer_future.await;

    while let Ok(result) = receiver.try_recv() {
        results.push(result);
    }

    match readback_result {
        TypedReadback::Break => {
            let failed_assertions: Vec<_> = results.iter().filter(|r| !r.passed).cloned().collect();

            if failed_assertions.is_empty() && !results.is_empty() {
                Ok(TestStatus::Passed)
            } else if !failed_assertions.is_empty() {
                Ok(TestStatus::FailedWithAssertions(failed_assertions))
            } else {
                Err("Test completed but no assertions were made".to_string())
            }
        }
        _ => Err("Test did not return expected type".to_string()),
    }
}

const PASSED: &str = "✓";
const FAILED: &str = "✗";

fn print_test_results(file: &Path, results: &[TestResult]) {
    let file_name = file.file_name().unwrap().to_string_lossy();
    println!("{} {}", PASSED.green(), file_name.bright_white());

    // TODO: consider to use Cow when we need to structure the output more
    for result in results {
        let (icon, error_info): (colored::ColoredString, Option<ErrorInfo>) = match &result.status {
            TestStatus::Passed => (PASSED.green(), None),
            TestStatus::Failed(msg) => (FAILED.red(), Some(ErrorInfo::Message(msg.clone()))),
            TestStatus::FailedWithAssertions(assertions) => (
                FAILED.red(),
                Some(ErrorInfo::Assertions(assertions.clone())),
            ),
            TestStatus::CompileError(msg) => (FAILED.red(), Some(ErrorInfo::Message(msg.clone()))),
            TestStatus::TypeError => (
                FAILED.red(),
                Some(ErrorInfo::Message("Type check failed".to_string())),
            ),
            TestStatus::ReadError => (
                FAILED.red(),
                Some(ErrorInfo::Message("File read error".to_string())),
            ),
        };

        let duration = format!("({:.3}s)", result.duration.as_secs_f32()).dimmed();
        println!("  {} {} {}", icon, result.name, duration);

        match error_info {
            Some(ErrorInfo::Message(msg)) => {
                println!("    {}", msg.red());
            }
            Some(ErrorInfo::Assertions(assertions)) => {
                for assertion in assertions {
                    println!(
                        "    {} {}: {}",
                        FAILED.red(),
                        assertion.description,
                        "assertion failed".red()
                    );
                }
            }
            None => {}
        }
    }
}

enum ErrorInfo {
    Message(String),
    Assertions(Vec<AssertionResult>),
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
