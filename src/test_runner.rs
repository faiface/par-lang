use crate::icombs::readback::{TypedHandle, TypedReadback};
use crate::par::parse;
use crate::playground::Compiled;
use crate::spawn::TokioSpawn;
use colored::Colorize;
use futures::task::SpawnExt;
use scopeguard::defer;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

#[derive(Debug)]
enum TestStatus {
    Passed,
    Failed(String),
    CompileError(String),
    TypeError,
    ReadError,
    NotFound,
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
    let panic_info = Arc::new(Mutex::new(None));
    let panic_info_clone = Arc::clone(&panic_info);

    let original_hook = std::panic::take_hook();
    defer! {
        std::panic::set_hook(original_hook);
    }
    std::panic::set_hook(Box::new(move |info| {
        if let Some(s) = info.payload().downcast_ref::<String>() {
            if s.contains("Test assertion failed") {
                *panic_info_clone.lock().unwrap() = Some(s.clone());
                return;
            }
        } else if let Some(s) = info.payload().downcast_ref::<&str>() {
            if s.contains("Test assertion failed") {
                *panic_info_clone.lock().unwrap() = Some(s.to_string());
                return;
            }
        }
        eprintln!("{}", info);
    }));

    let runtime = tokio::runtime::Runtime::new().unwrap();
    let result = runtime.block_on(async {
        let name = program
            .definitions
            .iter()
            .find(|(n, _)| n.primary == test_name)
            .map(|(n, _)| n)
            .ok_or_else(|| "Definition not found")?;

        let ty = ic_compiled.get_type_of(name).ok_or("Type not found")?;

        let mut net = ic_compiled.create_net();
        let child_net = ic_compiled.get_with_name(name).ok_or("Net not found")?;

        let tree = net.inject_net(child_net).with_type(ty.clone());

        let (net_wrapper, reducer_future) = net.start_reducer(Arc::new(TokioSpawn::new()));

        let type_defs = program.type_defs.clone();
        let spawner = Arc::new(TokioSpawn::new());
        let readback_result = spawner
            .spawn_with_handle(async move {
                let handle = TypedHandle::from_wrapper(type_defs, net_wrapper, tree);
                handle.readback().await
            })
            .map_err(|_| "Spawn failed")?
            .await;

        reducer_future.await;

        match readback_result {
            TypedReadback::Break => Ok(()),
            _ => Err("Test did not return expected type".to_string()),
        }
    });

    let duration = start.elapsed();
    let final_result = if let Some(panic_msg) = panic_info.lock().unwrap().take() {
        TestStatus::Failed(panic_msg)
    } else {
        match result {
            Ok(()) => TestStatus::Passed,
            Err(msg) => TestStatus::Failed(msg),
        }
    };

    TestResult {
        name: test_name,
        duration,
        status: final_result,
    }
}

const PASSED: &str = "✓";
const FAILED: &str = "✗";

fn print_test_results(file: &Path, results: &[TestResult]) {
    let file_name = file.file_name().unwrap().to_string_lossy();
    println!("{} {}", PASSED.green(), file_name.bright_white());

    // TODO: consider to use Cow when we need to structure the output more
    for result in results {
        let (icon, error_str): (colored::ColoredString, Option<String>) = match &result.status {
            TestStatus::Passed => (PASSED.green(), None),
            TestStatus::Failed(msg) => (FAILED.red(), Some(msg.clone())),
            TestStatus::CompileError(msg) => (FAILED.red(), Some(msg.clone())),
            TestStatus::TypeError => (FAILED.red(), Some("Type check failed".to_string())),
            TestStatus::ReadError => (FAILED.red(), Some("File read error".to_string())),
            TestStatus::NotFound => (FAILED.red(), Some("Test definition not found".to_string())),
        };

        let duration = format!("({:.3}s)", result.duration.as_secs_f32()).dimmed();
        println!("  {} {} {}", icon, result.name, duration);

        if let Some(error) = error_str {
            println!("    {}", error.red());
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn test_result_passed() {
        let result = TestResult {
            name: "test_passed".to_string(),
            duration: Duration::from_millis(123),
            status: TestStatus::Passed,
        };

        assert_eq!(result.name, "test_passed");
        assert!(matches!(result.status, TestStatus::Passed));
        assert_eq!(result.duration.as_millis(), 123);
    }

    #[test]
    fn test_result_failed_with_message() {
        let result = TestResult {
            name: "test_failed".to_string(),
            duration: Duration::from_millis(50),
            status: TestStatus::Failed("Assertion failed".to_string()),
        };

        match result.status {
            TestStatus::Failed(msg) => {
                assert_eq!(msg, "Assertion failed");
            }
            _ => panic!("Expected TestStatus::Failed"),
        }
    }

    #[test]
    fn test_compile_error_status() {
        let result = TestResult {
            name: "compile_error".to_string(),
            duration: Duration::ZERO,
            status: TestStatus::CompileError("Syntax error".to_string()),
        };

        match result.status {
            TestStatus::CompileError(msg) => {
                assert_eq!(msg, "Syntax error");
            }
            _ => panic!("Expected TestStatus::CompileError"),
        }
    }

    #[test]
    fn test_type_error_status() {
        let result = TestResult {
            name: "type_error".to_string(),
            duration: Duration::ZERO,
            status: TestStatus::TypeError,
        };

        assert!(matches!(result.status, TestStatus::TypeError));
    }

    #[test]
    fn test_print_summary_all_passed() {
        // Smoke test: should not panic
        print_summary(3, 3, Duration::from_secs(1));
    }

    #[test]
    fn test_print_summary_some_failed() {
        // Smoke test: should not panic
        print_summary(5, 2, Duration::from_millis(500));
    }
}
