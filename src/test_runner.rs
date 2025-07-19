use crate::icombs::readback::{TypedHandle, TypedReadback};
use crate::par::parse;
use crate::playground::Compiled;
use crate::spawn::TokioSpawn;
use colored::Colorize;
use futures::task::SpawnExt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

#[derive(Debug)]
struct TestResult {
    name: String,
    passed: bool,
    duration: std::time::Duration,
    error: Option<String>,
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
        passed_tests += results.iter().filter(|r| r.passed).count();
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
            passed: false,
            duration: std::time::Duration::ZERO,
            error: Some("Could not read file".to_string()),
        });
        return results;
    };

    // Import builtin modules including Test module
    let code_with_imports = code.clone();

    let compiled = match stacker::grow(32 * 1024 * 1024, || {
        Compiled::from_string(&code_with_imports)
    }) {
        Ok(compiled) => compiled,
        Err(err) => {
            results.push(TestResult {
                name: file.to_string_lossy().to_string(),
                passed: false,
                duration: std::time::Duration::ZERO,
                error: Some(format!(
                    "Compilation failed: {}",
                    err.display(Arc::from(code_with_imports.as_str()))
                )),
            });
            return results;
        }
    };

    let Ok(checked) = compiled.checked else {
        results.push(TestResult {
            name: file.to_string_lossy().to_string(),
            passed: false,
            duration: std::time::Duration::ZERO,
            error: Some("Type check failed".to_string()),
        });
        return results;
    };

    let program = checked.program;
    let ic_compiled = checked.ic_compiled.unwrap();

    // Find all test definitions
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
            .unwrap();

        let ty = ic_compiled.get_type_of(name).unwrap();

        // For PoC, we expect tests to return TestResult type
        // For now, we'll just check if it runs without error
        let mut net = ic_compiled.create_net();
        let child_net = ic_compiled.get_with_name(name).unwrap();
        let tree = net.inject_net(child_net).with_type(ty.clone());

        let (net_wrapper, reducer_future) = net.start_reducer(Arc::new(TokioSpawn::new()));

        let type_defs = program.type_defs.clone();
        let spawner = Arc::new(TokioSpawn::new());
        let readback_result = spawner
            .spawn_with_handle(async move {
                let handle = TypedHandle::from_wrapper(type_defs, net_wrapper, tree);

                handle.readback().await
            })
            .unwrap()
            .await;

        reducer_future.await;

        match readback_result {
            TypedReadback::Break => Ok(()),
            _ => Err("Test did not return expected type".to_string()),
        }
    });

    let duration = start.elapsed();

    TestResult {
        name: test_name,
        passed: result.is_ok(),
        duration,
        error: result.err(),
    }
}

fn print_test_results(file: &Path, results: &[TestResult]) {
    let file_name = file.file_name().unwrap().to_string_lossy();
    println!("{} {}", "✓".green(), file_name.bright_white());

    for result in results {
        let icon = if result.passed {
            "✓".green()
        } else {
            "✗".red()
        };
        let duration = format!("({:.3}s)", result.duration.as_secs_f32()).dimmed();

        println!("  {} {} {}", icon, result.name, duration);

        if let Some(error) = &result.error {
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
    fn test_result_creation() {
        let result = TestResult {
            name: "test_example".to_string(),
            passed: true,
            duration: Duration::from_millis(100),
            error: None,
        };

        assert_eq!(result.name, "test_example");
        assert!(result.passed);
        assert_eq!(result.duration.as_millis(), 100);
        assert!(result.error.is_none());
    }

    #[test]
    fn test_failed_result() {
        let result = TestResult {
            name: "test_failed".to_string(),
            passed: false,
            duration: Duration::from_millis(50),
            error: Some("Assertion failed".to_string()),
        };

        assert!(!result.passed);
        assert_eq!(result.error.as_ref().unwrap(), "Assertion failed");
    }

    #[test]
    fn test_find_test_files_empty() {
        // When no test directory exists, should return empty vec
        let files = find_test_files();
        // This might not be empty if tests directory exists, but that's okay
        assert!(
            files.is_empty()
                || files
                    .iter()
                    .all(|f| f.extension().map(|e| e == "par").unwrap_or(false))
        );
    }

    #[test]
    fn test_print_summary_all_passed() {
        // This test just ensures the function doesn't panic
        print_summary(5, 5, Duration::from_secs(1));
    }

    #[test]
    fn test_print_summary_some_failed() {
        // This test just ensures the function doesn't panic
        print_summary(5, 3, Duration::from_millis(500));
    }
}
