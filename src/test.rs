#![cfg(test)]

use std::path::PathBuf;

use crate::check;

const LARGE_TEST_STACK_SIZE: usize = 6 * 1024 * 1024;

fn run_with_large_stack<T>(f: impl FnOnce() -> T + Send + 'static) -> T
where
    T: Send + 'static,
{
    let handle = std::thread::Builder::new()
        .name("par-large-stack-test".to_string())
        .stack_size(LARGE_TEST_STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn large-stack test thread");
    match handle.join() {
        Ok(result) => result,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

#[test]
fn check_all_examples() -> Result<(), String> {
    run_with_large_stack(|| {
        // Check the examples package as a whole.
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("examples");
        eprintln!("Checking {:?}", d);
        check(d)
    })
}

#[test]
fn test_all_files() -> Result<(), String> {
    run_with_large_stack(|| {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("tests");
        eprintln!("Testing {:?}", d);
        if crate::test_runner::run_tests(d, None, None, 10_000) {
            Ok(())
        } else {
            Err("Some tests failed".to_string())
        }
    })
}
