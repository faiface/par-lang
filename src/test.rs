#![cfg(test)]

use std::path::PathBuf;

use crate::check;

#[test]
fn check_all_examples() -> Result<(), String> {
    // Check the examples package as a whole.
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("examples");
    eprintln!("Checking {:?}", d);
    check(d)
}

#[test]
fn test_all_files() -> Result<(), String> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests");
    eprintln!("Testing {:?}", d);
    if crate::test_runner::run_tests(d, None, None, 10_000) {
        Ok(())
    } else {
        Err("Some tests failed".to_string())
    }
}
