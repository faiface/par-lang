#![cfg(test)]

use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

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

fn temp_package(name: &str, source: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("par-lang-{name}-{unique}"));
    fs::create_dir_all(root.join("src")).expect("failed to create temp package");
    fs::write(root.join("Par.toml"), "[package]\nname = \"tmp\"\n")
        .expect("failed to write manifest");
    fs::write(root.join("src").join("Main.par"), source).expect("failed to write source");
    root
}

#[test]
fn check_allows_unresolved_external_definitions() -> Result<(), String> {
    let package = temp_package("check-external", "module Main\n\ndef Main : ! = external\n");
    check(package)
}

#[test]
fn runtime_link_reports_missing_external_registration() {
    let package = temp_package("link-external", "module Main\n\ndef Main : ! = external\n");
    let error = match stacker::grow(32 * 1024 * 1024, || {
        crate::build_runtime_package(&package, 10_000)
    }) {
        Ok(_) => panic!("link should fail"),
        Err(error) => error,
    };
    let display = error.display();
    assert!(
        display.contains("Missing external registration for"),
        "unexpected error: {display}"
    );
}
