#![cfg(test)]

use std::fs::{self, DirEntry};
use std::io;
use std::path::{Path, PathBuf};

use crate::check;
use crate::par::build_result::BuildConfig;

// one possible implementation of walking a directory only visiting files
fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}

#[test]
fn test_all_files_rtv3() -> Result<(), String> {
    let config = BuildConfig { new_runtime: true };
    test_all_files(&config)
}

#[test]
fn test_all_files_rtv2() -> Result<(), String> {
    let config = BuildConfig { new_runtime: false };
    test_all_files(&config)
}

#[test]
fn check_all_examples() -> Result<(), String> {
    // check all examples
    // this also pre-reduces them
    let config = BuildConfig { new_runtime: false };
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("examples");
    let mut result = Ok(());
    visit_dirs(&d, &mut |entry| {
        if !matches!(
            entry.path().extension().map(|x| x.to_str()),
            Some(Some("par"))
        ) {
            return ();
        };
        eprintln!("Checking {:?}", entry.path());
        result = result.clone().and(check(&config, entry.path()));
    })
    .unwrap();
    result
}

fn test_all_files(config: &BuildConfig) -> Result<(), String> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests");
    let mut all_results = vec![];
    visit_dirs(&d, &mut |entry| {
        if !matches!(
            entry.path().extension().map(|x| x.to_str()),
            Some(Some("par"))
        ) {
            return ();
        }
        eprintln!("Testing {:?}", entry.path());
        let results = crate::test_runner::run_test_file(config, &entry.path(), &None);
        all_results.extend(
            results
                .into_iter()
                .filter(|x| !x.status.is_passed())
                .map(|x| (x, entry.path())),
        )
    })
    .unwrap();
    if all_results.is_empty() {
        Ok(())
    } else {
        use std::fmt::Write;
        let mut output = String::new();
        writeln!(&mut output, "Some tests failed:").unwrap();
        for (result, file) in all_results {
            writeln!(&mut output, "{}: {:?}", file.display(), result.status).unwrap()
        }
        eprintln!("{}", output);
        Err(output)
    }
}
