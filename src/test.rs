use std::fs::{self, DirEntry};
use std::io;
use std::path::{Path, PathBuf};

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
fn test_all_files() -> Result<(), String> {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests");
    let mut result = Ok(());
    visit_dirs(&d, &mut |entry| {
        if matches!(
            entry.path().extension().map(|x| x.to_str()),
            Some(Some("par"))
        ) {
            // test it
            eprintln!("{}", entry.path().display());
            let results = crate::test_runner::run_test_file(&crate::BuildConfig { new_runtime: true }, &entry.path(), &None);
            if !results.iter().all(|x| x.status.is_passed()) {
                result = result.clone().and(Err(":(".to_owned()));
            }
        }
    })
    .unwrap();
    result
}
