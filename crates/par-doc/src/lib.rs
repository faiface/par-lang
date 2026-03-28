mod error;
mod html;
mod load;
mod model;
mod paths;
mod render;

use std::path::PathBuf;

pub use error::DocError;

#[derive(Debug, Clone)]
pub struct DocOptions {
    pub package_path: PathBuf,
    pub out_dir: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct GeneratedDocs {
    pub out_dir: PathBuf,
    pub index_file: PathBuf,
}

pub fn generate_docs(options: DocOptions) -> Result<GeneratedDocs, DocError> {
    let loaded = load::load_site(&options.package_path)?;
    let out_dir = options.out_dir.unwrap_or(loaded.default_out_dir);
    let index_file = render::render_site(&loaded.model, &out_dir)?;
    Ok(GeneratedDocs {
        out_dir,
        index_file,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::PackageKind;
    use crate::paths::{SitePaths, relative_href_with_anchor};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(prefix: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("par-doc-{prefix}-{unique}"));
        fs::create_dir_all(&path).expect("failed to create temp dir");
        path
    }

    fn write_package(root: &Path, manifest: &str, files: &[(&str, &str)]) {
        fs::create_dir_all(root.join("src")).expect("failed to create src directory");
        fs::write(root.join("Par.toml"), manifest).expect("failed to write manifest");
        for (relative_path, source) in files {
            let path = root.join(relative_path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("failed to create parent directory");
            }
            fs::write(path, source).expect("failed to write source file");
        }
    }

    fn read(out_dir: &Path, relative_path: &Path) -> String {
        fs::read_to_string(out_dir.join(relative_path)).expect("failed to read generated file")
    }

    #[test]
    fn generates_builtin_docs_outside_package() {
        let start = temp_dir("builtins");
        let out_dir = start.join("out");

        let loaded = crate::load::load_site(&start).expect("failed to load builtin docs site");
        let generated = generate_docs(DocOptions {
            package_path: start.clone(),
            out_dir: Some(out_dir.clone()),
        })
        .expect("failed to generate builtin docs");

        let paths = SitePaths::new(&loaded.model);
        let index_html = read(&out_dir, &paths.index);

        assert!(loaded.model.sections.root.is_empty());
        assert_eq!(loaded.model.sections.built_in.len(), 2);
        assert!(index_html.contains("Built In"));
        assert!(index_html.contains("core"));
        assert!(index_html.contains("basic"));
        assert!(!index_html.contains("Dependencies</h2>"));
        assert!(generated.index_file.is_file());
    }

    #[test]
    fn generates_workspace_docs_with_visibility_and_cross_links() {
        let root = temp_dir("workspace");
        let helper = root.join("helper");
        let inner = root.join("inner");

        write_package(
            &inner,
            "\
[package]
name = \"inner\"
",
            &[(
                "src/Inner.par",
                "\
export module Inner

export {
  type Inner = !
  dec Make : Inner
}

def Make = !
",
            )],
        );

        write_package(
            &helper,
            "\
[package]
name = \"helper\"

[dependencies]
inner = \"../inner\"
",
            &[(
                "src/Util.par",
                "\
export module Util

import @inner/Inner

export {
  type Visible = Inner.Inner
  dec VisibleDec : Visible
}

type Hidden = !
dec HiddenDec : Hidden

def VisibleDec = Inner.Make
def HiddenDec = !
",
            )],
        );

        write_package(
            &root,
            "\
[package]
name = \"app\"

[dependencies]
helper = \"./helper\"
",
            &[
                (
                    "src/Main.par",
                    "\
export module Main

import @helper/Util

export {
  type Visible = Util.Visible
  dec VisibleDec : Visible
}

type Hidden = !
dec HiddenDec : Hidden

def VisibleDec = Util.VisibleDec
def HiddenDec = !
",
                ),
                (
                    "src/Secret.par",
                    "\
module Secret

export type OpenInSecret = !
type Closed = !

dec HiddenSecret : Closed
export dec SecretVisible : OpenInSecret

def HiddenSecret = !
def SecretVisible = !
",
                ),
            ],
        );

        let loaded = crate::load::load_site(&root).expect("failed to load workspace docs site");
        let out_dir = root.join("site");
        generate_docs(DocOptions {
            package_path: root.clone(),
            out_dir: Some(out_dir.clone()),
        })
        .expect("failed to generate workspace docs");

        let paths = SitePaths::new(&loaded.model);
        let index_html = read(&out_dir, &paths.index);
        assert!(index_html.contains("Dependencies"));
        assert!(index_html.contains("helper"));
        assert!(index_html.contains("Indirect Dependencies"));
        assert!(index_html.contains("inner"));

        let root_package = loaded
            .model
            .packages
            .values()
            .find(|package| package.kind == PackageKind::Root)
            .expect("missing root package");
        let helper_package = loaded
            .model
            .packages
            .values()
            .find(|package| package.name == "helper")
            .expect("missing helper package");
        let inner_package = loaded
            .model
            .packages
            .values()
            .find(|package| package.name == "inner")
            .expect("missing inner package");

        let root_package_html = read(
            &out_dir,
            paths
                .package_pages
                .get(&root_package.id)
                .expect("missing root package page"),
        );
        let helper_package_html = read(
            &out_dir,
            paths
                .package_pages
                .get(&helper_package.id)
                .expect("missing helper package page"),
        );

        assert!(root_package_html.contains("#type-Hidden"));
        assert!(root_package_html.contains("#dec-HiddenDec"));
        assert!(root_package_html.contains("Secret"));
        assert!(!helper_package_html.contains("#type-Hidden"));
        assert!(!helper_package_html.contains("#dec-HiddenDec"));

        let secret_module = root_package
            .modules
            .iter()
            .find(|module| module.path.module == "Secret")
            .expect("missing Secret module");
        let secret_module_html = read(
            &out_dir,
            paths
                .module_pages
                .get(&secret_module.key())
                .expect("missing Secret module page"),
        );
        assert!(secret_module_html.contains("title=\"Unexported\""));

        let main_module = root_package
            .modules
            .iter()
            .find(|module| module.path.module == "Main")
            .expect("missing Main module");
        let helper_module = helper_package
            .modules
            .iter()
            .find(|module| module.path.module == "Util")
            .expect("missing Util module");
        let main_module_html = read(
            &out_dir,
            paths
                .module_pages
                .get(&main_module.key())
                .expect("missing Main module page"),
        );
        let helper_visible_href = relative_href_with_anchor(
            paths
                .module_pages
                .get(&main_module.key())
                .expect("missing Main module page"),
            paths
                .module_pages
                .get(&helper_module.key())
                .expect("missing Util module page"),
            "type-Visible",
        );
        assert!(main_module_html.contains(&helper_visible_href));

        let helper_module_html = read(
            &out_dir,
            paths
                .module_pages
                .get(&helper_module.key())
                .expect("missing Util module page"),
        );
        let inner_module = inner_package
            .modules
            .iter()
            .find(|module| module.path.module == "Inner")
            .expect("missing Inner module");
        let inner_href = relative_href_with_anchor(
            paths
                .module_pages
                .get(&helper_module.key())
                .expect("missing Util module page"),
            paths
                .module_pages
                .get(&inner_module.key())
                .expect("missing Inner module page"),
            "type-Inner",
        );
        assert!(helper_module_html.contains(&inner_href));
    }
}
