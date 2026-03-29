use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use par_core::frontend::language::PackageId;
use pathdiff::diff_paths;
use percent_encoding::{AsciiSet, CONTROLS, utf8_percent_encode};

use crate::model::{ItemKind, ModuleKey, PackageKind, SiteModel};

const FILE_SEGMENT: &AsciiSet = &CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'#')
    .add(b'%')
    .add(b'*')
    .add(b'/')
    .add(b':')
    .add(b'<')
    .add(b'>')
    .add(b'?')
    .add(b'\\')
    .add(b'|');

#[derive(Debug, Clone)]
pub struct SitePaths {
    pub index: PathBuf,
    pub css: PathBuf,
    pub js: PathBuf,
    pub package_pages: BTreeMap<PackageId, PathBuf>,
    pub module_pages: BTreeMap<ModuleKey, PathBuf>,
}

impl SitePaths {
    pub fn new(site: &SiteModel) -> Self {
        let mut package_pages = BTreeMap::new();
        let mut module_pages = BTreeMap::new();

        for package in site.packages.values() {
            let package_path = package_page_path(&package.id, package.kind);
            package_pages.insert(package.id.clone(), package_path.clone());

            for module in &package.modules {
                module_pages.insert(
                    module.key(),
                    package_path_for_module(&package_path, &module.path.to_slash_path()),
                );
            }
        }

        Self {
            index: PathBuf::from("index.html"),
            css: PathBuf::from("assets/site.css"),
            js: PathBuf::from("assets/site.js"),
            package_pages,
            module_pages,
        }
    }
}

pub fn item_anchor(kind: ItemKind, primary: &str) -> String {
    let prefix = match kind {
        ItemKind::Type => "type",
        ItemKind::Declaration => "dec",
    };
    format!("{prefix}-{}", encode_segment(primary))
}

pub fn relative_href(from_page: &Path, to_page: &Path) -> String {
    let base_dir = from_page.parent().unwrap_or_else(|| Path::new(""));
    let diff = diff_paths(to_page, base_dir).unwrap_or_else(|| to_page.to_path_buf());
    let text = diff.to_string_lossy().replace('\\', "/");
    if text.is_empty() {
        String::from(".")
    } else {
        text
    }
}

pub fn relative_href_with_anchor(from_page: &Path, to_page: &Path, anchor: &str) -> String {
    if from_page == to_page {
        return format!("#{anchor}");
    }

    let mut href = relative_href(from_page, to_page);
    href.push('#');
    href.push_str(anchor);
    href
}

fn package_page_path(id: &PackageId, kind: PackageKind) -> PathBuf {
    let mut path = PathBuf::from("packages").join(kind_dir(kind));
    if kind != PackageKind::Root {
        path = path.join(package_identity_segment(id));
    }
    path.join("index.html")
}

fn package_path_for_module(package_page: &Path, module_path: &str) -> PathBuf {
    package_page
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .join("modules")
        .join(encode_segment(module_path))
        .join("index.html")
}

fn kind_dir(kind: PackageKind) -> &'static str {
    match kind {
        PackageKind::Root => "root",
        PackageKind::BuiltIn => "builtin",
        PackageKind::DirectDependency => "dependency",
        PackageKind::IndirectDependency => "indirect",
    }
}

fn package_identity_segment(id: &PackageId) -> String {
    match id {
        PackageId::Special(name) | PackageId::Local(name) | PackageId::Remote(name) => {
            encode_segment(name.as_str())
        }
    }
}

fn encode_segment(value: &str) -> String {
    utf8_percent_encode(value, FILE_SEGMENT).to_string()
}
