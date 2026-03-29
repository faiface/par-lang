use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use askama::Template;
use par_core::frontend::language::{GlobalName, PackageId, Universal};

use crate::error::DocError;
use crate::html::{self, TypeLinkTarget};
use crate::model::{ItemKind, ModuleModel, PackageKind, PackageModel, SiteModel};
use crate::paths::{SitePaths, item_anchor, relative_href, relative_href_with_anchor};

const SITE_CSS: &str = include_str!("../assets/site.css");
const SITE_JS: &str = include_str!("../assets/site.js");

pub fn render_site(site: &SiteModel, out_dir: &Path) -> Result<PathBuf, DocError> {
    let site_paths = SitePaths::new(site);
    let renderer = Renderer::new(site, &site_paths);

    write_file(out_dir, &site_paths.css, SITE_CSS)?;
    write_file(out_dir, &site_paths.js, SITE_JS)?;

    let index_html = renderer.render_packages_page()?;
    write_file(out_dir, &site_paths.index, &index_html)?;

    for package in site.packages.values() {
        let package_page = site_paths
            .package_pages
            .get(&package.id)
            .expect("site paths missing package page");
        let package_html = renderer.render_package_page(package)?;
        write_file(out_dir, package_page, &package_html)?;

        for module in &package.modules {
            let module_page = site_paths
                .module_pages
                .get(&module.key())
                .expect("site paths missing module page");
            let module_html = renderer.render_module_page(package, module)?;
            write_file(out_dir, module_page, &module_html)?;
        }
    }

    Ok(out_dir.join(&site_paths.index))
}

struct Renderer<'a> {
    site: &'a SiteModel,
    site_paths: &'a SitePaths,
    type_targets: BTreeMap<GlobalName<Universal>, TypeLinkTarget>,
}

impl<'a> Renderer<'a> {
    fn new(site: &'a SiteModel, site_paths: &'a SitePaths) -> Self {
        let mut type_targets = BTreeMap::new();
        for package in site.packages.values() {
            for module in &package.modules {
                let module_page = site_paths
                    .module_pages
                    .get(&module.key())
                    .expect("site paths missing module page");
                for item in &module.items {
                    if item.kind == ItemKind::Type {
                        type_targets.insert(
                            item.name.clone(),
                            TypeLinkTarget {
                                page: module_page.clone(),
                                anchor: item_anchor(item.kind, &item.name.primary),
                            },
                        );
                    }
                }
            }
        }

        Self {
            site,
            site_paths,
            type_targets,
        }
    }

    fn render_packages_page(&self) -> Result<String, DocError> {
        let current_page = &self.site_paths.index;
        let base = self.base_page(
            current_page,
            String::from("Packages · Par Docs"),
            vec![BreadcrumbView::current("Packages")],
        );
        let sections = self.section_views(current_page);
        PackagesPageTemplate {
            base: &base,
            sections: &sections,
        }
        .render()
        .map_err(DocError::template)
    }

    fn render_package_page(&self, package: &PackageModel) -> Result<String, DocError> {
        let current_page = self
            .site_paths
            .package_pages
            .get(&package.id)
            .expect("site paths missing package page");
        let base = self.base_page(
            current_page,
            format!("{} · Par Docs", package.name),
            vec![
                BreadcrumbView::link(
                    "Packages",
                    relative_href(current_page, &self.site_paths.index),
                ),
                BreadcrumbView::current(&package.name),
            ],
        );
        let modules = package
            .modules
            .iter()
            .map(|module| self.package_module_view(module, current_page))
            .collect::<Result<Vec<_>, _>>()?;

        PackagePageTemplate {
            base: &base,
            package: &PackageHeaderView {
                name: package.name.clone(),
                identity: package_page_identity(package),
                identity_href: package_identity_href(&package.id),
                has_identity_link: matches!(package.id, PackageId::Remote(_)),
            },
            modules: &modules,
            has_modules: !modules.is_empty(),
        }
        .render()
        .map_err(DocError::template)
    }

    fn render_module_page(
        &self,
        package: &PackageModel,
        module: &ModuleModel,
    ) -> Result<String, DocError> {
        let current_page = self
            .site_paths
            .module_pages
            .get(&module.key())
            .expect("site paths missing module page");
        let package_page = self
            .site_paths
            .package_pages
            .get(&package.id)
            .expect("site paths missing package page");
        let (path_prefix, has_path_prefix, name) = module_path_parts(&module.path);
        let base = self.base_page(
            current_page,
            format!(
                "{} · {} · Par Docs",
                module.path.to_slash_path(),
                package.name
            ),
            vec![
                BreadcrumbView::link(
                    "Packages",
                    relative_href(current_page, &self.site_paths.index),
                ),
                BreadcrumbView::link(&package.name, relative_href(current_page, package_page)),
                BreadcrumbView::current_with_prefix(&path_prefix, has_path_prefix, &name),
            ],
        );
        let content_html = self.render_module_content(module, current_page, true)?;

        ModulePageTemplate {
            base: &base,
            module: &ModuleHeaderView {
                path_prefix,
                has_path_prefix,
                name,
                is_unexported: !module.is_exported,
            },
            content_html: &content_html,
        }
        .render()
        .map_err(DocError::template)
    }

    fn render_module_content(
        &self,
        module: &ModuleModel,
        current_page: &Path,
        include_ids: bool,
    ) -> Result<String, DocError> {
        let module_page = self
            .site_paths
            .module_pages
            .get(&module.key())
            .expect("site paths missing module page");
        let items = module
            .items
            .iter()
            .map(|item| {
                let anchor = item_anchor(item.kind, &item.name.primary);
                let item_href = relative_href_with_anchor(current_page, module_page, &anchor);
                Ok(ItemView {
                    anchor,
                    include_id: include_ids,
                    signature_html: html::render_item_signature(
                        item,
                        &item_href,
                        current_page,
                        &self.type_targets,
                    ),
                    doc_html: item
                        .doc_markdown
                        .as_deref()
                        .map(html::render_markdown)
                        .unwrap_or_default(),
                    has_doc: item.doc_markdown.is_some(),
                    is_unexported: !item.is_public,
                    copy_href: item_href,
                })
            })
            .collect::<Result<Vec<_>, DocError>>()?;

        ModuleContentTemplate {
            items: &items,
            is_empty: items.is_empty(),
        }
        .render()
        .map_err(DocError::template)
    }

    fn section_views(&self, current_page: &Path) -> Vec<PackageSectionView> {
        [
            ("", &self.site.sections.root),
            ("Built In", &self.site.sections.built_in),
            ("Dependencies", &self.site.sections.direct_dependencies),
            (
                "Indirect Dependencies",
                &self.site.sections.indirect_dependencies,
            ),
        ]
        .into_iter()
        .filter_map(|(heading, ids)| {
            if ids.is_empty() {
                return None;
            }

            let packages = ids
                .iter()
                .map(|id| {
                    let package = self
                        .site
                        .package(id)
                        .expect("package section references missing package");
                    PackageCardView {
                        name: package.name.clone(),
                        identity: package.identity.clone(),
                        href: relative_href(
                            current_page,
                            self.site_paths
                                .package_pages
                                .get(id)
                                .expect("site paths missing package page"),
                        ),
                    }
                })
                .collect::<Vec<_>>();

            Some(PackageSectionView {
                heading: heading.to_string(),
                show_heading: !heading.is_empty(),
                packages,
            })
        })
        .collect()
    }

    fn package_module_view(
        &self,
        module: &ModuleModel,
        current_page: &Path,
    ) -> Result<PackageModuleView, DocError> {
        let module_page = self
            .site_paths
            .module_pages
            .get(&module.key())
            .expect("site paths missing module page");
        let content_html = self.render_module_content(module, current_page, false)?;
        let (path_prefix, has_path_prefix, name) = module_path_parts(&module.path);

        Ok(PackageModuleView {
            path_prefix,
            has_path_prefix,
            name,
            href: relative_href(current_page, module_page),
            is_unexported: !module.is_exported,
            content_html,
        })
    }

    fn base_page(
        &self,
        current_page: &Path,
        title: String,
        breadcrumbs: Vec<BreadcrumbView>,
    ) -> BasePageView {
        BasePageView {
            title,
            stylesheet_href: relative_href(current_page, &self.site_paths.css),
            script_href: relative_href(current_page, &self.site_paths.js),
            breadcrumbs,
        }
    }
}

fn write_file(out_dir: &Path, relative_path: &Path, contents: &str) -> Result<(), DocError> {
    let full_path = out_dir.join(relative_path);
    if let Some(parent) = full_path.parent() {
        fs::create_dir_all(parent).map_err(|error| DocError::io(parent, error))?;
    }
    fs::write(&full_path, contents).map_err(|error| DocError::io(&full_path, error))
}

fn module_path_parts(path: &par_core::workspace::ModulePath) -> (String, bool, String) {
    if path.directories.is_empty() {
        (String::new(), false, path.module.clone())
    } else {
        (
            format!("{} /", path.directories.join(" / ")),
            true,
            path.module.clone(),
        )
    }
}

fn package_identity_href(id: &PackageId) -> String {
    match id {
        PackageId::Remote(path) => format!("https://{path}"),
        PackageId::Special(_) | PackageId::Local(_) => String::new(),
    }
}

fn package_page_identity(package: &PackageModel) -> String {
    match package.kind {
        PackageKind::Root => String::from("This package"),
        PackageKind::BuiltIn => String::from("Built-in package"),
        PackageKind::DirectDependency | PackageKind::IndirectDependency => package.identity.clone(),
    }
}

#[derive(Debug, Clone)]
struct BasePageView {
    title: String,
    stylesheet_href: String,
    script_href: String,
    breadcrumbs: Vec<BreadcrumbView>,
}

#[derive(Debug, Clone)]
struct BreadcrumbView {
    prefix: String,
    has_prefix: bool,
    label: String,
    href: String,
    is_link: bool,
}

impl BreadcrumbView {
    fn link(label: &str, href: String) -> Self {
        Self {
            prefix: String::new(),
            has_prefix: false,
            label: label.to_string(),
            href,
            is_link: true,
        }
    }

    fn current(label: &str) -> Self {
        Self {
            prefix: String::new(),
            has_prefix: false,
            label: label.to_string(),
            href: String::new(),
            is_link: false,
        }
    }

    fn current_with_prefix(prefix: &str, has_prefix: bool, label: &str) -> Self {
        Self {
            prefix: prefix.to_string(),
            has_prefix,
            label: label.to_string(),
            href: String::new(),
            is_link: false,
        }
    }
}

#[derive(Debug, Clone)]
struct PackageSectionView {
    heading: String,
    show_heading: bool,
    packages: Vec<PackageCardView>,
}

#[derive(Debug, Clone)]
struct PackageCardView {
    name: String,
    identity: String,
    href: String,
}

#[derive(Debug, Clone)]
struct PackageHeaderView {
    name: String,
    identity: String,
    identity_href: String,
    has_identity_link: bool,
}

#[derive(Debug, Clone)]
struct PackageModuleView {
    path_prefix: String,
    has_path_prefix: bool,
    name: String,
    href: String,
    is_unexported: bool,
    content_html: String,
}

#[derive(Debug, Clone)]
struct ModuleHeaderView {
    path_prefix: String,
    has_path_prefix: bool,
    name: String,
    is_unexported: bool,
}

#[derive(Debug, Clone)]
struct ItemView {
    anchor: String,
    include_id: bool,
    signature_html: String,
    doc_html: String,
    has_doc: bool,
    is_unexported: bool,
    copy_href: String,
}

#[derive(Template)]
#[template(path = "packages.html")]
struct PackagesPageTemplate<'a> {
    base: &'a BasePageView,
    sections: &'a [PackageSectionView],
}

#[derive(Template)]
#[template(path = "package.html")]
struct PackagePageTemplate<'a> {
    base: &'a BasePageView,
    package: &'a PackageHeaderView,
    modules: &'a [PackageModuleView],
    has_modules: bool,
}

#[derive(Template)]
#[template(path = "module.html")]
struct ModulePageTemplate<'a> {
    base: &'a BasePageView,
    module: &'a ModuleHeaderView,
    content_html: &'a str,
}

#[derive(Template)]
#[template(path = "module_content.html")]
struct ModuleContentTemplate<'a> {
    items: &'a [ItemView],
    is_empty: bool,
}
