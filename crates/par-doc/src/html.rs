use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{self, Write};
use std::path::{Path, PathBuf};

use par_core::frontend::GlobalNameWriter;
use par_core::frontend::language::{GlobalName, Universal};
use pulldown_cmark::{Options, Parser, html};

use crate::model::{ItemKind, ItemModel};
use crate::paths::relative_href_with_anchor;

#[derive(Debug, Clone)]
pub struct TypeLinkTarget {
    pub page: PathBuf,
    pub anchor: String,
}

pub fn render_markdown(markdown: &str) -> String {
    let parser = Parser::new_ext(
        markdown,
        Options::ENABLE_TABLES
            | Options::ENABLE_FOOTNOTES
            | Options::ENABLE_STRIKETHROUGH
            | Options::ENABLE_TASKLISTS,
    );
    let mut output = String::new();
    html::push_html(&mut output, parser);
    output
}

pub fn render_item_signature(
    item: &ItemModel,
    item_href: &str,
    current_page: &Path,
    type_targets: &BTreeMap<GlobalName<Universal>, TypeLinkTarget>,
) -> String {
    let mut output = String::new();
    match item.kind {
        ItemKind::Type => {
            output.push_str(r#"<span class="sig-keyword">type</span> "#);
            output.push_str(&render_item_name_link(&item.name, item_href));
            if !item.params.is_empty() {
                output.push_str("&lt;");
                output.push_str(
                    &item
                        .params
                        .iter()
                        .map(|param| escape_html(param))
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                output.push_str("&gt;");
            }
            output.push_str(" = ");
        }
        ItemKind::Declaration => {
            output.push_str(r#"<span class="sig-keyword">dec</span> "#);
            output.push_str(&render_item_name_link(&item.name, item_href));
            output.push_str(" : ");
        }
    }
    output.push_str(&render_type_html(&item.typ, current_page, type_targets));
    output
}

pub fn escape_html(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&#39;"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn render_type_html(
    typ: &par_core::frontend::Type<Universal>,
    current_page: &Path,
    type_targets: &BTreeMap<GlobalName<Universal>, TypeLinkTarget>,
) -> String {
    let writer = TypeLinkWriter {
        current_page,
        type_targets,
        replacements: RefCell::new(Vec::new()),
    };

    let mut raw = String::new();
    let _ = typ.pretty(&mut raw, &writer, 0);

    let mut escaped = escape_html(&raw);
    for replacement in writer.replacements.into_inner() {
        escaped = escaped.replace(&replacement.marker, &replacement.html);
    }
    escaped
}

fn render_item_name_link(name: &GlobalName<Universal>, href: &str) -> String {
    let module_name = &name.module.module;
    let primary = escape_html(&name.primary);
    let href = escape_html(href);
    let title = escape_html(&full_global_name(name));

    if name.primary == *module_name {
        return format!(
            r#"<a class="item-link" href="{href}" title="{title}"><span class="item-name">{primary}</span></a>"#
        );
    }

    let module_prefix = escape_html(module_name);
    format!(
        r#"<span class="item-module">{module_prefix}.</span><a class="item-link" href="{href}" title="{title}"><span class="item-name">{primary}</span></a>"#
    )
}

fn short_global_name(name: &GlobalName<Universal>) -> String {
    if name.primary == name.module.module {
        name.primary.clone()
    } else {
        format!("{}.{}", name.module.module, name.primary)
    }
}

fn full_global_name(name: &GlobalName<Universal>) -> String {
    name.to_string()
}

struct TypeLinkWriter<'a> {
    current_page: &'a Path,
    type_targets: &'a BTreeMap<GlobalName<Universal>, TypeLinkTarget>,
    replacements: RefCell<Vec<LinkReplacement>>,
}

impl GlobalNameWriter<Universal> for TypeLinkWriter<'_> {
    fn write_global_name<W: Write>(&self, f: &mut W, name: &GlobalName<Universal>) -> fmt::Result {
        let marker = {
            let replacements = self.replacements.borrow();
            format!("__PAR_DOC_LINK_[{}]__", replacements.len())
        };
        let html = render_type_reference(name, self.current_page, self.type_targets);
        self.replacements.borrow_mut().push(LinkReplacement {
            marker: marker.clone(),
            html,
        });
        f.write_str(&marker)
    }
}

#[derive(Debug, Clone)]
struct LinkReplacement {
    marker: String,
    html: String,
}

fn render_type_reference(
    name: &GlobalName<Universal>,
    current_page: &Path,
    type_targets: &BTreeMap<GlobalName<Universal>, TypeLinkTarget>,
) -> String {
    let label = escape_html(&short_global_name(name));
    let title = escape_html(&full_global_name(name));

    match type_targets.get(name) {
        Some(target) => {
            let href = escape_html(&relative_href_with_anchor(
                current_page,
                &target.page,
                &target.anchor,
            ));
            format!(r#"<a class="type-ref" href="{href}" title="{title}">{label}</a>"#)
        }
        None => label,
    }
}
