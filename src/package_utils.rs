use std::collections::HashMap;
use std::sync::Arc;

use par_core::frontend::TypeError;
use par_core::frontend::language::{PackageId, Universal};
use par_core::source::FileName;
use par_core::workspace::ModulePath;

pub type SourceLookup = HashMap<FileName, Arc<str>>;

pub fn source_for_fallback(sources: &SourceLookup) -> Arc<str> {
    sources
        .values()
        .next()
        .cloned()
        .unwrap_or_else(|| Arc::from(""))
}

pub fn source_for_type_error(error: &TypeError<Universal>, sources: &SourceLookup) -> Arc<str> {
    let (span, _related) = error.spans();
    span.file()
        .and_then(|file| sources.get(&file).cloned())
        .unwrap_or_else(|| source_for_fallback(sources))
}

pub fn root_module_slash_path(root_package: &PackageId, module: &Universal) -> Option<String> {
    if &module.package != root_package {
        return None;
    }
    if module.directories.is_empty() {
        return Some(module.module.clone());
    }
    Some(format!(
        "{}/{}",
        module.directories.join("/"),
        module.module
    ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedTarget {
    pub module_path: String,
    pub definition_name: Option<String>,
}

pub fn parse_target(target: &str) -> ParsedTarget {
    let target = target.trim();
    if target.is_empty() {
        return ParsedTarget {
            module_path: "Main".to_string(),
            definition_name: None,
        };
    }

    if let Some(last_slash_index) = target.rfind('/') {
        let after_last_slash = &target[last_slash_index + 1..];
        if let Some(dot_index) = after_last_slash.rfind('.') {
            let module = format!(
                "{}{}",
                &target[..last_slash_index + 1],
                &after_last_slash[..dot_index]
            );
            let definition = after_last_slash[dot_index + 1..].to_string();
            return ParsedTarget {
                module_path: module,
                definition_name: Some(definition),
            };
        }
        return ParsedTarget {
            module_path: target.to_string(),
            definition_name: None,
        };
    }

    if let Some(dot_index) = target.rfind('.') {
        let module = target[..dot_index].to_string();
        let definition = target[dot_index + 1..].to_string();
        return ParsedTarget {
            module_path: module,
            definition_name: Some(definition),
        };
    }

    ParsedTarget {
        module_path: target.to_string(),
        definition_name: None,
    }
}

pub fn find_local_module<'a>(
    module_target: &str,
    local_modules: &'a [ModulePath],
) -> Option<&'a ModulePath> {
    let module_target = module_target.trim_matches('/');
    if module_target.is_empty() {
        return None;
    }

    let mut segments: Vec<&str> = module_target.split('/').collect();
    let module_name = segments.pop()?;
    let directories: Vec<String> = segments
        .into_iter()
        .map(|segment| segment.to_lowercase())
        .collect();

    local_modules.iter().find(|candidate| {
        candidate.directories == directories && candidate.module.eq_ignore_ascii_case(module_name)
    })
}
