use super::io::IO;
use crate::language_server::data::ToLspPosition;
use lsp_types::{self as lsp, Uri};
use par_builtin::import_builtins;
use par_core::frontend::{
    CheckedModule, ParseAndCompileError, TypeError, TypeOnHover, lower, parse, type_check,
};
use par_core::source::FileName;
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum CompileError {
    ParseAndCompile(ParseAndCompileError),
    Type(TypeError),
}

pub struct Instance {
    uri: Uri,
    file: FileName,
    dirty: bool,
    checked: Option<Arc<CheckedModule>>,
    type_on_hover: Option<TypeOnHover>,
    error: Option<CompileError>,
    io: IO,
}

impl Instance {
    pub fn new(uri: Uri, io: IO) -> Instance {
        Self {
            file: uri.as_str().into(),
            uri,
            dirty: true,
            checked: None,
            type_on_hover: None,
            error: None,
            io,
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::debug!("Handling hover request with params: {:?}", params);

        let pos = params.text_document_position_params.position;

        let payload = match self.type_on_hover.as_ref() {
            Some(type_on_hover) => match type_on_hover.query(&self.file, pos.line, pos.character) {
                Some(name_info) => {
                    let mut buf = String::new();
                    if let Some(name) = name_info.name {
                        write!(&mut buf, "{}: ", name).unwrap();
                    }
                    name_info.typ.pretty(&mut buf, 0).unwrap();
                    lsp::MarkedString::LanguageString(lsp::LanguageString {
                        language: "par".to_owned(),
                        value: buf,
                    })
                }
                _ => {
                    return None;
                }
            },
            None => lsp::MarkedString::String("Not compiled".to_string()),
        };

        let hover = lsp::Hover {
            contents: lsp::HoverContents::Scalar(payload),
            range: None,
        };
        Some(hover)
    }

    /* todo:
    look at C language servers, how they handle split declaration/definition
    look at Rust language servers, what "kind" they use for type aliases & traits
     */
    #[allow(deprecated)] // some types only allow construction using deprecated fields
    pub fn provide_document_symbols(
        &self,
        params: &lsp::DocumentSymbolParams,
    ) -> Option<lsp::DocumentSymbolResponse> {
        tracing::debug!("Handling symbols request with params: {:?}", params);

        let Some(checked) = self.checked.as_ref() else {
            return None;
        };

        let mut symbols = HashMap::new();

        /* kinds (maybe like this):
        CLASS: choice type
        METHOD: receiving choice branch, trait function
        PROPERTY: general choice branch, trait constant
        ENUM: either type
        INTERFACE: trait
        FUNCTION: value of receiving type
        CONSTANT: value of other type
        OBJECT: value of choice type
        ENUM_MEMBER: either variant
        STRUCT: record
        TYPE_PARAMETER: type alias
         */

        for (name, (span, _, _)) in checked.type_defs.globals.as_ref() {
            if let (Some((name_start, name_end)), Some((start, end))) =
                (name.span.points(), span.points())
            {
                symbols.insert(
                    name,
                    lsp::DocumentSymbol {
                        name: name.to_string(),
                        detail: None,
                        kind: lsp::SymbolKind::INTERFACE,
                        tags: None,
                        deprecated: None, // must be specified
                        range: lsp::Range {
                            start: start.to_lsp_position(),
                            end: end.to_lsp_position(),
                        },
                        selection_range: lsp::Range {
                            start: name_start.to_lsp_position(),
                            end: name_end.to_lsp_position(),
                        },
                        children: None,
                    },
                );
            }
        }

        for (name, declaration) in &checked.declarations {
            let mut detail = String::new();
            declaration.typ.pretty_compact(&mut detail).unwrap();

            if let (Some((name_start, name_end)), Some((start, end))) =
                (name.span.points(), declaration.span.points())
            {
                symbols.insert(
                    name,
                    lsp::DocumentSymbol {
                        name: name.to_string(),
                        detail: Some(detail),
                        kind: lsp::SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None, // must be specified
                        range: lsp::Range {
                            start: start.to_lsp_position(),
                            end: end.to_lsp_position(),
                        },
                        selection_range: lsp::Range {
                            start: name_start.to_lsp_position(),
                            end: name_end.to_lsp_position(),
                        },
                        children: None,
                    },
                );
            }
        }

        for (name, (definition, _typ)) in &checked.definitions {
            if let (Some((name_start, name_end)), Some((start, end))) =
                (name.span.points(), definition.span.points())
            {
                let range = lsp::Range {
                    start: start.to_lsp_position(),
                    end: end.to_lsp_position(),
                };
                let selection_range = lsp::Range {
                    start: name_start.to_lsp_position(),
                    end: name_end.to_lsp_position(),
                };
                symbols
                    .entry(name)
                    .and_modify(|symbol| {
                        symbol.range = range;
                        symbol.selection_range = selection_range;
                    })
                    .or_insert({
                        let typ = definition.expression.get_type();
                        let mut detail = String::new();
                        typ.pretty_compact(&mut detail).unwrap();

                        lsp::DocumentSymbol {
                            name: name.to_string(),
                            detail: Some(detail),
                            kind: lsp::SymbolKind::FUNCTION,
                            tags: None,
                            deprecated: None, // must be specified
                            range,
                            selection_range,
                            children: None,
                        }
                    });
            }
        }

        // todo: fix the bug that causes this
        // the same bug also causes run labels to appear on usages of the name
        for symbol in symbols.values() {
            let range = symbol.range;
            let selection_range = symbol.selection_range;
            let inside = range.start.character <= selection_range.start.character
                && range.start.line <= selection_range.start.line
                && range.end.character >= selection_range.end.character
                && range.end.line >= selection_range.end.line;
            if !inside {
                tracing::error!(
                    "Symbol selection range is not inside the range: {:?}",
                    symbol
                );
            }
        }

        Some(lsp::DocumentSymbolResponse::Nested(
            symbols.into_iter().map(|(_, v)| v).collect(),
        ))
    }

    pub fn handle_goto_declaration(
        &self,
        params: &lsp::GotoDefinitionParams,
    ) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!(
            "Handling goto declaration request with params: {:?}",
            params
        );
        let Some(type_on_hover) = self.type_on_hover.as_ref() else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let name_info = type_on_hover.query(&self.file, pos.line, pos.character)?;

        let (start, end) = name_info.decl_span.points()?;
        let path = name_info.decl_span.file()?;
        if path == FileName::BUILTIN {
            return None;
        }

        Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
            uri: path.0.parse().ok()?,
            range: lsp::Range {
                start: start.to_lsp_position(),
                end: end.to_lsp_position(),
            },
        }))
    }

    pub fn handle_goto_definition(
        &self,
        params: &lsp::GotoDefinitionParams,
    ) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!("Handling goto definition request with params: {:?}", params);
        let Some(type_on_hover) = self.type_on_hover.as_ref() else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let name_info = type_on_hover.query(&self.file, pos.line, pos.character)?;

        let (start, end) = name_info.def_span.points()?;
        let path = name_info.def_span.file()?;
        if path == FileName::BUILTIN {
            return None;
        }

        Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
            uri: path.0.parse().ok()?,
            range: lsp::Range {
                start: start.to_lsp_position(),
                end: end.to_lsp_position(),
            },
        }))
    }

    /// Last compile/type error, if any
    pub fn last_error(&self) -> Option<CompileError> {
        self.error.clone()
    }

    pub fn run_in_playground(&self, def_name: &str) -> Option<serde_json::Value> {
        tracing::info!("Handling playground request with def_name: {:?}", def_name);
        let Some(checked) = self.checked.as_ref() else {
            return None;
        };

        //TODO: use map indexing
        let Some(_definition) = checked
            .definitions
            .iter()
            .find(|(name, _)| name.to_string().as_str() == def_name)
        else {
            return None;
        };

        tracing::warn!("Run in playground is not supported!");

        // todo: run

        None
    }

    pub fn compile(&mut self) {
        tracing::info!("Compiling: {:?}", self.uri);
        if !self.dirty {
            tracing::info!("No changes");
            tracing::debug!("No changes to compile");
            return;
        }
        let Some(code) = self.io.read(&self.uri) else {
            self.checked = None;
            self.type_on_hover = None;
            self.error = None;
            self.dirty = false;
            return;
        };

        let result = stacker::grow(32 * 1024 * 1024, || {
            let parsed = parse(&code, self.file.clone()).map_err(|error| {
                CompileError::ParseAndCompile(ParseAndCompileError::Parse(error))
            })?;
            let mut lowered = lower(parsed).map_err(|error| {
                CompileError::ParseAndCompile(ParseAndCompileError::Compile(error))
            })?;
            import_builtins(&mut lowered);
            let checked = Arc::new(type_check(&lowered).map_err(CompileError::Type)?);
            let type_on_hover = TypeOnHover::new(&checked);
            Ok::<_, CompileError>((checked, type_on_hover))
        });

        match result {
            Ok((checked, type_on_hover)) => {
                self.checked = Some(checked);
                self.type_on_hover = Some(type_on_hover);
                self.error = None;
            }
            Err(error) => {
                self.checked = None;
                self.type_on_hover = None;
                self.error = Some(error);
            }
        }
        tracing::info!("Compiled!");
        // reset dirty flag after successful compile attempt
        self.dirty = false;
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}
