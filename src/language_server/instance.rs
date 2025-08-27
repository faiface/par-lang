use super::io::IO;
use crate::language_server::data::{semantic_token_modifiers, semantic_token_types};
use crate::location::{FileName, Span};
use crate::playground::BuildResult;
use lsp_types::{self as lsp, Uri};
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum CompileError {
    Compile(crate::playground::Error),
    //Types(TypeError<Internal<Name>>),
}

pub struct Instance {
    uri: Uri,
    file: FileName,
    dirty: bool,
    build: BuildResult,
    io: IO,
}

impl Instance {
    pub fn new(uri: Uri, io: IO) -> Instance {
        Self {
            file: uri.as_str().into(),
            uri,
            dirty: true,
            build: BuildResult::None,
            io,
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::debug!("Handling hover request with params: {:?}", params);

        let pos = params.text_document_position_params.position;

        let payload = match self.build.type_on_hover() {
            Some(type_on_hover) => {
                if let Some(name_info) = type_on_hover.query(&self.file, pos.line, pos.character) {
                    let mut buf = String::new();
                    if let Some(name) = name_info.name {
                        write!(&mut buf, "{}: ", name).unwrap();
                    }
                    name_info.typ.pretty(&mut buf, 0).unwrap();
                    lsp::MarkedString::LanguageString(lsp::LanguageString {
                        language: "par".to_owned(),
                        value: buf,
                    })
                } else {
                    return None;
                }
            }
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

        let Some(checked) = self.build.checked() else {
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
                            start: start.into(),
                            end: end.into(),
                        },
                        selection_range: lsp::Range {
                            start: name_start.into(),
                            end: name_end.into(),
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
                            start: start.into(),
                            end: end.into(),
                        },
                        selection_range: lsp::Range {
                            start: name_start.into(),
                            end: name_end.into(),
                        },
                        children: None,
                    },
                );
            }
        }

        for (name, definition) in &checked.definitions {
            if let (Some((name_start, name_end)), Some((start, end))) =
                (name.span.points(), definition.span.points())
            {
                let range = lsp::Range {
                    start: start.into(),
                    end: end.into(),
                };
                let selection_range = lsp::Range {
                    start: name_start.into(),
                    end: name_end.into(),
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
        let Some(type_on_hover) = self.build.type_on_hover() else {
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
                start: start.into(),
                end: end.into(),
            },
        }))
    }

    pub fn handle_goto_definition(
        &self,
        params: &lsp::GotoDefinitionParams,
    ) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!("Handling goto definition request with params: {:?}", params);
        let Some(type_on_hover) = self.build.type_on_hover() else {
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
                start: start.into(),
                end: end.into(),
            },
        }))
    }

    // todo: caching
    pub fn provide_semantic_tokens(
        &self,
        params: &lsp::SemanticTokensParams,
    ) -> Option<lsp::SemanticTokensResult> {
        tracing::info!("Handling semantic tokens request with params: {:?}", params);
        let Some(checked) = self.build.checked() else {
            return None;
        };

        let mut semantic_tokens = Vec::new();

        for (name, _) in checked.type_defs.globals.as_ref() {
            if let Some(start) = name.span.start() {
                semantic_tokens.push(lsp::SemanticToken {
                    delta_line: start.row as u32,
                    delta_start: start.column as u32,
                    length: name.span.len() as u32,
                    token_type: semantic_token_types::TYPE,
                    token_modifiers_bitset: 0u32,
                });
            }
        }

        for (name, _) in &checked.declarations {
            if let Some(start) = name.span.start() {
                semantic_tokens.push(lsp::SemanticToken {
                    delta_line: start.row as u32,
                    delta_start: start.column as u32,
                    length: name.span.len() as u32,
                    token_type: semantic_token_types::FUNCTION,
                    token_modifiers_bitset: semantic_token_modifiers::DECLARATION
                        | semantic_token_modifiers::READONLY,
                });
            }
        }

        for (name, _) in &checked.definitions {
            if let Some(start) = name.span.start() {
                semantic_tokens.push(lsp::SemanticToken {
                    delta_line: start.row as u32,
                    delta_start: start.column as u32,
                    length: name.span.len() as u32,
                    token_type: semantic_token_types::FUNCTION,
                    token_modifiers_bitset: semantic_token_modifiers::DEFINITION
                        | semantic_token_modifiers::READONLY,
                });
            }
        }

        semantic_tokens.sort_by(|a, b| a.delta_line.cmp(&b.delta_line));
        let mut line = 0;
        let mut start = 0;
        for token in &mut semantic_tokens {
            token.delta_line -= line;
            if token.delta_line == 0 {
                token.delta_start -= start;
                start += token.delta_start;
            } else {
                start = 0;
            }
            line += token.delta_line;
        }

        let result = Some(lsp::SemanticTokensResult::Tokens(lsp::SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        }));
        tracing::info!("Providing semantic tokens: {:?}", result);
        result
    }

    pub fn provide_code_lens(&self, params: &lsp::CodeLensParams) -> Option<Vec<lsp::CodeLens>> {
        tracing::debug!("Handling code lens request with params: {:?}", params);
        let Some(checked) = self.build.checked() else {
            return None;
        };

        Some(
            checked
                .definitions
                .iter()
                .filter_map(|(name, def)| name.span.points().map(|pts| (pts, name, def)))
                .map(|((start, end), name, _)| lsp::CodeLens {
                    range: lsp::Range {
                        start: start.into(),
                        end: end.into(),
                    },
                    command: Some(lsp::Command {
                        title: "$(play) Run".to_owned(),
                        command: "run".to_owned(),
                        arguments: Some(vec![self.uri.to_string().into(), name.to_string().into()]),
                    }),
                    data: None,
                })
                .collect(),
        )
    }

    pub fn provide_inlay_hints(
        &self,
        params: &lsp::InlayHintParams,
    ) -> Option<Vec<lsp::InlayHint>> {
        tracing::debug!("Handling inlay hints request with params: {:?}", params);
        let Some(checked) = self.build.checked() else {
            return None;
        };

        Some(
            checked
                .definitions
                .iter()
                .filter(|(name, _)| !checked.declarations.contains_key(*name))
                .filter_map(|(name, def)| name.span.points().map(|pts| (pts, name, def)))
                .map(|((_, end), _, definition)| {
                    let mut label = ": ".to_owned();
                    definition
                        .expression
                        .get_type()
                        .pretty_compact(&mut label)
                        .unwrap();

                    lsp::InlayHint {
                        position: end.into(),
                        label: lsp::InlayHintLabel::String(label),
                        kind: Some(lsp::InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: None,
                        padding_right: None,
                        data: None,
                    }
                })
                .collect(),
        )
    }

    pub fn run_in_playground(&self, def_name: &str) -> Option<serde_json::Value> {
        tracing::info!("Handling playground request with def_name: {:?}", def_name);
        let Some(checked) = self.build.checked() else {
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
        let code = self.io.read(&self.uri);

        self.build = stacker::grow(32 * 1024 * 1024, || {
            BuildResult::from_source(&code.unwrap(), self.file.clone())
        });
        tracing::info!("Compiled!");
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}

fn is_inside(pos: lsp::Position, span: &Span) -> bool {
    let Some((start, end)) = span.points() else {
        return false;
    };

    let pos_row = pos.line;
    let pos_column = pos.character;

    !(pos_row < start.row || pos_row > end.row)
        && !(pos_row == start.row && pos_column < start.column)
        && !(pos_row == end.row && pos_column > end.column)
}
