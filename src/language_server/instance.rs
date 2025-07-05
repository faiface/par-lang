use super::io::IO;
use crate::language_server::data::{semantic_token_modifiers, semantic_token_types};
use crate::location::{Span, Spanning};
use crate::par::program::NameWithType;
use crate::playground::{Checked, Compiled};
use lsp_types::{self as lsp, Uri};
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum CompileError {
    Compile(crate::playground::Error),
    //Types(TypeError<Internal<Name>>),
}
pub type CompileResult = Result<Checked, CompileError>;

pub struct Instance {
    uri: Uri,
    dirty: bool,
    compiled: Option<CompileResult>,
    io: IO,
}

impl Instance {
    pub fn new(uri: Uri, io: IO) -> Instance {
        Self {
            uri,
            dirty: true,
            compiled: None,
            io,
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::debug!("Handling hover request with params: {:?}", params);

        let pos = params.text_document_position_params.position;

        let payload = match &self.compiled {
            Some(Ok(compiled)) => {
                if let Some(NameWithType(name, typ)) = compiled
                    .type_on_hover
                    .query(pos.line as usize, pos.character as usize)
                {
                    let mut buf = String::new();
                    if let Some(name) = name {
                        write!(&mut buf, "{}: ", name).unwrap();
                    }
                    typ.pretty(&mut buf, 0).unwrap();
                    lsp::MarkedString::LanguageString(lsp::LanguageString {
                        language: "par".to_owned(),
                        value: buf,
                    })
                } else {
                    return None;
                }
            }
            Some(Err(_)) => return None,
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

        let Some(Ok(compiled)) = &self.compiled else {
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

        for (name, (span, _, _)) in compiled.program.type_defs.globals.as_ref() {
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

        for (name, declaration) in &compiled.program.declarations {
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

        for (name, definition) in &compiled.program.definitions {
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
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let mut original = None;

        for (name, _) in &compiled.program.definitions {
            if is_inside(pos, &name.span()) {
                let Some(declaration) = compiled.program.declarations.get(name) else {
                    return None;
                };

                original = Some(declaration);
                break;
            }
        }

        for (name, declaration) in &compiled.program.declarations {
            if is_inside(pos, &name.span()) {
                original = Some(declaration);
                break;
            }
        }

        let declaration = original?;

        match declaration.name.span {
            Span::None => None,
            Span::At { start, end } => Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
                uri: self.uri.clone(),
                range: lsp::Range {
                    start: start.into(),
                    end: end.into(),
                },
            })),
        }
    }

    pub fn handle_goto_definition(
        &self,
        params: &lsp::GotoDefinitionParams,
    ) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!("Handling goto definition request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let mut original = None;

        //TODO: use map indexing
        for (name, _) in &compiled.program.declarations {
            if is_inside(pos, &name.span()) {
                let Some(definition) = compiled
                    .program
                    .definitions
                    .iter()
                    .find(|(def_name, _)| def_name == &name)
                else {
                    return None;
                };

                original = Some(definition);
                break;
            }
        }

        for definition in &compiled.program.definitions {
            if is_inside(pos, &definition.0.span()) {
                original = Some(definition);
                break;
            }
        }

        let definition = original?;

        match definition.0.span {
            Span::None => None,
            Span::At { start, end } => Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
                uri: self.uri.clone(),
                range: lsp::Range {
                    start: start.into(),
                    end: end.into(),
                },
            })),
        }
    }

    // todo: caching
    pub fn provide_semantic_tokens(
        &self,
        params: &lsp::SemanticTokensParams,
    ) -> Option<lsp::SemanticTokensResult> {
        tracing::info!("Handling semantic tokens request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let mut semantic_tokens = Vec::new();

        for (name, _) in compiled.program.type_defs.globals.as_ref() {
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

        for (name, _) in &compiled.program.declarations {
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

        for (name, _) in &compiled.program.definitions {
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
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        Some(
            compiled
                .program
                .definitions
                .iter()
                .filter_map(|(name, def)| name.span().points().map(|pts| (pts, name, def)))
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
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        Some(
            compiled
                .program
                .definitions
                .iter()
                .filter(|(name, _)| !compiled.program.declarations.contains_key(*name))
                .filter_map(|(name, def)| name.span().points().map(|pts| (pts, name, def)))
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
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        //TODO: use map indexing
        let Some(_definition) = compiled
            .program
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

    pub fn compile(&mut self) -> Result<(), CompileError> {
        tracing::info!("Compiling: {:?}", self.uri);
        if !self.dirty {
            tracing::info!("No changes");
            tracing::debug!("No changes to compile");
            return Ok(());
        }
        let code = self.io.read(&self.uri);

        // todo: progress reporting
        let compiled = stacker::grow(32 * 1024 * 1024, || Compiled::from_string(&code.unwrap()))
            .map_err(|err| CompileError::Compile(err))
            .and_then(|compiled| compiled.checked.map_err(|err| CompileError::Compile(err)));

        let result = match &compiled {
            Ok(_) => {
                self.dirty = false;
                tracing::info!("Compilation successful");
                Ok(())
            }
            Err(err) => {
                tracing::info!("Compilation failed");
                Err(err.clone())
            }
        };

        self.compiled = Some(compiled);

        result
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}

fn is_inside(pos: lsp::Position, span: &Span) -> bool {
    let Some((start, end)) = span.points() else {
        return false;
    };

    let pos_row = pos.line as usize;
    let pos_column = pos.character as usize;

    !(pos_row < start.row || pos_row > end.row)
        && !(pos_row == start.row && pos_column < start.column)
        && !(pos_row == end.row && pos_column > end.column)
}
