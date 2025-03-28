use std::collections::HashMap;
use lsp_types::{self as lsp, Uri};
use crate::language_server::data::{semantic_token_modifiers, semantic_token_types, SEMANTIC_TOKEN_MODIFIERS, SEMANTIC_TOKEN_TYPES};
use crate::par::language::{Declaration, Definition, Internal, Name, TypeDef};
use crate::location::Span;
use crate::par::types::TypeError;
use crate::playground::Compiled;
use super::io::IO;

#[derive(Debug, Clone)]
pub enum CompileError {
    Compile(crate::playground::Error),
    Types(TypeError<Internal<Name>>),
}
pub type CompileResult = Result<Compiled, CompileError>;

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
            io
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::debug!("Handling hover request with params: {:?}", params);

        let pos = params.text_document_position_params.position;

        let payload = match &self.compiled {
            Some(Ok(compiled)) => {
                let checked = compiled.checked.as_ref().unwrap();

                let mut message: Option<String> = Some(format!("{}:{}", pos.line, pos.character));

                let mut inside_item = false;

                for TypeDef { span, name, .. } in &checked.program.type_defs {
                    if !is_inside(pos, span) {
                        continue;
                    }
                    inside_item = true;
                    message = Some(format!("Type: {}", name.to_string()));
                    break;
                }

                if !inside_item {
                    for Declaration { span, name, typ } in &checked.program.declarations {
                        if !is_inside(pos, span) {
                            continue;
                        }
                        inside_item = true;
                        let mut msg = format!("Declaration: {}: ", name.to_string());
                        let indent = msg.len();
                        typ.pretty(&mut msg, indent + 1).unwrap();
                        message = Some(msg);
                        break;
                    }
                }

                if !inside_item {
                    for Definition { span, name, expression } in &checked.program.definitions {
                        if !is_inside(pos, span) {
                            continue;
                        }
                        inside_item = true;
                        let mut msg = format!("Definition: {}: ", name.to_string());
                        let indent = msg.len();
                        expression.get_type().pretty(&mut msg, indent + 1).unwrap();
                        message = Some(msg);
                        break;
                    }
                }

                if let Some(message) = message {
                    message
                } else {
                    format!("Compiled:\n{}", compiled.pretty.clone())
                }
            },
            Some(Err(e)) => format!("Compiled error: {:?}", e),
            None => "Not compiled".to_string(),
        };

        let hover = lsp::Hover {
            contents: lsp::HoverContents::Scalar(
                lsp::MarkedString::String(payload)
            ),
            range: None,
        };
        Some(hover)
    }

    /* todo:
    look at C language servers, how they handle split declaration/definition
    look at Rust language servers, what "kind" they use for type aliases & traits
     */
    pub fn provide_document_symbols(&self, params: &lsp::DocumentSymbolParams) -> Option<lsp::DocumentSymbolResponse> {
        tracing::debug!("Handling symbols request with params: {:?}", params);

        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let mut symbols = HashMap::new();

        for TypeDef { span, name, .. } in &compiled.program.type_defs {
            symbols.insert(name, lsp::DocumentSymbol {
                name: name.to_string(),
                detail: None,
                kind: lsp::SymbolKind::INTERFACE, // fits best?
                tags: None,
                deprecated: None, // must be specified
                range: span.into(),
                selection_range: name.span().unwrap().into(),
                children: None,
            });
        }

        for Declaration { span, name, typ } in &compiled.program.declarations {
            let mut detail = String::new();
            typ.pretty(&mut detail, 0).unwrap();
            symbols.insert(name, lsp::DocumentSymbol {
                name: name.to_string(),
                detail: Some(detail),
                kind: lsp::SymbolKind::FUNCTION, // something else for non-functions?
                tags: None,
                deprecated: None, // must be specified
                range: span.into(),
                selection_range: name.span().unwrap().into(),
                children: None,
            });
        }

        for Definition { span, name, .. } in &compiled.program.definitions {
            let range = span.into();
            let selection_range = name.span().unwrap().into();
            symbols.entry(name)
                .and_modify(|symbol| {
                    symbol.range = range;
                    symbol.selection_range = selection_range;
                })
                .or_insert(lsp::DocumentSymbol {
                    name: name.to_string(),
                    detail: None,
                    kind: lsp::SymbolKind::FUNCTION, // something else for non-functions?
                    tags: None,
                    deprecated: None, // must be specified
                    range,
                    selection_range,
                    children: None,
                });
        }

        Some(lsp::DocumentSymbolResponse::Nested(
            symbols.into_iter().map(|(_, v)| v).collect()
        ))
    }

    pub fn handle_goto_declaration(&self, params: &lsp::GotoDefinitionParams) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!("Handling goto declaration request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let mut original = None;

        for definition in &compiled.program.definitions {
            if is_inside(pos, &definition.name.span().unwrap()) {
                let Some(declaration) = compiled.program.declarations.iter().find(|dec| {
                    dec.name == definition.name
                }) else {
                    return None;
                };

                original = Some(declaration);
                break;
            }
        }

        for declaration in &compiled.program.declarations {
            if is_inside(pos, &declaration.name.span().unwrap()) {
                original = Some(declaration);
                break;
            }
        }

        let declaration = original?;

        Some(lsp::GotoDefinitionResponse::Scalar(
            lsp::Location {
                uri: self.uri.clone(),
                range: declaration.name.span().unwrap().into()
            }
        ))
    }

    pub fn handle_goto_definition(&self, params: &lsp::GotoDefinitionParams) -> Option<lsp::GotoDefinitionResponse> {
        // todo: locals

        tracing::debug!("Handling goto definition request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let pos = params.text_document_position_params.position;

        let mut original = None;

        for declaration in &compiled.program.declarations {
            if is_inside(pos, &declaration.name.span().unwrap()) {
                let Some(definition) = compiled.program.definitions.iter().find(|definition| {
                    definition.name == declaration.name
                }) else {
                    return None;
                };

                original = Some(definition);
                break;
            }
        }

        for definition in &compiled.program.definitions {
            if is_inside(pos, &definition.name.span().unwrap()) {
                original = Some(definition);
                break;
            }
        }

        let definition = original?;

        Some(lsp::GotoDefinitionResponse::Scalar(
            lsp::Location {
                uri: self.uri.clone(),
                range: definition.name.span().unwrap().into()
            }
        ))
    }

    // todo: caching
    pub fn provide_semantic_tokens(&self, params: &lsp::SemanticTokensParams) -> Option<lsp::SemanticTokensResult> {
        tracing::info!("Handling semantic tokens request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let mut semantic_tokens = Vec::new();

        for TypeDef { name, .. } in &compiled.program.type_defs {
            let name_span = name.span().unwrap();
            semantic_tokens.push(lsp::SemanticToken {
                delta_line: name_span.start.row as u32,
                delta_start: name_span.start.column as u32,
                length: name_span.len() as u32,
                token_type: semantic_token_types::TYPE,
                token_modifiers_bitset: 0u32
            });
        }

        for Declaration { name, .. } in &compiled.program.declarations {
            let name_span = name.span().unwrap();
            semantic_tokens.push(lsp::SemanticToken {
                delta_line: name_span.start.row as u32,
                delta_start: name_span.start.column as u32,
                length: name_span.len() as u32,
                token_type: semantic_token_types::FUNCTION, // maybe also something else
                token_modifiers_bitset: semantic_token_modifiers::DECLARATION,
            });
        }

        for Definition { name, .. } in &compiled.program.definitions {
            let name_span = name.span().unwrap();
            semantic_tokens.push(lsp::SemanticToken {
                delta_line: name_span.start.row as u32,
                delta_start: name_span.start.column as u32,
                length: name_span.len() as u32,
                token_type: semantic_token_types::FUNCTION, // maybe also something else
                token_modifiers_bitset: semantic_token_modifiers::DEFINITION,
            });
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

        // todo: display run button over declaration if it is exactly one line above definition
        Some(compiled.program.definitions.iter().map(|definition| {
            lsp::CodeLens {
                range: definition.name.span().unwrap().into(),
                command: Some(lsp::Command {
                    title: "$(play) Run".to_owned(),
                    command: "run".to_owned(),
                    arguments: Some(vec![
                        self.uri.to_string().into(),
                        definition.name.to_string().into(),
                    ]),
                }),
                data: None,
            }
        }).collect())
    }

    pub fn provide_inlay_hints(&self, params: &lsp::InlayHintParams) -> Option<Vec<lsp::InlayHint>> {
        tracing::debug!("Handling inlay hints request with params: {:?}", params);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };
        let checked = compiled.checked.as_ref().unwrap();

        Some(checked.program.definitions.iter()
            .filter(|definition| {
                !checked.program.declarations.iter().any(|declaration| {
                    definition.name == declaration.name
                })
            })
            .map(|Definition { name, expression, .. }| {
                let mut label = ": ".to_owned();
                expression.get_type().pretty(&mut label, 0).unwrap();

                lsp::InlayHint {
                    position: name.span().unwrap().end.into(),
                    label: lsp::InlayHintLabel::String(label),
                    kind: Some(lsp::InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                }
            })
            .collect()
        )
    }

    pub fn run_in_playground(&self, def_name: &str) -> Option<serde_json::Value> {
        tracing::info!("Handling playground request with def_name: {:?}", def_name);
        let Some(Ok(compiled)) = &self.compiled else {
            return None;
        };

        let Some(definition) = compiled.program.definitions.iter().find(|definition| {
            definition.name.to_string().as_str() == def_name
        }) else {
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
        let mut compiled = stacker::grow(32 * 1024 * 1024, || {
            Compiled::from_string(&code.unwrap())
        }).map_err(|err| CompileError::Compile(err));
        match compiled {
            Ok(Compiled { checked: Err(err), .. }) => {
                compiled = Err(CompileError::Types(err));
            }
            _ => {}
        }

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
    let pos_row = pos.line as usize;
    let pos_column = pos.character as usize;

    let start = span.start;
    let end = span.end;

    !(pos_row < start.row || pos_row > end.row)
        && !(pos_row == start.row && pos_column < start.column)
        && !(pos_row == end.row && pos_column > end.column)
}