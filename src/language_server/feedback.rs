use crate::language_server::instance::CompileError;
use lsp_types::{self as lsp, Uri};
use miette::Diagnostic;
use par_core::source::{Span, Spanning};
use std::collections::HashMap;
use std::sync::Arc;

pub struct Feedback {
    diagnostics: HashMap<Uri, Vec<lsp::Diagnostic>>,
}

impl Feedback {
    pub fn new() -> Feedback {
        Self {
            diagnostics: HashMap::new(),
        }
    }

    pub fn diagnostics(&self) -> &HashMap<Uri, Vec<lsp::Diagnostic>> {
        &self.diagnostics
    }

    pub fn add_diagnostic(&mut self, uri: Uri, diagnostic: lsp::Diagnostic) {
        self.diagnostics.entry(uri).or_default().push(diagnostic);
    }
}

pub struct FeedbackBookKeeper {
    feedback: Feedback,
}

impl FeedbackBookKeeper {
    pub fn new() -> FeedbackBookKeeper {
        Self {
            feedback: Feedback::new(),
        }
    }

    /// The last feedback with empty diagnostics
    /// for all URIs, so that the client can clear
    pub fn cleanup(&mut self) -> &mut Feedback {
        let feedback = Feedback::new();
        let last_feedback = std::mem::replace(&mut self.feedback, feedback);
        for (uri, diagnostics) in last_feedback.diagnostics.into_iter() {
            if !diagnostics.is_empty() {
                self.feedback.diagnostics.entry(uri).or_default();
            }
        }
        &mut self.feedback
    }

    pub fn diagnostics(&self) -> &HashMap<Uri, Vec<lsp::Diagnostic>> {
        self.feedback.diagnostics()
    }
}

pub fn diagnostic_for_error(err: &CompileError, code: Arc<str>) -> lsp::Diagnostic {
    let (span, message, help, _related_spans) = match err {
        CompileError::ParseAndCompile(par_core::frontend::ParseAndCompileError::Parse(err)) => (
            err.span(),
            format!(
                "{:?}",
                miette::Report::from(err.to_owned()).with_source_code(code)
            ),
            err.help().map(|s| s.to_string()),
            vec![],
        ),
        CompileError::ParseAndCompile(par_core::frontend::ParseAndCompileError::Compile(error)) => {
            let span = error.span();
            let error = error.to_report(code);
            (span, format!("{error:?}"), None, vec![])
        }
        CompileError::Type(err) => {
            let (span, related_span) = err.spans();
            (
                span,
                format!("{:?}", err.to_report(code)),
                None,
                related_span.into_iter().collect(),
            )
        }
    };
    let message = match help {
        Some(help) => format!("{}\n{}", message, help),
        None => message,
    };
    lsp::Diagnostic {
        range: span_to_lsp_range(&span),
        severity: Some(lsp::DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message,
        related_information: None, // todo
        tags: None,
        data: None,
    }
}

fn span_to_lsp_range(span: &Span) -> lsp::Range {
    match span {
        Span::None => lsp::Range {
            start: lsp::Position {
                line: 0,
                character: 0,
            },
            end: lsp::Position {
                line: 0,
                character: 0,
            },
        },
        Span::At { start, end, .. } => lsp::Range {
            start: lsp::Position {
                line: start.row as u32,
                character: start.column as u32,
            },
            end: lsp::Position {
                line: end.row as u32,
                character: end.column as u32,
            },
        },
    }
}
