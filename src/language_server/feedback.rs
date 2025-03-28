use std::collections::HashMap;
use lsp_types::{self as lsp, Uri};
use miette::Diagnostic;
use crate::language_server::instance::CompileError;
use crate::location::{Span, Spanning};

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

pub fn diagnostic_for_error(err: &CompileError) -> lsp::Diagnostic {
    use crate::playground::Error;

    let (span, message, help, related_span) = match err {
        CompileError::Compile(Error::Parse(err))
        => (
            err.span(),
            err.message().to_string(),
            err.help().map(|s| s.to_string()),
            None,
        ),

        CompileError::Compile(Error::Compile(err))
        => (
            err.span(),
            err.message().to_string(),
            Some("Help".to_string()),
            None,
        ),

        | CompileError::Compile(Error::Type(err))
        | CompileError::Types(err)
        => {
            let (span, related_span) = err.spans();
            (
                span,
                "Type Error".to_string(),
                Some("Help".to_string()),
                related_span,
            )
        },

        CompileError::Compile(Error::Runtime(_)) => {
            unreachable!("Runtime error at compile time")
        },
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
    lsp::Range {
        start: lsp::Position {
            line: span.start.row as u32,
            character: span.start.column as u32,
        },
        end: lsp::Position {
            line: span.end.row as u32,
            character: span.end.column as u32,
        }
    }
}