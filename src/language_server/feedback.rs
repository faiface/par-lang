use crate::language_server::instance::CompileError;
use crate::package_utils::{format_with_source_span, source_for_type_error};
use lsp_types::{self as lsp, Uri};
use miette::Diagnostic;
use par_core::source::{FileName, Span, Spanning};
use par_core::workspace::{PackageLoadError, WorkspaceError};
use std::collections::HashMap;
use std::path::Path;
use url::Url;

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

pub fn diagnostic_for_error(err: &CompileError, fallback_uri: &Uri) -> (Uri, lsp::Diagnostic) {
    let (span, message, help, _related_spans) = match err {
        CompileError::Type {
            error,
            file_scope,
            sources,
        } => {
            let (span, related_span) = error.spans();
            (
                span,
                format!(
                    "{:?}",
                    error.to_report_in_scope(
                        source_for_type_error(error, sources),
                        file_scope.as_ref(),
                    )
                ),
                None,
                related_span.into_iter().collect(),
            )
        }
        CompileError::Workspace(WorkspaceError::Load(PackageLoadError::ParseError {
            source,
            error,
            ..
        })) => (
            error.span(),
            format!(
                "{:?}",
                miette::Report::from(error.to_owned()).with_source_code(source.clone())
            ),
            error.help().map(|s| s.to_string()),
            vec![],
        ),
        CompileError::Workspace(WorkspaceError::LowerError { source, error, .. }) => {
            let span = error.span();
            let report = error.to_report(source.clone());
            (span, format!("{report:?}"), None, vec![])
        }
        CompileError::Workspace(error @ WorkspaceError::UnknownDependency { source, span, .. })
        | CompileError::Workspace(
            error @ WorkspaceError::ImportedModuleNotFound { source, span, .. },
        )
        | CompileError::Workspace(
            error @ WorkspaceError::DuplicateImportAlias { source, span, .. },
        )
        | CompileError::Workspace(
            error @ WorkspaceError::BindingNameConflictsWithImportAlias { source, span, .. },
        )
        | CompileError::Workspace(
            error @ WorkspaceError::UnknownModuleQualifier { source, span, .. },
        )
        | CompileError::Workspace(
            error @ WorkspaceError::QualifiedCurrentModuleReference { source, span, .. },
        ) => (
            span.clone(),
            format_with_source_span(source.clone(), span, error.to_string()),
            None,
            vec![],
        ),
        CompileError::Workspace(error) => (Span::None, error.to_string(), None, vec![]),
    };
    let message = match help {
        Some(help) => format!("{}\n{}", message, help),
        None => message,
    };
    (
        uri_for_error(err).unwrap_or_else(|| fallback_uri.clone()),
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
        },
    )
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

fn uri_for_error(err: &CompileError) -> Option<Uri> {
    match err {
        CompileError::Type { error, .. } => {
            let (span, _) = error.spans();
            uri_for_span(&span)
        }
        CompileError::Workspace(WorkspaceError::Load(error)) => uri_for_load_error(error),
        CompileError::Workspace(WorkspaceError::LowerError { file, .. }) => {
            file_name_to_uri(file)
        }
        CompileError::Workspace(WorkspaceError::UnknownDependency { span, .. })
        | CompileError::Workspace(WorkspaceError::ImportedModuleNotFound { span, .. })
        | CompileError::Workspace(WorkspaceError::DuplicateImportAlias { span, .. })
        | CompileError::Workspace(WorkspaceError::BindingNameConflictsWithImportAlias {
            span,
            ..
        })
        | CompileError::Workspace(WorkspaceError::UnknownModuleQualifier { span, .. })
        | CompileError::Workspace(WorkspaceError::QualifiedCurrentModuleReference {
            span, ..
        }) => uri_for_span(span),
        CompileError::Workspace(WorkspaceError::UnattachedExternalModule { .. }) => None,
    }
}

fn uri_for_load_error(error: &PackageLoadError) -> Option<Uri> {
    match error {
        PackageLoadError::ParseError { file, .. }
        | PackageLoadError::MissingModuleDeclaration { file }
        | PackageLoadError::FileNameModuleMismatch { file, .. }
        | PackageLoadError::ConflictingModuleNameCasing {
            first_file: file, ..
        } => file_name_to_uri(file),
        PackageLoadError::PackageRootNotFound { .. }
        | PackageLoadError::ManifestReadError { .. }
        | PackageLoadError::SrcDirectoryMissing { .. }
        | PackageLoadError::DirectoryReadError { .. }
        | PackageLoadError::FileReadError { .. }
        | PackageLoadError::InvalidSourceFilePath { .. }
        | PackageLoadError::InvalidSourceFileName { .. } => None,
    }
}

fn uri_for_span(span: &Span) -> Option<Uri> {
    span.file().and_then(|file| file_name_to_uri(&file))
}

fn file_name_to_uri(file: &FileName) -> Option<Uri> {
    if *file == FileName::BUILTIN {
        return None;
    }
    let path = Path::new(file.0.as_str());
    if path.is_absolute() {
        return path_to_uri(path);
    }
    file.0.as_str().parse().ok()
}

fn path_to_uri(path: &Path) -> Option<Uri> {
    Url::from_file_path(path)
        .ok()
        .and_then(|url| url.as_str().parse().ok())
}
