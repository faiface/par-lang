use std::{collections::HashMap, sync::Arc};

use arcstr::ArcStr;
use indexmap::{IndexMap, IndexSet};

use crate::location::{FileName, Point, Span, Spanning};

use super::{
    language::{CompileError, GlobalName, LocalName, Unresolved},
    parse::SyntaxError,
    process::{self, HoverInfo},
    types::{Context, Type, TypeDefs, TypeError},
};

use crate::frontend::language::Expression;

#[derive(Clone, Debug)]
pub struct Module<Expr, S> {
    pub type_defs: Vec<TypeDef<S>>,
    pub declarations: Vec<Declaration<S>>,
    pub definitions: Vec<Definition<Expr, S>>,
}

#[derive(Clone, Debug)]
pub struct ModuleDecl {
    pub span: Span,
    pub exported: bool,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ImportPath {
    pub dependency: Option<String>,
    pub directories: Vec<String>,
    pub module: String,
}

#[derive(Clone, Debug)]
pub struct ImportDecl {
    pub span: Span,
    pub path: ImportPath,
    pub alias: Option<String>,
}

#[derive(Clone, Debug)]
pub struct SourceFile<Expr> {
    pub module_decl: Option<ModuleDecl>,
    pub imports: Vec<ImportDecl>,
    pub body: Module<Expr, Unresolved>,
}

#[derive(Debug, Clone)]
pub struct CheckedModule<S> {
    pub type_defs: TypeDefs<S>,
    pub declarations: IndexMap<GlobalName<S>, Declaration<S>>,
    pub definitions:
        IndexMap<GlobalName<S>, (Definition<Arc<process::Expression<Type<S>, S>>, S>, Type<S>)>,
}

#[derive(Clone, Debug)]
pub struct TypeDef<S> {
    pub span: Span,
    pub exported: bool,
    pub doc: Option<DocComment>,
    pub name: GlobalName<S>,
    pub params: Vec<LocalName>,
    pub typ: Type<S>,
}

#[derive(Clone, Debug)]
pub struct Declaration<S> {
    pub span: Span,
    pub exported: bool,
    pub doc: Option<DocComment>,
    pub name: GlobalName<S>,
    pub typ: Type<S>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocComment {
    pub span: Span,
    pub markdown: ArcStr,
}

#[derive(Clone, Debug, Default)]
pub struct Docs<S> {
    pub types: IndexMap<GlobalName<S>, DocComment>,
    pub declarations: IndexMap<GlobalName<S>, DocComment>,
}

impl<S: std::hash::Hash + Eq> Docs<S> {
    pub fn type_doc(&self, name: &GlobalName<S>) -> Option<&DocComment> {
        self.types.get(name)
    }

    pub fn declaration_doc(&self, name: &GlobalName<S>) -> Option<&DocComment> {
        self.declarations.get(name)
    }
}

#[derive(Clone, Debug)]
pub enum DefinitionBody<Expr> {
    Par(Expr),
    External(Span),
}

impl<S> DefinitionBody<Expression<S>> {
    pub fn span(&self) -> Span {
        match self {
            DefinitionBody::Par(expr) => expr.span(),
            DefinitionBody::External(span) => span.clone(),
        }
    }
}
#[derive(Clone, Debug)]
pub struct Definition<Expr, S> {
    pub span: Span,
    pub name: GlobalName<S>,
    pub body: DefinitionBody<Expr>,
}

impl TypeDef<Unresolved> {
    pub fn external(name: &'static str, params: &[&'static str], typ: Type<Unresolved>) -> Self {
        Self {
            span: Default::default(),
            exported: true,
            doc: None,
            name: GlobalName::<Unresolved>::external(None, name),
            params: params
                .into_iter()
                .map(|&var| LocalName {
                    span: Default::default(),
                    string: ArcStr::from(var),
                })
                .collect(),
            typ,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseAndCompileError {
    Parse(SyntaxError),
    Compile(CompileError),
}

impl From<SyntaxError> for ParseAndCompileError {
    fn from(value: SyntaxError) -> Self {
        Self::Parse(value)
    }
}

impl From<CompileError> for ParseAndCompileError {
    fn from(value: CompileError) -> Self {
        Self::Compile(value)
    }
}

impl<S: Clone> Module<Arc<process::Expression<(), S>>, S> {
    pub fn map_global_names<T, E>(
        self,
        mut map_name: impl FnMut(GlobalName<S>) -> Result<GlobalName<T>, E>,
    ) -> Result<Module<Arc<process::Expression<(), T>>, T>, E> {
        Ok(Module {
            type_defs: self
                .type_defs
                .into_iter()
                .map(
                    |TypeDef {
                         span,
                         exported,
                         doc,
                         name,
                         params,
                         typ,
                     }| {
                        Ok(TypeDef {
                            span,
                            exported,
                            doc,
                            name: map_name(name)?,
                            params,
                            typ: typ.map_global_names(&mut map_name)?,
                        })
                    },
                )
                .collect::<Result<Vec<_>, _>>()?,
            declarations: self
                .declarations
                .into_iter()
                .map(
                    |Declaration {
                         span,
                         exported,
                         doc,
                         name,
                         typ,
                     }| {
                        Ok(Declaration {
                            span,
                            exported,
                            doc,
                            name: map_name(name)?,
                            typ: typ.map_global_names(&mut map_name)?,
                        })
                    },
                )
                .collect::<Result<Vec<_>, _>>()?,
            definitions: self
                .definitions
                .into_iter()
                .map(|Definition { span, name, body }| {
                    Ok(Definition {
                        span,
                        name: map_name(name)?,
                        body: match body {
                            DefinitionBody::Par(expr) => DefinitionBody::Par(Arc::new(
                                Arc::unwrap_or_clone(expr).map_global_names(&mut map_name)?,
                            )),
                            DefinitionBody::External(span) => DefinitionBody::External(span),
                        },
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    pub fn type_check(&self) -> (CheckedModule<S>, Vec<TypeError<S>>)
    where
        S: Eq + std::hash::Hash,
    {
        let mut errors: IndexSet<TypeError<S>> = IndexSet::new();

        let type_defs = match TypeDefs::new_with_validation(
            self.type_defs
                .iter()
                .map(|d| (&d.span, &d.name, &d.params, &d.typ)),
        ) {
            Ok(td) => td,
            Err(e) => {
                errors.insert(e);
                TypeDefs::default()
            }
        };

        let mut unchecked_definitions = IndexMap::new();
        for Definition { span, name, body } in &self.definitions {
            if let Some((span1, _)) =
                unchecked_definitions.insert(name.clone(), (span.clone(), body.clone()))
            {
                errors.insert(TypeError::NameAlreadyDefined(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let mut declarations = IndexMap::new();
        for Declaration {
            span, name, typ, ..
        } in &self.declarations
        {
            if !unchecked_definitions.contains_key(name) {
                errors.insert(TypeError::DeclaredButNotDefined(span.clone(), name.clone()));
            }
            if let Some((span1, _)) = declarations.insert(name.clone(), (span.clone(), typ.clone()))
            {
                errors.insert(TypeError::NameAlreadyDeclared(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let names_to_check = unchecked_definitions
            .iter()
            .map(|(name, (span, _))| (span.clone(), name.clone()))
            .collect::<Vec<_>>();

        let mut context = Context::new(type_defs, declarations, unchecked_definitions);
        for (span, name) in names_to_check {
            context.check_definition(&span, &name, &mut |e| {
                errors.insert(e);
            });
        }

        (
            CheckedModule {
                type_defs: context.get_type_defs().clone(),
                declarations: context
                    .get_declarations()
                    .into_iter()
                    .map(|(name, (span, typ))| {
                        let source_declaration = self.declarations.iter().find(|declaration| {
                            declaration.name == name && declaration.span == span
                        });
                        let doc =
                            source_declaration.and_then(|declaration| declaration.doc.clone());
                        (
                            name.clone(),
                            Declaration {
                                span,
                                exported: source_declaration
                                    .is_some_and(|declaration| declaration.exported),
                                doc,
                                name,
                                typ,
                            },
                        )
                    })
                    .collect(),
                definitions: context
                    .get_checked_definitions()
                    .into_iter()
                    .map(|(name, (span, body, typ))| {
                        (name.clone(), (Definition { span, name, body }, typ))
                    })
                    .collect(),
            },
            errors.into_iter().collect(),
        )
    }
}

impl<Expr, S> Default for Module<Expr, S> {
    fn default() -> Self {
        Self {
            type_defs: Vec::new(),
            declarations: Vec::new(),
            definitions: Vec::new(),
        }
    }
}

impl<Expr, S: Clone + Eq + std::hash::Hash> Module<Expr, S> {
    pub fn docs(&self) -> Docs<S> {
        Docs {
            types: self
                .type_defs
                .iter()
                .filter_map(|type_def| type_def.doc.clone().map(|doc| (type_def.name.clone(), doc)))
                .collect(),
            declarations: self
                .declarations
                .iter()
                .filter_map(|declaration| {
                    declaration
                        .doc
                        .clone()
                        .map(|doc| (declaration.name.clone(), doc))
                })
                .collect(),
        }
    }
}

#[derive(Clone)]
pub struct HoverIndex<S> {
    files: HashMap<FileName, FileHovers<S>>,
}

#[derive(Clone)]
struct FileHovers<S> {
    pairs: Vec<((Point, Point), HoverInfo<S>)>,
}

impl<S> Default for FileHovers<S> {
    fn default() -> Self {
        Self { pairs: Vec::new() }
    }
}

impl<S: Clone + Eq + std::hash::Hash + std::fmt::Display> HoverIndex<S> {
    pub fn new(
        program: &CheckedModule<S>,
        docs: &Docs<S>,
        import_spans: &HashMap<FileName, Vec<(Span, S)>>,
        mut is_type_visible: impl FnMut(&FileName, &S, &GlobalName<S>) -> bool,
        mut is_dec_visible: impl FnMut(&FileName, &S, &GlobalName<S>) -> bool,
    ) -> Self
    where
        S: Ord,
    {
        let mut files = HashMap::<_, FileHovers<S>>::new();

        for (name, (span, params, typ)) in program.type_defs.globals.iter() {
            let Some(file) = span.file() else { continue };
            let file_hovers = files.entry(file).or_default();
            file_hovers.push(
                name.span(),
                HoverInfo::type_definition(
                    name.clone(),
                    params.clone(),
                    typ.clone(),
                    docs.type_doc(name).cloned(),
                    span.clone(),
                ),
            );
            typ.types_at_spans(&program.type_defs, docs, &mut |span, name_info| {
                file_hovers.push(span, name_info)
            });
        }

        for (name, declaration) in &program.declarations {
            let Some(file) = declaration.span.file() else {
                continue;
            };
            let file_hovers = files.entry(file).or_default();
            let def_span = (program.definitions.get(name))
                .map(|(def, _typ)| def.span.clone())
                .unwrap_or_default();
            file_hovers.push(
                name.span(),
                HoverInfo::declaration(
                    name.clone(),
                    declaration.typ.clone(),
                    docs.declaration_doc(name).cloned(),
                    def_span,
                    declaration.span.clone(),
                ),
            );
            declaration
                .typ
                .types_at_spans(&program.type_defs, docs, &mut |span, name_info| {
                    file_hovers.push(span, name_info)
                });
        }

        for (name, (definition, typ)) in &program.definitions {
            let Some(file) = definition.span.file() else {
                continue;
            };
            let file_hovers = files.entry(file).or_default();
            let decl_span = (program.declarations.get(name))
                .map(|decl| decl.span.clone())
                .unwrap_or_default();
            file_hovers.push(
                name.span(),
                HoverInfo::declaration(
                    name.clone(),
                    typ.clone(),
                    docs.declaration_doc(name).cloned(),
                    definition.span.clone(),
                    decl_span,
                ),
            );
            if let DefinitionBody::Par(expr) = &definition.body {
                expr.types_at_spans(program, docs, &mut |span, name_info| {
                    file_hovers.push(span, name_info)
                });
            }
        }

        // Add hover info for import declarations
        for (file, spans) in import_spans {
            let file_hovers = files.entry(file.clone()).or_default();
            for (span, module) in spans {
                let mut types = Vec::new();
                for (name, (_, params, typ)) in program.type_defs.globals.iter() {
                    if &name.module == module && is_type_visible(file, module, name) {
                        types.push((name.clone(), params.clone(), typ.clone()));
                    }
                }
                types.sort_by(|(a, _, _), (b, _, _)| a.primary.cmp(&b.primary));

                let mut declarations = Vec::new();
                // Collect from declarations
                for (name, decl) in &program.declarations {
                    if &name.module == module && is_dec_visible(file, module, name) {
                        declarations.push((name.clone(), decl.typ.clone()));
                    }
                }
                // Collect from definitions that don't have a separate declaration
                for (name, (_def, typ)) in &program.definitions {
                    if &name.module == module
                        && !program.declarations.contains_key(name)
                        && is_dec_visible(file, module, name)
                    {
                        declarations.push((name.clone(), typ.clone()));
                    }
                }
                declarations.sort_by(|(a, _), (b, _)| a.primary.cmp(&b.primary));

                if types.is_empty() && declarations.is_empty() {
                    continue;
                }

                file_hovers.push(
                    span.clone(),
                    HoverInfo::module(module.clone(), types, declarations),
                );
            }
        }

        for file_hovers in files.values_mut() {
            file_hovers.sort_and_dedup();
        }

        Self { files }
    }

    pub fn query(&self, file: &FileName, row: u32, column: u32) -> Option<HoverInfo<S>> {
        self.files.get(file)?.query(row, column)
    }
}

impl<S: Clone> FileHovers<S> {
    fn push(&mut self, span: Span, name_info: HoverInfo<S>) {
        if let Some(points) = span.points() {
            self.pairs.push((points, name_info))
        }
    }
    fn sort_and_dedup(&mut self) {
        self.pairs.sort_by_key(|((start, _), _)| start.offset);
        self.pairs.dedup_by_key(|((start, _), _)| start.offset);
    }
    fn query(&self, row: u32, column: u32) -> Option<HoverInfo<S>> {
        let sorted_pairs = &self.pairs;
        if sorted_pairs.is_empty() {
            return None;
        }

        // find index with the greatest start that is <= than (row, column)
        let (mut lo, mut hi) = (0, sorted_pairs.len());
        while lo + 1 < hi {
            let mi = (lo + hi) / 2;
            let ((mp, _), _) = sorted_pairs[mi];
            if mp.row < row || (mp.row == row && mp.column <= column) {
                lo = mi;
            } else {
                hi = mi;
            }
        }

        let ((start, end), typ) = &sorted_pairs[lo];

        // check if queried (row, column) is in the found span
        if row < start.row || (row == start.row && column < start.column) {
            return None;
        }
        if end.row < row || (end.row == row && end.column < column) {
            return None;
        }

        // found a good span
        Some(typ.clone())
    }
}
