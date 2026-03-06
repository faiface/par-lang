use std::{collections::HashMap, future::Future, pin::Pin, sync::Arc};

use arcstr::ArcStr;
use indexmap::IndexMap;

use crate::location::{FileName, Point, Span, Spanning};

use super::{
    language::{CompileError, GlobalName, LocalName, Unresolved},
    parse::SyntaxError,
    process::{self, NameWithType},
    types::{Context, Type, TypeDefs, TypeError},
};

use crate::frontend::language::Expression;
use par_runtime::readback::Handle;

#[derive(Clone, Debug)]
pub struct Module<Expr, S> {
    pub type_defs: Vec<TypeDef<S>>,
    pub declarations: Vec<Declaration<S>>,
    pub definitions: Vec<Definition<Expr, S>>,
}

#[derive(Clone, Debug)]
pub struct ModuleDecl {
    pub span: Span,
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
    pub name: GlobalName<S>,
    pub params: Vec<LocalName>,
    pub typ: Type<S>,
}

#[derive(Clone, Debug)]
pub struct Declaration<S> {
    pub span: Span,
    pub name: GlobalName<S>,
    pub typ: Type<S>,
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

impl Definition<Arc<process::Expression<(), Unresolved>>, Unresolved> {
    //TODO: remove this
    pub fn external(
        name: &'static str,
        f: fn(Handle) -> Pin<Box<dyn Send + Future<Output = ()>>>,
    ) -> Self {
        Self {
            span: Default::default(),
            name: GlobalName::<Unresolved>::external(None, name),
            body: DefinitionBody::Par(Arc::new(process::Expression::External(f, ()))),
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

// TODO: remove this
// impl Module<Arc<process::Expression<(), Unresolved>>, Unresolved> {
//     pub fn parse_and_compile(source: &str, file: FileName) -> Result<Self, ParseAndCompileError> {
//         let parsed = parse_module(source, file)?;
//
//         let compiled_definitions = parsed
//             .definitions
//             .into_iter()
//             .map(
//                 |Definition {
//                      span,
//                      name,
//                      body,
//                  }| {
//                     language::Context::new()
//                         .compile_expression(&expression)
//                         .map(|compiled| Definition {
//                             span,
//                             name,
//                             expression: compiled.optimize().fix_captures().0.optimize_subject(None),
//                         })
//                 },
//             )
//             .collect::<Result<_, _>>()?;
//
//         Ok(Module {
//             type_defs: parsed.type_defs,
//             declarations: parsed.declarations,
//             definitions: compiled_definitions,
//         })
//     }
// }

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
                         name,
                         params,
                         typ,
                     }| {
                        Ok(TypeDef {
                            span,
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
                .map(|Declaration { span, name, typ }| {
                    Ok(Declaration {
                        span,
                        name: map_name(name)?,
                        typ: typ.map_global_names(&mut map_name)?,
                    })
                })
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

    pub fn type_check(&self) -> Result<CheckedModule<S>, TypeError<S>>
    where
        S: Eq + std::hash::Hash,
    {
        let type_defs = TypeDefs::new_with_validation(
            self.type_defs
                .iter()
                .map(|d| (&d.span, &d.name, &d.params, &d.typ)),
        )?;

        let mut unchecked_definitions = IndexMap::new();
        for Definition { span, name, body } in &self.definitions {
            if let Some((span1, _)) =
                unchecked_definitions.insert(name.clone(), (span.clone(), body.clone()))
            {
                return Err(TypeError::NameAlreadyDefined(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let mut declarations = IndexMap::new();
        for Declaration { span, name, typ } in &self.declarations {
            if !unchecked_definitions.contains_key(name) {
                return Err(TypeError::DeclaredButNotDefined(span.clone(), name.clone()));
            }
            if let Some((span1, _)) = declarations.insert(name.clone(), (span.clone(), typ.clone()))
            {
                return Err(TypeError::NameAlreadyDeclared(
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
            context.check_definition(&span, &name)?;
        }

        Ok(CheckedModule {
            type_defs: context.get_type_defs().clone(),
            declarations: context
                .get_declarations()
                .into_iter()
                .map(|(name, (span, typ))| (name.clone(), Declaration { span, name, typ }))
                .collect(),
            definitions: context
                .get_checked_definitions()
                .into_iter()
                .map(|(name, (span, body, typ))| {
                    (name.clone(), (Definition { span, name, body }, typ))
                })
                .collect(),
        })
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

#[derive(Clone)]
pub struct TypeOnHover<S> {
    files: HashMap<FileName, FileHovers<S>>,
}

#[derive(Clone)]
struct FileHovers<S> {
    pairs: Vec<((Point, Point), NameWithType<S>)>,
}

impl<S> Default for FileHovers<S> {
    fn default() -> Self {
        Self { pairs: Vec::new() }
    }
}

impl<S: Clone + Eq + std::hash::Hash + std::fmt::Display> TypeOnHover<S> {
    pub fn new(program: &CheckedModule<S>) -> Self {
        let mut files = HashMap::<_, FileHovers<S>>::new();

        for (name, (span, _, typ)) in program.type_defs.globals.iter() {
            let Some(file) = span.file() else { continue };
            let file_hovers = files.entry(file).or_default();
            file_hovers.push(name.span(), NameWithType::global(name.clone(), typ.clone()));
            typ.types_at_spans(&program.type_defs, &mut |span, name_info| {
                file_hovers.push(span, name_info)
            });
        }

        for (name, declaration) in &program.declarations {
            let Some(file) = declaration.span.file() else {
                continue;
            };
            let file_hovers = files.entry(file).or_default();
            let def_span = (program.definitions.get(name))
                .map(|(def, _typ)| def.name.span())
                .unwrap_or_default();
            file_hovers.push(
                name.span(),
                NameWithType {
                    name: Some(name.to_string()),
                    global_name: Some(name.clone()),
                    typ: declaration.typ.clone(),
                    def_span,
                    decl_span: Span::None,
                },
            );
            declaration
                .typ
                .types_at_spans(&program.type_defs, &mut |span, name_info| {
                    file_hovers.push(span, name_info)
                });
        }

        for (name, (definition, typ)) in &program.definitions {
            let Some(file) = definition.span.file() else {
                continue;
            };
            let file_hovers = files.entry(file).or_default();
            let decl_span = (program.declarations.get(name))
                .map(|decl| decl.name.span())
                .unwrap_or_default();
            file_hovers.push(
                name.span(),
                NameWithType {
                    name: Some(name.to_string()),
                    global_name: Some(name.clone()),
                    typ: typ.clone(),
                    def_span: Span::None,
                    decl_span,
                },
            );
            if let DefinitionBody::Par(expr) = &definition.body {
                expr.types_at_spans(program, &mut |span, name_info| {
                    file_hovers.push(span, name_info)
                });
            }
        }

        for file_hovers in files.values_mut() {
            file_hovers.sort_and_dedup();
        }

        Self { files }
    }

    pub fn query(&self, file: &FileName, row: u32, column: u32) -> Option<NameWithType<S>> {
        self.files.get(file)?.query(row, column)
    }
}

impl<S: Clone> FileHovers<S> {
    fn push(&mut self, span: Span, name_info: NameWithType<S>) {
        if let Some(points) = span.points() {
            self.pairs.push((points, name_info))
        }
    }
    fn sort_and_dedup(&mut self) {
        self.pairs.sort_by_key(|((start, _), _)| start.offset);
        self.pairs.dedup_by_key(|((start, _), _)| start.offset);
    }
    fn query(&self, row: u32, column: u32) -> Option<NameWithType<S>> {
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
