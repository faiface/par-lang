pub mod source {
    pub use crate::location::{FileName, Point, Span, Spanning};
}

pub mod frontend {
    use std::sync::Arc;

    use crate::location::FileName;
    use crate::frontend_impl::language::{CompileError, Context};
    use crate::frontend_impl::parse::parse_module;
    use crate::runtime_impl::{Compiled, RuntimeCompilerError};

    pub mod language {
        pub use crate::frontend_impl::language::*;
    }

    pub mod process {
        pub use crate::frontend_impl::process::*;
    }

    pub use crate::frontend_impl::parse::SyntaxError;
    pub use crate::frontend_impl::parse_bytes;
    pub use crate::frontend_impl::primitive::{ParString, Primitive};
    pub use crate::frontend_impl::program::{
        CheckedModule, Declaration, Definition, Module, ParseAndCompileError, TypeDef, TypeOnHover,
    };
    pub use crate::frontend_impl::set_miette_hook;
    pub use crate::frontend_impl::types::lattice::{intersect_types, union_types};
    pub use crate::frontend_impl::types::{Operation, PrimitiveType, Type, TypeDefs, TypeError};

    pub type HighLevelModule = Module<language::Expression>;
    pub type LowLevelModule = Module<Arc<process::Expression<()>>>;

    pub fn parse(source: &str, file: FileName) -> Result<HighLevelModule, SyntaxError> {
        parse_module(source, file)
    }

    pub fn lower(module: HighLevelModule) -> Result<LowLevelModule, CompileError> {
        let compiled_definitions = module
            .definitions
            .into_iter()
            .map(
                |Definition {
                     span,
                     name,
                     expression,
                 }| {
                    Context::new()
                        .compile_expression(&expression)
                        .map(|compiled| Definition {
                            span,
                            name,
                            expression: compiled.optimize().fix_captures().0.optimize_subject(None),
                        })
                },
            )
            .collect::<Result<_, _>>()?;

        Ok(Module {
            type_defs: module.type_defs,
            declarations: module.declarations,
            definitions: compiled_definitions,
        })
    }

    pub fn type_check(module: &LowLevelModule) -> Result<CheckedModule, TypeError> {
        module.type_check()
    }

    pub fn compile_runtime(
        module: &CheckedModule,
        max_interactions: u32,
    ) -> Result<Compiled, RuntimeCompilerError> {
        Compiled::compile_file(module, max_interactions)
    }
}

pub mod runtime {
    pub use crate::runtime_impl::{
        Compiled, Handle, Rewrites, RuntimeCompilerError, TypedHandle, TypedReadback, executor::start_and_instantiate
    };
}

pub mod execution {
    pub use crate::spawn::TokioSpawn;
}

pub mod testing {
    pub use crate::test_assertion::{AssertionResult, import_test_module, provide_test};
}
