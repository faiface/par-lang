# Packages & Modules Implementation Plan (Iteration 1)

This plan translates the proposal into an implementation sequence for the current codebase.

## 1. Scope and constraints

### In scope (iteration 1)
- Multi-file package loading from `Par.toml` + `src/`.
- Modules + imports (`import`, grouped imports, aliasing).
- Name resolution pipeline in 3 stages:
  - `Unresolved`
  - `Resolved`
  - `Universal`
- Default builtin dependencies:
  - `@core` (always)
  - `@basic` (non-web only)
- Package-centric CLI and playground behavior.

### Out of scope (iteration 1)
- Custom dependencies from `Par.toml`.
- Exports (`export module`, `export type`, `export dec`).
- Dependency fetching and lock/versioning.
- Cross-package Rust `external/` linking.

## 2. Ground rules from design discussion

- No numeric IDs for module/package resolution; use structured string paths.
- Directory path segments are canonicalized to lowercase.
- Module name source of truth is the `module X` declaration (not file name).
- File name must match module name case-insensitively.
- Resolved/Universal paths are structured (no slash-joined strings).
- In multi-file modules, imports are file-local (no sharing between `Module.*.par` siblings).

## 3. Current code hotspots (to refactor)

### Core language/types
- `crates/par-core/src/frontend_impl/language.rs`
  - `GlobalName` currently `{ module: Option<String>, primary: String }`.
  - Equality/hash/display currently contain `module == primary` special behavior.
- `crates/par-core/src/frontend_impl/types/core.rs`
  - `Type::Name` / `Type::DualName` contain `GlobalName`.
- `crates/par-core/src/frontend_impl/process.rs`
  - `Expression::Global` contains `GlobalName`.
- `crates/par-core/src/frontend_impl/program.rs`
  - `TypeDef`, `Declaration`, `Definition`, `CheckedModule` keyed by `GlobalName`.

### Parser/lexer
- `crates/par-core/src/frontend_impl/lexer.rs`
  - currently no `module/import/as/export` keywords.
- `crates/par-core/src/frontend_impl/parse.rs`
  - top-level parser currently accepts only `type/dec/def` items.
  - no file header/import parsing.

### App pipeline (single-file assumptions)
- `src/main.rs`, `src/test_runner.rs`, `src/playground.rs`, `src/language_server/instance.rs`
  - parse/lower one source file and then call `import_builtins(...)`.

### Builtins
- `crates/par-builtin/src/builtin.rs`
  - builtins injected directly into one module via `module.import(...)`.

## 4. Target data model

## 4.1 Stage marker types
- Add marker types in frontend core:
  - `Unresolved`
  - `Resolved`
  - `Universal`

## 4.2 Path representations
- `Resolved` module reference should contain structured local package path:
  - canonical directory segments (lowercase),
  - module name (from declaration).
- `Universal` should extend resolved path with package identity:
  - `enum PackageRef { Local, Core, Basic }` for iteration 1.

## 4.3 Generic global names
- Convert `GlobalName` to `GlobalName<S>`.
- Remove old `module == primary` equality shortcut in favor of explicit stage semantics.

## 4.4 Generic propagation
All AST/IR types containing `GlobalName` must carry stage parameter(s). At minimum:
- `Type<S>` (because `Type::Name` and `Type::DualName` reference globals).
- High-level AST expression/process/pattern/condition types where global names or type annotations appear.
- Lowered process expressions/commands/types that include globals.
- Program containers:
  - `TypeDef<S>`
  - `Declaration<S>`
  - `Definition<Expr, S>` (or keep `Definition<Expr>` with `Expr` carrying `S`)
  - `Module<Expr, S>`
  - `CheckedModule` fixed to `Universal` at end of resolution.

Use type aliases aggressively to keep signatures readable.

## 5. Mapping APIs (required for staged flattening)

Introduce fallible AST mappers for stage transitions.

- On relevant types, add methods conceptually like:
  - `map_global_names`
  - `map_types`
  - `map_module_paths` (for convenience wrappers)

These should construct new values (no in-place mutation assumptions), so each pass can safely transform:
- `Unresolved -> Resolved`
- `Resolved -> Universal`

## 6. Parsing and file-level AST

## 6.1 Lexer updates
Add tokens/keywords:
- `module`
- `import`
- `as`
- (optional now) `export` token reserved for future

## 6.2 Parse file headers
Extend parser with file-level shape:
1. `module ModuleName`
2. zero or more imports (`import ...` / grouped form)
3. top-level `type/dec/def` items

Introduce parsed-file metadata structures, for example:
- `ModuleDecl`
- `ImportDecl` (single/grouped, alias, dependency prefix)
- `ParsedSourceFile`

Keep export parsing deferred (iteration 1). If parsed, return a clear "not implemented in iteration 1" error.

## 6.3 Import grammar (iteration 1)
Support:
- `import path/to/Module`
- `import @core/path/to/Module`
- `import @basic/path/to/Module`
- `import x/y/Module as Alias`
- grouped import block

## 7. Package loader and source collection

Add package-loading subsystem in app crate (filesystem-oriented), with a clean interface reusable by CLI/playground/LSP:

Responsibilities:
- Discover package root (`Par.toml`) from explicit path or cwd.
- Enumerate `src/**/*.par`.
- Group multi-file modules (`Module.par`, `Module.*.par`).
- Parse each source file and validate:
  - module declaration exists,
  - file/module case-insensitive match.
- Build per-file import maps and module membership.

Note: custom dependency loading is disabled in iteration 1.

## 8. Resolution pass 1: Unresolved -> Resolved

For each source file:
- Build import alias map: `Alias -> target module`.
- Rewrite global references according to agreed rules:

Given `Name` or `Module.Name` in source:
1. `Module.Name`
   - `Module` must be an imported alias in this file.
   - rewrite to imported module path + `Name`.
2. `Name` (unqualified)
   - if `Name` equals an imported alias, rewrite to that module's module-main symbol (`Module.Module`).
   - else rewrite to `current_module.Name`.

Reject unknown qualifiers/aliases with precise file+span diagnostics.

## 9. Resolution pass 2: Resolved -> Universal

Qualify each resolved module path with package ref:
- local files -> `PackageRef::Local`
- builtin imports -> `PackageRef::Core` / `PackageRef::Basic`

In iteration 1, any other `@dep/...` import is an unresolved dependency error.

## 10. Builtin packaging changes

Replace `import_builtins(&mut Module<...>)` with package/module providers.

Target API (conceptual):
- `builtin_core_modules()`
- `builtin_basic_modules()` (cfg non-wasm)

Compiler context should treat these as implicit default dependency packages.

Web target:
- only register `@core`.
- importing `@basic/...` should fail naturally as unknown package/module.

## 11. Build pipeline integration

Create package-oriented build entry points used by all fronts:
- parse+lower+resolve whole package
- typecheck resulting unified `Module<..., Universal>`
- runtime compile unchanged from checked module onward

Refactor callers:
- `src/main.rs` (`run`, `check`, `test`)
- `src/test_runner.rs`
- `src/playground.rs`
- `src/language_server/instance.rs`

Remove direct `import_builtins(...)` from single-file flows.

## 12. CLI behavior changes

Add optional package path argument/option to commands.

`run` target semantics:
- no target -> `Main.Main`
- `path/to/Module` -> `path/to/Module.Main`
- `path/to/Module.Def` -> exact definition

Use `/` for module path and `.` for definition selector.

## 13. Playground behavior changes (iteration 1)

- Load whole package context.
- Keep one active file center-stage.
- Allow file switching (branching menu is acceptable).
- Compile/typecheck package-wide.
- Update Run menu to include:
  - `Packages` tree
  - `Modules` tree (current package)
  - definitions in active module

(Full multi-tab virtual package editing can be incremental after baseline works.)

## 14. Migration tasks

- Convert examples into an `examples` package (one example per module).
- Update examples/tests/docs code to explicit imports (no implicit prelude).
- Ensure builtin references resolve via `@core` / `@basic` imports.

## 15. Validation and testing strategy

## 15.1 Unit tests
- Lexer tests for new keywords.
- Parser tests for module headers/import forms/grouped imports.
- Resolver tests:
  - alias resolution,
  - unqualified fallback behavior,
  - alias-as-main (`Name -> Module.Module`) behavior,
  - errors on unknown alias/unknown package.

## 15.2 Integration tests
- Compile/run a small multi-module local package.
- Multi-file module import isolation test.
- CLI run target resolution tests (`Main.Main`, module-only, module+def).
- Web target test: `@basic` import fails with clear message.

## 15.3 Regression checks
- Existing single-file flow can be temporarily shimmed as a one-module package for transition.
- Ensure type checker/runtime behavior unchanged after names are `Universal`.

## 16. Implementation order (recommended)

1. Introduce stage markers + generic `GlobalName<S>` + aliases.
2. Propagate generics to types/AST/program containers; keep old behavior compiling.
3. Add mapper APIs (`map_global_names` family).
4. Extend lexer/parser with module/import file structure.
5. Add package loader (filesystem) and module grouping.
6. Implement Unresolved -> Resolved pass.
7. Implement Resolved -> Universal pass.
8. Add builtin package providers (`@core`, `@basic`).
9. Wire package build pipeline into CLI/test/playground/LSP.
10. Migrate examples/tests to explicit imports and package layout.
11. Add/adjust tests and clean up transitional shims.

## 17. Risks and mitigation

- **Risk:** Generic propagation causes large refactor blast radius.
  - **Mitigation:** use staged aliases + compile after each subsystem conversion.
- **Risk:** Resolver semantics mismatch expectations in edge cases.
  - **Mitigation:** lock rules with focused resolver unit tests before integration.
- **Risk:** Playground/LSP package integration creates temporary UX regressions.
  - **Mitigation:** ship minimal package compile first, UI enhancements second.

## 18. Done criteria for iteration 1

- Multi-file local package compiles/runs with explicit imports.
- Name resolution is staged (`Unresolved` -> `Resolved` -> `Universal`) and used by compiler pipeline.
- CLI commands are package-centric and obey `run` target semantics.
- Builtins are accessible as default dependency packages (`@core`, `@basic` non-web).
- Web build exposes only `@core` and errors on `@basic` imports.
- Examples are organized as modules in an `examples` package.
