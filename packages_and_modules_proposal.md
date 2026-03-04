Currently, Par only supports single-file programs. Splitting programs into multiple files and managing projects with dependencies is crucial, and this proposal outlines one way this could work in Par.

My priorities for the design:
- Simple to understand
- Scalable to projects of any size
- Easy to find what you're looking for in a codebase
- Easy to add and manage dependencies
- Seamless support for Rust interop
- Fits well with the pattern we're seeing in `Builtin.par`:
  - `String` (main module type),
  - `String.Builder` (associated type in a module),
  - `String.Equals` (a `dec` in a module).

Here is the proposal. I'm curious to hear your thoughts and criticisms :)

# First implementation scope (iteration 1)

This document describes the target design, but the first implementation intentionally ships only a subset:

- Modules within a single package.
- Package root + `src/` module graph.
- Default builtin package dependencies (`@core` and `@basic`, with target-specific availability).

Not in iteration 1:

- Custom dependencies from `Par.toml`.
- Exports (`export module`, `export type`, `export dec`).
- Rust `external/` package linking across dependencies.

So iteration 1 is: package + modules + builtin packages as implicit dependencies.

# Bullet-point summary

- **Package = project**
  Every Par program or library is a *package* with this layout:
  `Par.toml`, `src/`, `dependencies/`, `external/`.

- **Par.toml**
  Defines the package (`name`, `url`) and its dependencies:
  ```
  [dependencies]
  web = "github.com/author/par-web"
  mypackage = "./mypackage"
  ```
  The left-hand name (`web`, `mypackage`) is what you use in imports.

- **Modules under `src/`**
  Arbitrary directory structure; each module is usually one file named after it, e.g. `handlers/api/Posts.par` starts with:
  ```
  module Posts
  ```
  Module file name and module declaration must match (case-insensitive).

- **Imports are absolute and path-based**
  From the `src/` root, no relative module paths:
  ```
  import data/Post
  import handlers/api/Posts
  ```
  Imports are required: inside a package, a module is only visible in another module if imported there.
  Paths in imports always use `/`.
  Importing from dependencies uses `@name`:
  ```
  import @web/http/Server
  import @mypackage/FancyModule
  ```

- **Module-name match = main module type/dec**
  If a module defines a `type` and/or `dec` with the same name as the module, those are usable directly under that module name when imported:
  ```
  import data/Post
  // gives: Post (type), Post (dec), and Post.FetchAllFromDB
  ```
  Other names are accessed as `Module.Name`.

- **Aliasing imports**
  To avoid clashes:
  ```
  import @dep1/blah/Data as Data1
  import @dep2/bleh/Data as Data2
  ```

- **Grouped imports**
  You can group imports like this:
  ```
  import {
    @dep1/blah/Data as Data1
    @dep2/bleh/Data as Data2
  }
  ```

- **Visibility & exports**
  Imports are required inside a package.
  To expose API to *other* packages, first `export module`, then export `type` and `dec` items:
  ```
  export module Iterator
  export type Iterator<a> = ...
  export dec Map : <a>[Iterator<a>] [type b] [box [a] b] Iterator<b>

  // or grouped:
  export {
    type Iterator<a> = ...
    dec Map : <a>[Iterator<a>] [type b] [box [a] b] Iterator<b>
  }
  ```
  `export` is valid on `type` and `dec`, but not on `def`.
  Exported items are only allowed in exported modules.

- **Multi-file modules**
  `Module.par`, `Module.foo.par`, `Module.bar.par` in the same directory form one module; all start with the same `module Module` declaration and share a single namespace.

- **Dependencies & Rust interop**
  - `dependencies/` is managed by the tool and stores fetched dependencies (with URL-encoded suffixes; versions later).
  - Local dependencies (`"./something"` and `"../something"`) are supported for nested packages and are referenced directly from disk.
  - `external/` holds an optional Rust crate compiled together with the Par runtime to implement external definitions.
  - Rust external integration is expected to land in a second iteration, not necessarily the first implementation of packages/modules.

- **Builtins as default dependencies**
  Builtins are split into two default packages:
  - `@core`: pure builtins.
  - `@basic`: I/O builtins.
  In web builds, only `@core` is available by default. Importing `@basic/...` on web is an unresolved-package error.

- **CLI & playground become package-centric**
  CLI commands operate on a package (default: current directory, overridable via package path argument/option).
  Playground loads and compiles a whole package, while keeping one active file center-stage.

- **Cycles**
  Cyclic imports between modules are allowed.
  Cyclic dependencies between packages are disallowed.

# Packages

A Par project — be it a program or a library — is called a _package_. **Each package has this general file structure:**

```
projectName/
  Par.toml
  src/
  dependencies/
  external/
```

Let's take it one by one.

## `Par.toml`

**A configuration file that specifies the properties and dependencies of a package.** These are:

```
name = "projectName"
url = "github.com/faiface/projectName"
```

And then a **list of dependencies:**

```
[dependencies]
libraryName = "sourcecontrol.com/author/libraryName"
localLibrary = "./localLibrary"
```

The name assigned to the dependency in the `[dependencies]` list does not have to match the name specified in the dependency's `Par.toml` file. Instead, **the assigned name will be used inside the package for referring to the dependency.**

Both remote dependencies (URLs) and local dependencies (relative paths like `"./localLibrary"` and `"../shared/localLibrary"`) are supported.

> This proposal doesn't address versioning, we'll address it in a future version. Go managed to go years without versioning, we can go some time too. Of course, they eventually realized it was a mistake at scale, but it worked well at the beginning. And we are at the beginning!

## `src/`

Here, the Par source code of the package lives, organized into _directories_. These directories contain _modules_. I'll explain modules in depth right after finishing explaining packages.

## `dependencies/`

This directory is not to be managed by the programmer, but the package manager instead. Par will automatically download remote dependencies here, and their dependencies, transitively.

The dependencies downloaded here will _not_ create their own `dependencies/` directories. Instead, if dependencies share another dependency, it will be downloaded once into this top-level `dependencies/` directory.

Each fetched dependency will be stored in a directory named after the package name from its `Par.toml` file, plus a suffix created by some encoding of the package's URL, to avoid conflicts between packages with the same name.

For example: `dependencies/wonderfulgui_1ujr191u13i1i93139/...`.

This way, we avoid conflicts, and the programmer is also able to easily locate the source code of dependencies.

Local path dependencies (for example `"./mypackage"` or `"../mypackage"`) are not fetched into `dependencies/`. They remain in place and are referenced from their local path.

Once versioning exists, the `dependencies/` layout will likely include version in the directory encoding as well, so different versions can coexist if we decide to allow that.

> Iteration 1 note: custom dependencies are not yet enabled. The first version only has implicit default dependencies (`@core` and, on non-web, `@basic`).

## `external/`

If the package has a part of it implemented in Rust, here goes a crate implementing those definitions. This crate will then be compiled together with the general Par runtime to create a specialized binary capable of executing all external code of all dependencies of the program.

> Support for compiling all `external/` crates across dependencies is likely to land in a second iteration, not necessarily in the first implementation of packages/modules.

## Nested packages (local dependencies)

Packages may contain nested packages and reference them through `Par.toml` with relative paths:

```
myproject/
  Par.toml
  src/
    Main.par
  mypackage/
    Par.toml
    src/
      FancyModule.par
```

`myproject/Par.toml`:

```
[dependencies]
mypackage = "./mypackage"
```

`myproject/src/Main.par`:

```
module Main

import @mypackage/FancyModule
```

Visibility across this boundary is package visibility: the outer package can only import what the inner package exports.

# Modules

Now on to the contents of the `src/` directory!

**It can contain any directory structure.** For example, here's some ad-hoc web server, not really adhering to any methodology:

```
src/
  data/
  handlers/
    api/
  util/
```

And the directories are populated with _modules_! **A module is (usually) a single file with the same name as the module.** Let's put some modules in the above structure:

```
src/
  Main.par
  data/
    User.par
    Post.par
  handlers/
    api/
      Posts.par
      Profile.par
    Index.par
  util/
    DateTimeUtils.par
```

> Multi-file modules are described near the end.

**The start of a module file looks like this:**

```
module Posts
```

This is the `src/handlers/api/Posts.par` file. It defines a module called `Posts`.

**The file name and module declaration must match, case-insensitively.** So `Post.par` must declare `module Post`, and `handlers/api/Posts.par` must declare `module Posts`.

Now what if it wants to use another module in the same package? A `Posts` handler certainly wants to use the `Post` type:

```
module Posts

import data/Post
```

**The `import` statement specifies the whole path (from `src/`) of a module to import. Relative import paths are not supported.**
**Imports are required:** a module is only visible inside another module if imported there.
**Import paths always use forward slashes (`/`).**

You can group imports like this:

```
import {
  data/Post
  handlers/api/Profile
}
```

Now before I go on to say how `Posts` will use the `Post` module, let's take a look at the `Post` module.

```
module Post

type Post = box choice {
  .title => String,
  .content => String,
}

dec Post : [String, String] Post

dec FetchAllFromDB : [!] List<Post>
```

The `Post` module specifies a `Post` type, as well as a `Post` declaration, a constructor.

**A type and/or dec with the same name as the module is special. It becomes available under the module name to the importer.**

Other types and declarations are usable as `Post.*`.

So inside the `Posts` handler:

```
module Posts

import data/Post

// we've imported:
// - type Post
// - dec Post
// - dec Post.FetchAllFromDB
```

The handler will be able to use these for implementation.

And for example the `Main.par` module will start like this:

```
module Main

import handlers/Index
import handlers/api/Posts
import handlers/api/Profile
```

## Importing from dependencies

Let's say the little web server example above has some dependencies. Here's its `Par.toml` file:

```
name = "postify"
url = "github.com/faiface/postify"

[dependencies]
web = "github.com/afjwie/par-web"
mypackage = "./mypackage"
```

Importing from a dependency is the same as importing from within a package itself, the only difference is we prefix the path with `@<name>`, like so:

```
import @web/http/Server
import @mypackage/FancyModule
```

## Aliasing

Module name conflicts can occur, especially between dependencies, and so this will be supported:

```
import @dep1/blah/Data as Data1
import @dep2/bleh/Data as Data2
```

Simply rename a module via `as Alias`. The module-name-matching type and/or dec carry this alias too.

## Grouped imports

You can group imports like this:

```
import {
  @dep1/blah/Data as Data1
  @dep2/bleh/Data as Data2
  data/Post
}
```

## Visibility

Inside a package, definitions are still package-wide in the sense that modules can refer to each other, but each module must explicitly import any module it uses.

To make API surface visible outside a package:
1. `export module Name`
2. `export` the `type`/`dec` items of that module that should be public.

```
export module List

export type List<a> = recursive either {
  .end!,
  .item(a) self,
}

export dec Map : <a>[List<a>] [type b] [box [a] b] List<b>
export dec Filter : <a>[List<a>] [box [a] Bool] List<a>

// or grouped:
export {
  type List<a> = recursive either {
    .end!,
    .item(a) self,
  }

  dec Map : <a>[List<a>] [type b] [box [a] b] List<b>
  dec Filter : <a>[List<a>] [box [a] Bool] List<a>
}
```

What this means for cross-package visibility:

- Exported `type` entries include their full type definitions (no opaque global types).
- Exported `dec` entries expose only signatures.
- `def` bodies are never exported directly; they are package-internal implementations.
- `export` is valid on `type` and `dec`, but not on `def`.

Only exported modules may contain exported items.

For a program that's not a library, nothing really needs to be exported.

> Iteration 1 note: exports are not implemented yet.

## Splitting a module into multiple files

Sometimes one file may not be enough for a module, and so in addition to `Module.par`, all files within the same directory that have this form:

```
Module.*.par
```

will be considered a part of the same module. All files of the module must start with the same `module Module` declaration.

All top-level definitions across these files share one module namespace. But each file has its own import section, which only applies within that file.
Imports from one `Module.*.par` file do not become visible in sibling files.

# CLI and playground

The CLI should be package-centric.

All commands should accept an optional package path (default is current directory):

- `par-lang check [--package <path>]`
- `par-lang test [--package <path>] [--filter ...]`
- `par-lang run [--package <path>] [target]`
- `par-lang playground [--package <path>]`

`run` target resolution:

- No `target`: run `Main.Main`.
- `target = path/to/Module`: run `path/to/Module.Main`.
- `target = path/to/Module.Def`: run `path/to/Module.Def`.

The target module path uses `/`, and the definition selector uses `.Def`.

Playground should also operate on a whole package:

- One file remains center-stage (active editor).
- Switching between package files is supported (for example through a branching menu).
- Compile/typecheck spans the whole package, not only the active file.
- `Run` menu is structured as:
  - `Packages` -> dependent packages -> their module trees.
  - `Modules` -> current package module tree.
  - Direct entries for definitions in the currently opened module.

# Builtin packages

Builtins become default dependencies:

- `@core`: pure builtins.
- `@basic`: I/O builtins.

Target behavior:

- Native/CLI: both `@core` and `@basic` are available by default.
- Web/playground-web: only `@core` is available by default.
- Importing `@basic/...` on web produces a regular unresolved package/module error (no special fallback package in iteration 1).

From the start, imports are explicit. There is no implicit prelude module import.

# Examples package

Examples will become a single `examples` package where each example is represented as a module.

# Restrictions

**Cyclic imports between modules are allowed.**

**No cyclic dependencies between packages are allowed.**

**Cyclic usages of items are not allowed.** For example:
- A cycle in type definitions is not allowed.
- A cycle in defs referring to themselves is not allowed.

Recursion in Par is done explicitly with the existing mechanisms:
- `recursive` / `iterative` with `self`
- `begin` / `loop`
