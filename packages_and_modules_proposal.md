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

# Bullet-point summary

- **Package = project**
  Every Par program or library is a *package* with this layout:
  `Par.toml`, `src/`, `dependencies/`, `external/`.

- **Par.toml**
  Defines the package (`name`) and its dependencies:
  ```
  [package]
  name = "myproject"

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
  If a module exports a `type` and/or `dec` with the same name as the module, those are usable directly under that module name when imported:
  ```
  import data/Post
  // gives: Post (type), Post (dec), and Post.FetchAllFromDB
  ```
  Other exported names are accessed as `Module.Name`.

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
  Items are **module-private by default.**
  `export type` / `export dec` exports an item inside its package.
  `export module Name` additionally exports the module from its package.
  Therefore, only **exported items of exported modules** are exported from the package:
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
  Exporting an item from a non-exported module makes it exported only inside its package, not from the package.

- **Multi-file modules**
  `Module.par`, `Module.foo.par`, `Module.bar.par` in the same directory form one module; all start with the same `module Module` declaration and share a single namespace.

- **Dependencies & Rust interop**
  - `dependencies/` is managed by the tool and stores fetched remote dependencies in a project-local tree.
  - Fetched dependencies are laid out by their normalized remote source, for example `dependencies/github.com/author/par-web/...`.
  - Only the root package has a managed `dependencies/` directory; transitive dependencies are also materialized there.
  - Local dependencies are recognized by path syntax and are referenced directly from disk, for example `"./something"`, `"../something"`, `"~/something"`, or `"$PAR_PACKAGES/something"`.
  - Package identity comes from the normalized dependency source, not from metadata inside `Par.toml`.
  - The import alias from `[dependencies]` is only a local name used in imports; it does not determine storage layout or package identity.
  - `external/` holds an optional Rust crate compiled together with the Par runtime to implement external definitions.
  - Rust external integration is expected to land in a second iteration, not necessarily the first implementation of packages/modules.

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
[package]
name = "projectName"
```

And then a **list of dependencies:**

```
[dependencies]
libraryName = "sourcecontrol.com/author/libraryName"
localLibrary = "./localLibrary"
```

The name assigned to the dependency in the `[dependencies]` list does not have to match the name specified in the dependency's `Par.toml` file. Instead, **the assigned name will be used inside the package for referring to the dependency.**

Both remote dependencies (URLs) and local dependencies (paths starting with `.`, `~`, or `$`, such as `"./localLibrary"`, `"../shared/localLibrary"`, `"~/par/localLibrary"`, or `"$PAR_LIBS/localLibrary"`) are supported.

The assigned dependency name is only a local alias used in imports such as `@libraryName/...`. It does **not** determine where the dependency is stored on disk, and it does **not** redefine the dependency's identity.

Package identity is determined by the normalized source through which the package is loaded:
- a local package is identified by its canonical absolute path
- a remote package is identified by its normalized remote source

The root package is therefore identified by its canonical local path, not by a self-declared URL in `Par.toml`.

> This proposal doesn't address versioning, we'll address it in a future version. Go managed to go years without versioning, we can go some time too. Of course, they eventually realized it was a mistake at scale, but it worked well at the beginning. And we are at the beginning!

## `src/`

Here, the Par source code of the package lives, organized into _directories_. These directories contain _modules_. I'll explain modules in depth right after finishing explaining packages.

## `dependencies/`

This directory is not to be managed by the programmer, but the package manager instead. Par will automatically download remote dependencies here, and their dependencies, transitively.

The dependencies downloaded here will _not_ create their own `dependencies/` directories. Instead, if dependencies share another dependency, it will be downloaded once into this top-level `dependencies/` directory.

Each fetched dependency will be stored according to its normalized remote source. For example:

`github.com/author/par-web` -> `dependencies/github.com/author/par-web/...`

This makes the dependency tree easy to browse and keeps remote package identity tied to the package source.

Local path dependencies (for example `"./mypackage"`, `"../mypackage"`, `"~/par/mypackage"`, or `"$PAR_LIBS/mypackage"`) are not fetched into `dependencies/`. They remain in place, are referenced from their local path, and are identified by their canonical absolute path.

Only the root package owns a managed `dependencies/` directory. Transitive dependencies are materialized there as well, so package resolution stays project-local.

`dependencies/` should generally be treated as generated state rather than handwritten source and should usually be ignored by version control.

Once versioning exists, the `dependencies/` layout can be extended to include version selection metadata or version-qualified directories if we decide to allow multiple versions to coexist.

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

export type Post = box choice {
  .title => String,
  .content => String,
}

export dec Post : [String, String] Post

export dec FetchAllFromDB : [!] List<Post>
```

The `Post` module specifies a `Post` type, as well as a `Post` declaration, a constructor.

**A type and/or dec with the same name as the module is special. If exported, it becomes available under the module name to the importer.**

Other exported types and declarations are usable as `Post.*`. Non-exported items stay available only inside the module.

So inside the `Posts` handler:

```
module Posts

import data/Post

// we've imported the exported items:
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
[package]
name = "postify"

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

Imports are still required: a module must explicitly import any other module it uses.

However, **items are available only inside their own module by default.** Merely placing two modules in the same package does not make their contents mutually visible.

To make an item visible in other modules of the same package, mark it `export`:

```
module Post

export type Post = box choice {
  .title => String,
  .content => String,
}

export dec Post : [String, String] Post
export dec FetchAllFromDB : [!] List<Post>

dec NormalizeTitle : [String] String
```

In this example:

- `Post`, `Post`, and `Post.FetchAllFromDB` are visible in other modules of the same package.
- `NormalizeTitle` is available only inside the `Post` module.

To make API surface exported from the package:
1. `export module Name`
2. `export` the `type`/`dec` items of that module that should be exported from the package.

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

Further notes:

- Exported `type` entries include their full type definitions (no opaque global types).
- Exported `dec` entries expose only signatures.
- `def` bodies are never exported directly; they are package-internal implementations.
- `export` is valid on `type` and `dec`, but not on `def`.

The module-level and item-level markings then interact like this:

- `export module` + `export item` = exported from the package
- `export module` + non-exported item = available only inside the module
- non-exported module + `export item` = exported only inside the package
- non-exported module + non-exported item = available only inside the module

For a program that's not a library, nothing needs to be `export module`, but cross-module shared items still need `export` on the item itself.

## Splitting a module into multiple files

Sometimes one file may not be enough for a module, and so in addition to `Module.par`, all files within the same directory that have this form:

```
Module.*.par
```

will be considered a part of the same module. All files of the module must start with the same `module Module` declaration.

All top-level definitions across these files share one module namespace. But each file has its own import section, which only applies within that file.

# Restrictions

**Cyclic imports between modules are allowed.**

**No cyclic dependencies between packages are allowed.**

**Cyclic usages of items are not allowed.** For example:
- A cycle in type definitions is not allowed.
- A cycle in defs referring to themselves is not allowed.

Recursion in Par is done explicitly with the existing mechanisms:
- `recursive` / `iterative` with `self`
- `begin` / `loop`
