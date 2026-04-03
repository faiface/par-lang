# Packages & Modules

So far, we've focused on the contents of one module file. That's a good place to start, but real Par
programs live in **packages,** and packages are made of **modules.**

The hierarchy looks like this:

- A **program** or a **library** is a **package.**
- A **package** contains **modules.**
- A **module** contains **type definitions, declarations, and definitions.**

Let's now look at how those pieces fit together.

## Packages

A Par package is a project directory with a `Par.toml` file and a `src/` directory.

For example:

```text
hello_par/
  Par.toml
  src/
    Main.par
```

This is exactly what `par new hello_par` creates.

The `Par.toml` file starts like this:

```toml
[package]
name = "hello_par"
```

This `name` is the package's **recommended name.** It is used by tooling such as generated
docs.

### Dependencies

Packages may depend on other packages through the `[dependencies]` section:

```toml
[package]
name = "postify"

[dependencies]
web = "github.com/author/par-web"
shared = "../shared"
```

Each dependency has the form:

```toml
alias = "reference"
```

The **alias** on the left is the name used in imports, such as `@web/...`.

The **reference** on the right may be one of two things:

- A **local path.** These are recognized by starting with `.`, `..`, `~`, or `$`.
- A **remote dependency source.** Anything else is treated as remote, for example
  `github.com/faiface/par-cancellable`.

Local paths may look like:

```toml
shared = "./shared"
common = "../common"
tools = "~/par/tools"
extras = "$PAR_PACKAGES/extras"
```

Remote dependencies currently have **no versioning.** Par just fetches the latest contents from the given source.

### Managing remote dependencies

Remote dependencies are managed by the CLI:

- `par add` reads `Par.toml` and fetches any missing remote dependencies into `dependencies/`.
- `par update` re-fetches all managed remote dependencies.
- `par add github.com/faiface/par-cancellable` both adds that dependency to `Par.toml` under its
  recommended name and fetches it.

The `dependencies/` directory is managed state. It is not where you write source code.

Transitive remote dependencies are fetched automatically. If the same remote package is reached
through multiple dependencies, Par handles that seamlessly and fetches it only once.

Local dependencies are **not** copied into `dependencies/`. They stay where they are on disk and are
referenced directly.

### Built-in packages

Every package automatically depends on two built-in packages:

- `@core` for core types and data structures such as `String`, `List`, `Map`, `Result`, and so on.
- `@basic` for simple I/O such as `Console`, `Os`, and `Http`.

These are **implicit dependencies,** but **not implicit imports.** Their modules are available to import from,
but their names are not automatically in scope inside your source files.

### Browsing packages with `par doc`

The `par doc` command is all about exploring packages:

- Outside any package, `par doc` shows the **built-in packages.**
- Inside a package, `par doc` shows the **current package together with its dependencies.**
- `par doc --remote github.com/faiface/par-cancellable` lets you inspect a **remote package**
  without manually adding it as a dependency.

## Modules

Modules live under `src/`, in any directory structure you like.

For example:

```text
src/
  Main.par
  data/
    Post.par
  handlers/
    api/
      Posts.par
```

Here:

- `src/Main.par` defines the `Main` module.
- `src/data/Post.par` defines the `Post` module with the path `data/Post`.
- `src/handlers/api/Posts.par` defines the `Posts` module with the path `handlers/api/Posts`.

Notice the split:

- A module has a **name** such as `Post`.
- A module also has a **path** such as `data/Post`.

The **name** is what appears in the file:

```par
module Post
```

The **path** is how the module is imported from elsewhere.
The file name and the module declaration must match, **case-insensitively.**

So:

- `Post.par` must declare `module Post`
- `post.par` may also declare `module Post`
- `handlers/api/Posts.par` must declare `module Posts`

The directories contribute to the module's **path**, not to its declared module name.

## Importing modules

Modules import other modules explicitly.

To import a module from the **same package,** use its absolute path from `src/`:

```par
import data/Post
import handlers/api/Posts
```

Relative module imports are **not** supported.

Import paths always use forward slashes:

```par
import util/DateTimeUtils
```

To import a module from a **dependency,** prefix the path with `@alias`:

```par
import @web/http/Server
import @shared/FancyModule
```

### Aliasing imports

If two modules would clash, rename them on import:

```par
import @dep1/blah/Data as Data1
import @dep2/bleh/Data as Data2
```

### Grouped imports

Multiple imports can be grouped:

```par
import {
  @basic/Console
  @core/List
  data/Post
}
```

Grouped imports are just syntax sugar over multiple `import` statements.

## Accessing names from imported modules

Once a module is imported, its exported items are accessed through the module name:

```par
import {
  @core/List
  @core/Nat
  @core/String
  data/Post
}

dec RenderPosts : String
def RenderPosts = Post.FetchAllFromDB(!)->List.Length->Nat.ToString
```

In general, imported names are accessed as:

```par
Module.Name
```

## Primary types and primary declarations

A module may export a **type** and/or a **declaration** with the same name as the module itself.

Those are special: they become available directly under the module name.

For example:

```par
module Post

export {
  type Post = box choice {
    .title => String,
    .content => String,
  }

  dec Post : [String, String] Post
  dec FetchAllFromDB : [!] List<Post>
}
```

Now another module can write:

```par
import data/Post
```

and gets access to:

- `Post` as a **type**
- `Post` as a **declaration**
- `Post.FetchAllFromDB` as another exported declaration from the module

This works smoothly with aliases too:

```par
import @core/List as L
```

After that:

- `L<a>` is the primary type of the module
- `L.Map`, `L.Filter`, and so on are other exported declarations

Having both a primary type `Post` and a primary declaration `Post` is perfectly fine,
because in Par, types and terms are always distinguishable by syntactic position.

## Visibility and exports

There are two related visibility questions:

1. Is the **module** visible outside its package?
2. Is a **type** or **declaration** visible outside its module?

### Exporting a module

Modules are visible **inside their own package** regardless of `export module`.

To make a module visible to **dependent packages,** mark it:

```par
export module List
```

### Exporting items

Types and declarations are **module-private by default.**

To make them visible outside the module, use `export`:

```par
export type Iterator<a> = ...
export dec Map : ...
```

Or grouped:

```par
export {
  type Iterator<a> = ...
  dec Map : ...
  dec Filter : ...
}
```

There is **no** `export def`.

Definitions are always the implementation side. If you want a value to be visible, export its
**declaration**.

### The three visibility levels

Putting the two layers together gives three effective visibility modes for items:

1. `export module` + exported item  
   The item is visible to dependent packages.
2. Non-exported module + exported item  
   The item is visible throughout its own package, but not outside it.
3. Any module + non-exported item  
   The item is visible only inside its own module.

### Visibility is checked through types too

Par also checks that visible API does not mention hidden types.

For example, if a declaration is visible throughout the package, or exported from the package,
then its type must not mention a less-visible type. In other words:

- a package-visible item may not mention a module-private type
- a public item may not mention a merely package-visible type

This prevents less-visible helper types from leaking into wider APIs.

## Cycles

In the previous section, we already saw that types and definitions may not use one another in a
cycle, and package dependencies follow the same spirit: cyclic dependencies between packages are
disallowed.

Modules are different, though: cyclic imports between modules of the same package are allowed.

Why is that useful, if definitions still may not form cycles? Because modules often need one
another's types in their signatures, and some definitions in one module may legitimately call
definitions in the other as long as no actual usage cycle is formed.

For example, the built-in `@core/List` module imports `@core/Nat` because `List.Length` returns a
`Nat`. At the same time, `@core/Nat` imports `@core/List` because `Nat.Range` returns a
`List<Nat>`.

So the modules import each other, but there is still no cyclic usage between the individual
definitions themselves.

## Multi-file modules

If one file becomes too large, a module may be split across several files in the same directory:

```text
src/
  Parser.par
  Parser.lexing.par
  Parser.errors.par
```

These all belong to the same module:

```par
module Parser
```

The rules are:

- `Parser.par` and every `Parser.*.par` in the same directory are parts of one module.
- Every part still declares `module Parser`.
- All parts share one top-level module namespace.
- Each file has its **own imports** that apply only within that file.
- All parts must agree on whether the module is marked `export module`.

## Running a definition

`par run` is specifically for definitions of type `!` — the unit type, comparable to `null` or an
empty tuple in other languages.

Other non-generic definitions can still be run in the playground, which generates an automatic UI
for interacting with them based on their type.

Now that modules have paths, the `par run` target syntax makes more sense:

- Targets have one of these forms:
  - `path/to/Module`
  - `path/to/Module.Def`
- The slash-separated part is always the **module path.**
- If the target ends right after the module path, `par run` looks for the module's `Main`
  definition.
- If the target ends with `.Def`, Par runs that specific definition from the module.

So:

- `par run` means `Main.Main`
- `par run Main` also means `Main.Main`
- `par run Main.Other` means the `Other` definition in the `Main` module
- `par run handlers/api/Posts` means `handlers/api/Posts.Main`
- `par run handlers/api/Posts.Program` means the `Program` definition in the `handlers/api/Posts` module

The same path-based rules are used by other commands such as `par test`.

That's the package/module system. With that in place, we can now return to the language itself.
