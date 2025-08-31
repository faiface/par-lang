# Error Handling

Error handling is everywhere: reading input, parsing, filesystem, networking. Languages
approach this differently (exceptions, checked results, monads, panic/abort). Par's
situation is unique thanks to its linear type system, so we wanted a style that
fits: convenient to write, flexible in use, and still precise enough for the type checker
to ensure everything is handled as it should.

The result is small sugar over the pre‑defined `Result` type that keeps semantics
transparent while making everyday code pleasant.

```par
type Result<e, a> = either {
  .err e,
  .ok a,
}
```

This chapter introduces lightweight sugar that makes `Result`-based error handling concise:
`try`, `catch`, and `throw`. The sugar does not change semantics — it desugars to `.case`
on `Result` — but it can make code dramatically clearer.

To make ideas concrete, examples use I/O from the `Os` module (where `Result`s are common),
but the constructs are general and work for any `Result`.

## Before the sugar: explicit case

Consider writing two lines to a file and then closing it. Without sugar we must
handle every `Result` explicitly:

```par
def Main: ! = chan exit {
  let console = Console.Open

  let path = Os.PathFromString("logs.txt")
  path.createOrAppendToFile.case {
    .err e => { console.print(e); console.close; exit! }
    .ok writer => {}
  }

  writer.writeString("[INFO] First new log\n").case {
    .err e => { console.print(e); console.close; exit! }
    .ok => {}
  }

  writer.writeString("[INFO] Second new log\n").case {
    .err e => { console.print(e); console.close; exit! }
    .ok => {}
  }

  writer.close(.ok!).case {
    .err e => { console.print(e); console.close; exit! }
    .ok! => {}
  }

  console.close
  exit!
}
```

This style is explicit but noisy. Let’s add the sugar!

## The sugar at a glance

- `try` (commands): `x.y(...).try` propagates `.err e => { throw e }`, otherwise continues.
- `try` (patterns): `let try x = E` binds on ok, `throw`s on err. Works in nested patterns and receives: `recv[try x]`.
- `catch` (statement): `catch e => { <handler process> }` introduces a handler scope for subsequent `throw`s.
- `catch` (expression): `catch e => H in E` evaluates `E` with a handler and returns a value (often a `Result`); `try`/`throw` must occur before any part of the final value is constructed.
- `throw`: jumps to the nearest matching `catch` (by label; see [Labels](#labels) below).

Rewriting the earlier example with sugar:

```par
def Main: ! = chan exit {
  let console = Console.Open

  catch e => {
    console.print(e)
    console.close
    exit!
  }

  let path = Os.PathFromString("logs.txt")
  let try writer = path.createOrAppendToFile
  writer.writeString("[INFO] First new log\n").try
  writer.writeString("[INFO] Second new log\n").try
  writer.close(.ok!).try

  console.close
  exit!
}
```

## `catch` (statement)

Statement‑form `catch` establishes an error handler for the subsequent process:

```par
catch e => {
  <handler process>
}
// ... code that may `throw` e or use `try` ...
```

Rules:

- **Same‑process condition:** all `try`/`.try`/`throw` that target this `catch` must live in the same process — not inside nested processes or sub‑expressions. (See [“Same‑process, explained”](#why-sameprocess) below.)
- **Handler termination:** the handler must end the surrounding process (e.g. with `exit!`, linking, or looping to completion). It may also `throw` to an outer `catch` (see [Throwing to a previous catch](#throwing-to-a-previous-catch-layered-cleanup)).
- Scope and shadowing: a `catch` applies to the following process; later catches shadow earlier ones.
- Error type: inferred from uses of `try`/`throw` in the scope, or explicitly specified via an annotation.

Simple throw:

```par
catch e => {
  Debug.Log(e)
  exit!
}
throw "Total meltdown"    // handled by the catch above
```

## `try` in commands

Postfix `.try` on a `Result`‑typed command (or value) expands to a `.case` that throws on error and continues on success. Start with the simplest shape:

```par
writer.writeString("hello").try

// desugars to
writer.writeString("hello").case {
  .err e => { throw e }
  .ok => {}
}
```

You can also apply `.try` to a `Result` you already have in a variable:

```par
let res = path.createOrReplaceFile
res.try

// desugars to
res.case {
  .err e => { throw e }
  .ok => {}
}
```

## Why same‑process?

It’s tempting to write something like:

```par
// This looks neat, but it is rejected on purpose
let writer = path.createOrAppendToFile.try
```

But there is a problem: Par uses concurrent evaluation. That means `path.createOrAppendToFile`
is evaluated concurrently with the process that performs the `let`. If the inner expression were allowed to `throw` via `.try`, the outer process might
already be running other commands.

There is no sound way to “rewind” and absorb that failure after the fact. To keep control
flow explicit and linear, `try`/`.try`/`throw` are restricted to the same process as their
`catch`.

The above doesn't work, but there is another way: **`try` in patterns!**

## `try` in patterns

`let try x = E` binds `x` on `.ok` and throws on `.err`. It stays in the current process, which is crucial in Par’s concurrent evaluation model.

```par
let try writer = path.createOrAppendToFile

// ≡

let writer = path.createOrAppendToFile
writer.case {
  .err e => { throw e }
  .ok => {}
}
```

It works in composite patterns:

```par
let (try left, try right)! = (leftPath.openFile, rightPath.openFile)!
```

And in receives:

```par
let console = Console.Open
catch ! => {
  console.print("Failed to read input.")
  console.close
  exit!
}

console.prompt("Name: ")[try name]
console.prompt("Address: ")[try address]
```

Here's another example of using `try` in a pattern when receiving:

```par
type Poll<e, a> = iterative choice {
  .close => Result<e, !>,
  .poll => Result<e, (a) self>,
}

// source : Poll<Os.Error, String>
source.poll.try[value]
// here `value` becomes `String`, and source keeps its type for the next iteration
```

## catch (expression)

Expression‑form `catch` evaluates an expression with a local handler and returns a value (often a `Result`):

```par
catch e => <err result> in <expression using try or throw>
```

Restrictions:

- As with statements, `try`/`throw` must live in the same process — not in nested processes created by subexpressions.
- Additionally, within the `catch ... in` expression, `try`/`throw` may only occur before any part of the final value is constructed.

Invalid (uses `try` in a nested expression):

```par
// result : Result<String, Int>
catch e => .err e in
.ok Int.Add(result.try, 1)
```

Still invalid (the outer `.ok` is too early):

```par
catch e => .err e in
.ok let try value = result in Int.Add(value, 1)
```

Correct: delay construction until after all `try`s:

```par
catch e => .err e in
let try value = result in
.ok Int.Add(value, 1)
```

**Some useful patterns:**

- Map the error (add context):
  ```par
  catch e => .err Decorate(e) in
  let try x = result in .ok x
  ```

- Map the ok value:
  ```par
  catch e => .err e in
  let try x = result in .ok Transform(x)
  ```

- Unwrap with a default:
  ```par
  catch ! => Default in result.try
  ```

## Labels

Like `begin`/`loop`, `catch` blocks can be labelled, and then referenced by `try/label` and `throw/label`.

Selection is purely by label and proximity: the nearest (most recent) `catch` with the
same label (or with no label, if you didn’t use one) is chosen. The error’s type is not
used to decide which `catch` to pick; types are checked only after a `catch` has been selected.

```par
catch/fs e => { /* handle filesystem error */ }
catch/net e => { /* handle network error */ }

let try/fs writer = path.createOrReplaceFile
let try/net conn = url.connect
```

## Throwing to a previous `catch` (layered cleanup)

Nested catches are great for local cleanup before delegating to an outer handler. Example: copy a file, ensuring both handles are closed on any failure:

```par
def Main: ! = chan exit {
  let console = Console.Open

  catch ! => { console.print("Failed to read input.").close; exit! }
  console.prompt("Src path: ")[try src]
  console.prompt("Dst path: ")[try dst]

  catch e: Os.Error => {
    console.print("An error occurred:")
    console.print(e)
    console.close
    exit!
  }

  let try reader = Os.PathFromString(src).openFile
  catch/w e => { reader.close(.ok!); throw e }

  let try/w writer = Os.PathFromString(dst).createOrReplaceFile
  catch/r e => { writer.close(.ok!); throw e }

  reader.begin.read.try/r.case {
    .end! => {
      writer.close(.ok!).try
      console.close
      exit!
    }
    .chunk(bytes) => {
      writer.write(bytes).try/w
      reader.loop
    }
  }
}
```
