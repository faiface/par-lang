# Conditions & `if`

Par has `if` in both expression syntax (it returns a value) and process syntax
(it runs process code). What makes Par’s `if` unusual is the condition language:
conditions can match `either`s and bind names, and those bindings flow through
`and`/`or`/`not`.

We'll start with a tiny `if` you can read left to right, then grow it into the
full condition language, and finally see how the same ideas apply in process
code.

## Expression `if { ... }`

Let's start with a small example:

```par
dec Describe : [Bool] String
def Describe = [flag] if {
  flag => "on",
  else => "off",
}
```

Read the syntax left to right:

- `if {` starts the conditional expression.
- A branch is `<condition> => <expression>`.
- Branches are separated by commas.
- `else => ...` is the fallback branch.
- The closing `}` ends the expression.

In other words:

```par
if {
  <condition> => <expression>,
  ...
  else => <expression>,
}
```

Values of the `Bool` type (an `either { .true!, .false! }`) can be used directly
as conditions, so any expression producing `Bool` can be used on the left of
`=>`.

Unlike the common `if ... else if ... else` chain found in many languages, Par’s
“normal” `if` is a single `if { ... }` block with any number of branches.

To evaluate an `if { ... }`, read it like this:

- Check branches from top to bottom.
- For each branch, evaluate its condition.
- The first condition that succeeds selects its branch, and the `if` evaluates
  to that branch’s expression.
- If no condition succeeds, the `else` branch is chosen.

## `is`: matching an `either` (and binding)

Par’s “sum type” is [`either`](../types/either.md). If you just want to match an
`either` without extra predicates, you can always use `.case`. `if` becomes
useful once you want to match *and* filter while keeping bindings.

Two common `either` types you’ll see everywhere are `Result` and `Option`:

```par
type Result<e, a> = either {
  .err e,
  .ok a,
}

type Option<a> = Result<!, a>
```

An `is` condition checks an `either` and binds its payload. Its shape is:

```par
<value> is .<variant><payload-pattern>
```

The payload pattern is always present. For a unit payload you still write `!`,
so `value is .less!` is valid, but `value is .less` is not.

Here is the return type of `Int.Compare`:

```par
type Ordering = either {
  .less!,
  .equal!,
  .greater!,
}
```

Now an `if` can match directly on those variants:

```par
dec CompareSign : [Int, Int] String
def CompareSign = [x, y]
  let cmp = Int.Compare(x, y) in
  if {
    cmp is .less! => "less",
    cmp is .greater! => "greater",
    else => "equal",
  }
```

Read it top to bottom:

- If `cmp is .less!` succeeds, the whole `if` becomes `"less"`.
- Otherwise, if `cmp is .greater!` succeeds, the whole `if` becomes `"greater"`.
- Otherwise it falls through to `else` and becomes `"equal"`.

`is` only works with `either` types. You cannot write `is 5` or match arbitrary
values; use normal boolean expressions for those.

## `and`: match, then refine

In Par, `and`/`or`/`not` are not just boolean algebra operators. They are
control-flow constructs: they short-circuit, and those success/failure paths are
how bindings from `is` become available (or not available) later in the
condition and inside the branch.

### `and` with an extra predicate

```par
dec CountStatus : [Option<Nat>] String
def CountStatus = [count] if {
  count is .ok n and Nat.Equals(n, 0) => "zero",
  count is .ok _ => "non-zero",
  else => "missing",
}
```

Step by step:

- Try the first branch.
  - First evaluate `count is .ok n`. If it fails, the whole `and` fails.
  - If it succeeds, `n` is bound and Par evaluates `Nat.Equals(n, 0)`.
  - Only if both parts succeed does the branch return `"zero"`.
- If the first branch fails, try the second branch `count is .ok _`, which
  matches any present value and returns `"non-zero"`.
- If both branches fail, `count` must be `.err!`, so `else` returns `"missing"`.

### `and` with two bindings

```par
dec AddOk : [Result<String, Int>, Result<String, Int>] Int
def AddOk = [left, right] if {
  left is .ok a and right is .ok b => Int.Add(a, b),
  else => 0,
}
```

If the whole condition succeeds, it means both matches succeeded, so both `a`
and `b` are in scope in the branch.

Read it like this:

- Try to bind `a` from `left`.
- Only if that succeeds, try to bind `b` from `right`.
- Only if both succeed does the branch run `Int.Add(a, b)`.

## `or`: try one condition, then another

Like `and`, `or` short-circuits. The right side only runs if the left side
fails. A good mental model is:

> try the left condition; if it fails, try the right condition

One common use for `or` is “fallback” matching:

```par
dec PickOk : [Result<String, String>, Result<String, String>] String
def PickOk = [primary, fallback] if {
  primary is .ok value or fallback is .ok value => value,
  else => "<missing>",
}
```

Read it as two attempts:

- Try `primary is .ok value`.
- Only if that fails, try `fallback is .ok value`.
- If either succeeds, the branch runs and returns `value`.

If you want to use a binding after an `or`, bind the *same name* on every
success path (as above). If the two sides bind different names, neither name is
guaranteed to exist after the `or`.

## Grouping conditions with `{ ... }`

`not`, `and`, and `or` have precedence (`not` > `and` > `or`). To make
grouping explicit, you can use `{ ... }` *inside a condition*:

```par
dec EmptyOrSpace : [Result<String, String>] Bool
def EmptyOrSpace = [result] if {
  result is .ok s and { String.Equals(s, "") or String.Equals(s, " ") }
    => .true!,
  else => .false!,
}
```

These braces group the condition. They are different from the braces after
`if` in `if { ... }`, which contain the branches.

## `not`: bindings move to the failure path

`not` is different from most languages here: it flips success and failure, and
that flip also affects bindings. Any bindings introduced inside the condition
end up on the *failure* path of `not`.

```par
dec UseOrError : [Result<String, String>] String
def UseOrError = [result] if {
  not result is .ok value => "error",
  else => value,
}
```

Read it carefully:

- `result is .ok value` would succeed on `.ok value`.
- `not` flips success and failure.
- So the `else` branch is the path where `result is .ok value` holds, and
  `value` is available there.

Because `or` only evaluates its right side when the left side fails, you can
combine `not` and `or` in a way that “unlocks” bindings on the right:

```par
dec NonEmptyOrError : [Result<String, String>] String
def NonEmptyOrError = [result] if {
  not result is .ok str or String.Equals(str, "") => "bad input",
  else => str,
}
```

Follow the control flow:

- First evaluate `not result is .ok str`.
- The right side `String.Equals(str, "")` runs only if the left side fails.
- The left side fails exactly when `result is .ok str` succeeds.
- That is why `str` is available on the right side and in the `else` branch.

This also works with grouped conditions:

```par
dec AddBothOrZero : [Result<String, Int>, Result<String, Int>] Int
def AddBothOrZero = [left, right] if {
  not { left is .ok x and right is .ok y } => 0,
  else => Int.Add(x, y),
}
```

Read it the same way: `x` and `y` are bound by the condition inside `{ ... }`,
and because of `not`, they become available on the `else` path.

### A quick comparison (Java Optional)

In Java, the same idea often becomes "check, then get":

```java
if (result.isEmpty() || result.get().isEmpty()) {
    log("bad input");
    return;
}
var str = result.get();
log(str);
```

Par keeps the match and the predicate together in one place.

## Standalone boolean expressions

You can also compute booleans directly:

```par
let ok = left or right
```

Bindings created inside such a boolean expression stay **inside** that
expression:

```par
let ok = result is .ok msg and String.Equals(msg, "")
// `msg` is not available here
```

Use `if` when you want bindings to escape into a branch.

## `if` in process syntax

In process syntax, branches contain *process code* in braces, and execution can
fall through after the `if`.

The multi-branch form looks like:

```par
if {
  <condition> => { <process> }
  ...
  else => { <process> }
}
```

(No commas here — branches are separated by whitespace, like `.case { ... }`.)

If you haven't seen process syntax yet, start with [The Process Syntax](../process_syntax.md).
In short: `do { ... } in expr` runs the process inside the braces, then
continues as the expression after `in`.

```par
dec Show : [Result<String, String>] !
def Show = [result] do {
  if {
    result is .ok msg => { Debug.Log(msg) }
    else => { Debug.Log("bad") }
  }
  Debug.Log("after") // fallthrough
} in !
```

Just like in the expression form, branches are checked top to bottom and the
first matching branch runs. After the `if { ... }` finishes, the process
continues with the fallthrough code.

### Single-condition process `if`

There is also a special single-condition form:

```par
if <condition> => { <process> }
<more process code>
```

Here `<more process code>` acts as the `else` branch and also as the
fallthrough, which is great for early exits.

This is a very common style for guard clauses: “if the input is bad, exit;
otherwise continue”.

```par
dec DefaultIfEmpty : [String] String
def DefaultIfEmpty = [text] chan out {
  if String.Equals(text, "") => {
    out <> "<empty>"
  }
  out <> text
}
```

We use `chan` here so we can return early: linking `out <> "<empty>"` ends the
process immediately.

A common style is to use a single-condition `if` as a guard:

```par
dec LogNonEmptyOk : [Result<String, String>] !
def LogNonEmptyOk = [result] chan exit {
  if not result is .ok str or String.Equals(str, "") => { exit! }
  Debug.Log(str)
  exit!
}
```

Read it as: “if the input is bad, exit; otherwise, `str` is bound and safe to
use”.

## Omitting `else` in `if { ... }`

The multi-branch `if { ... }` form may omit `else` **only if the conditions are
exhaustive**. This is checked by types.

For example, this works because `cmp` has type `Ordering`:

```par
let cmp = Int.Compare(x, y) in
if {
  cmp is .less! => "less",
  cmp is .equal! => "equal",
  cmp is .greater! => "greater",
}
```

```par
if {
  list is .item(x) xs => { ... }
  list is .end! => { ... }
}
```

In process syntax, code after the `if { ... }` is still just a fallthrough, not
an implicit `else`.

## Wrap-up

- Expression `if { ... }` returns a value.
- `is` matches an `either` and always includes a payload pattern.
- `and`/`or`/`not` short-circuit and carry bindings from `is` through the paths.
- Standalone boolean expressions keep their bindings local.
- In process syntax, multi-branch `if { ... }` falls through.
- The single-condition process form also uses the following code as an `else`.
