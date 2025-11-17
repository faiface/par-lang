# Looping & Branching

We’ve now seen how commands work with [choice types](/types/choice.md) (via selection),
and [function types](/types/function.md) (via sending). But those aren’t the only types that
come with their own command styles. Every type does.

Let’s turn our attention to something more intricate: **recursive types.**

Take this familiar definition:

```par
type List<a> = recursive either {
  .end!,
  .item(a) self,
}
```

The `List` type is [recursive](/types/recursive.md), and contains an
[`either`](/types/either.md), and within that, a [pair](/types/pair.md).

To work with such a structure in process syntax, we’ll need to combine three kinds of commands:

- `.begin` and `.loop` for recursion,
- `.case` branching on the either,
- And `value[variable]` for receiving (peeling off) a value from a pair.

We're now going to demonstrate all of these by implementing a string concatenation function.

```par
dec Concat : [List<String>] String
```

This function takes a list of strings, and returns them concatenated. We'll use process syntax and
see how it works out!

```par
def Concat = [strings] do {
  // code continued below...
```

**Let’s go step by step!**

## Recursion in processes

We start by telling the process: _"Hey, we’re about to work with a recursive value!"_

That’s what `.begin` does. It establishes a looping point, to which we can jump back later
using `.loop`.

```par
  strings.begin
```

## Branching

The `.case` command behaves similarly to expression-based `.case`, but with two key differences:
In process syntax, the **body** of each `.case` branch **is a process, not a value.**

This means the syntax requires curly braces after the `=>`. You do something in each branch —
not return something.

```par
  strings.case {
    .end! => {
      // empty list, nothing to do
    }
    .item => {
```

There's another key difference: notice the `.item` branch has `=>` right after the branch name!
There's no pattern. That's because in process syntax, binding the payload of an
[either](/types/either.md) is optional. Normally, **the subject itself becomes the payload.**
However, it is possible to match the payload fully, if desired.

So, inside the `.item` branch, `strings` now has the type `(String) List<String>` — it’s a pair, and
we want to peel off its first part, to add it to the string builder.

There's **one last important detail,** and that's concerning **control flow.** If a `.case`
branch process does not end (we'll learn about ending processes in the section about
[`chan` expressions](/processes/chan_expression.md)), then it proceeds to the next line after the closing
parenthesis of the `.case` command. All local variables are kept.

## Receiving

To peel a value from a pair, we use the **receive command:**

```par
      strings[str]
```

This command says:
_"Take the first part of the pair and store it in `str`. Keep the rest as the new value of `strings`."_

Now we can assemble the basic skeleton:

```par
def Concat = [strings] do {
  let builder = String.Builder
  strings.begin.case {
    .end! => {
      // nothing to do
    }
    .item => {
      strings[str]
      builder.add(str)
      strings.loop
    }
  }
} in builder.build
```

This looks _very_ imperative! The variable `strings` flows through the process —
branching, unwrapping, and looping — without needing to be reassigned. It performs all the
different operations based on its changing type. Meanwhile, `builder` accumulates the result,
tagging along the control flow of `strings`.

## Patterns in `.case` branches

The `.item =>` branch can be made a little more pleasant. If the subject after .begin is a
[pair](/types/pair.md), we’re allowed to use pattern matching directly in the `.case` branch.

That means this:

```par
    .item => {
      strings[str]
      builder.add(str)
      strings.loop
    }
```

Can be rewritten as:

```par
    .item(str) => {
      builder.add(str)
      strings.loop
    }
```

Which is exactly what we’ll do in the final version:

```par
dec Concat : [List<String>] String
def Concat = [strings] do {
  let builder = String.Builder
  strings.begin.case {
    .end! => {}
    .item(str) => {
      builder.add(str)
      strings.loop
    }
  }
} in builder.build

def TestConcat = Concat(*("A", "B", "C"))  // = "ABC"
```

This beautifully ties together all the commands we've covered so far:
- **Selection** for [choices](/types/choice.md).
- **Sending** for [functions](/types/function.md).
- **Branching** for [eithers](/types/either.md).
- **Receiving** for [pairs](/types/pair.md). Here, in the form of a pattern.

The _receive commands_ is the least clearly useful here. Let's move to infinite sequences to see a
more compelling use-case.
