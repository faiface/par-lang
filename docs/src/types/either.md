# Either

_Either types_ are the well-known [sum types](https://en.wikipedia.org/wiki/Tagged_union),
otherwise known as _tagged unions_.

They defined a finite number of variants, each with a different name and a payload. A value of
an either type is one of its variants.

```par
type StringOrNumber = either {
  .string String,
  .number Int,
}

def Str: StringOrNumber = .string "Hello!"
def Num: StringOrNumber = .number 42,
```

An either type is spelled with the keyword `either`, followed by curly braces enclosing a
comma-separated list of variants.

Each variant has a lower-case name prefixed by a period and followed by a single, obligatory
payload type:

```par
either {
  .variant1 Payload1,
  .variant2 Payload2,
  .variant3 Payload3,
}
```

Since each payload must be a single type, [units](./unit.md), [pairs](./pair.md), and other types
are used to define composite payloads. For example:

```par
type MaybeBoth<a, b> = either {
  .neither!,
  .left a,
  .right b,
  .both(a, b)!,
}
```

> Either types are frequently used together with [_recursive_](./recursive.md) types to define finite
> tree-like structures.
> 
> ```par
> type BinaryTree<a> = recursive either {
>   .empty!,
>   .node(a, self, self)!,
> }
> ```
> 
> The pre-defined `List<a>` type is a combination of `recursive` and `either`:
> 
> ```par
> type List<a> = recursive either {
>   .end!,
>   .item(a) self,
> }
> ```

## Construction

Values of _either types_ are constructed starting with `.name` — the name of one of the variants in
the type — followed by an expression of the corresponding payload type.

Here are some examples of constructions for an either type that demonstrates many possible payloads:

```par
type Varied = either {
  .unit!,                        // payload is `!`
  .string String,                // payload is `String`
  .number Int,                   // payload is `Int`
  .pair(Int) String,             // payload is `(Int) String`
  .symmetricPair(Int, String)!,  // payload is `(Int, String)!`
  .nested either {               // payload is another either type
    .left!,
    .right!,
  },
  .nested2(String) either {      // payload is a pair of `String` and another either
    .left!,
    .right!,
  }
}

def Example1: Varied = .unit!
def Example2: Varied = .string "Hello!"
def Example3: Varied = .number 42
def Example4: Varied = .pair(42) "Hello!"
def Example5: Varied = .symmetricPair(42, "Hello!")!
def Example6: Varied = .nested.left!
def Example7: Varied = .nested.right!
def Example8: Varied = .nested2("Hello!").left!
def Example9: Varied = .nested2("Hello!").right!
```

[Pairs](./pair.md) are frequently used in payloads of either types, both in their symmetric and sequential
styles. The sequential style makes chaining either types with attached payloads very natural, like
in the `.nested2` variant.

## Destruction

Values of _either types_ can be deconstructed using `.case` expressions, similar to pattern-matching in
other languages.

A `.case` expression starts with the value to be destructed, followed by `.case`, and a list of
comma-separated branches enclosed in curly braces, one per each variant.

```par
value.case {
  // branches
}
```

Each branch consists of the name of its variant, a _pattern_ to assign the payload to, then a `=>`
followed by an expression computing the result for that branch. All branches must evaluate to the
same type.

```par
// branch
.name pattern => expression,
```

The patterns to assign the payloads are the same as can appear on the left side of
`let` assignments:
- `variable` matches the whole value.
- `!` matches [units](./unit.md).
- `(pattern1, ...) patternN` matches [pairs](./pair.md).

For a small example, we analyze the `Str` and `Num` values of the `StringOrNumber` type from above:

```par
// evaluates to "Hello!"
def ResultForStr = Str.case {
  .string s => s,
  .number n => Int.ToString(n),
}

// evaluates to "42"
def ResultForNum = Num.case {
  .string s => s,
  .number n => Int.ToString(n),
}
```

For a comprehensive example, here's a big [function](./function.md) converting the above `Varied` type
to a `String`:

```par
dec VariedToString : [Varied] String
def VariedToString = [varied] varied.case {
  .unit! => ".unit!",

  .string s => String.Builder.add(".string ").add(String.Quote(s)).build,

  .number n => String.Builder.add(".number ").add(Int.ToString(n)).build,

  .pair(n) s =>
    String.Builder
      .add(".pair(")
      .add(Int.ToString(n))
      .add(") ")
      .add(String.Quote(s))
      .build,

  .symmetricPair(n, s)! =>
    String.Builder
      .add(".symmetricPair(")
      .add(Int.ToString(n))
      .add(", ")
      .add(String.Quote(s))
      .add(")!")
      .build,

  .nested inside => String.Builder.add(".nested").add(inside.case {
    .left! => ".left!",
    .right! => ".right!",
  }).build,

  .nested2(s) inside =>
    String.Builder
      .add(".nested2(")
      .add(String.Quote(s))
      .add(")")
      .add(inside.case {
        .left! => ".left!",
        .right! => ".right!",
      }).build,
}
```
