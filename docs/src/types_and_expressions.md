# Types & Their Expressions

Types in Par serve two seemingly incompatible purposes at the same time:

- Objects of every-day programming, like functions and pairs.
- [Session-typed](https://en.wikipedia.org/wiki/Session_type) communication channels.

In the concurrent framework of linear logic, these are one and the same. But to make this
connection harmonious and ergonomic, some unusual choices have to be made in the design of
the basic building blocks.

**Types in Par are sequential.** The basic building blocks — pairs, functions, eithers (sums),
and choices (co-sums) — all read as **first this, then that.**

Let's take pairs. In many programming language, `(A, B)` is the type of a pair of `A` and `B`.
This approach is not sequential: both types assume equal position.

In Par, the pair type is instead `(A) B`. The second type being outside of the parentheses is
essential. It allows us to sequentially continue the type without the burden of nesting.

Compare

- `(A, B)`
- `(A, (B, C))`
- `(A, (B, (C, D)))`

against

- `(A) B`
- `(A) (B) C`
- `(A) (B) (C) D`

Of course, most languages that provide `(A, B)` pairs also support triples `(A, B, C)`, and
quadruples `(A, B, C, D)`, so let's mix it up!

The usual syntax for function types is `A -> B`. That is sequential, but in Par we have a syntax
that plays more nicely with the pairs: `[A] B`. Now compare

- `(A, B -> (C, D -> E))`

versus

- `(A) [B] (C) [D] E`

We can read it as: _first give `A`, then take `B`, then give `C`, then take `D`, and finally give `E`._

This is starting to look a lot like session types! An alternative reading of the type could be:
_first send `A`, then receive `B`, then send `C`, then receive `D`, and finally proceed as `E`._

And that, in a nutshell, is how Par unifies every-day types with session types.

**This chapter covers the every-day aspect of types in Par.** For the session types aspect, check
out [Processes](./processes.md).
