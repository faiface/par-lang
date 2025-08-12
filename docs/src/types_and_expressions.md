# Types & Their Expressions

Types in Par serve two seemingly incompatible purposes at the same time:

- Objects of every-day programming, like functions and pairs.
- [Session-typed](https://en.wikipedia.org/wiki/Session_type) communication channels.

In the world of linear logic, these are the same thing. But to make this
connection harmonious and ergonomic, some unusual choices have to be made in the design of
the basic building blocks.

**Types in Par are sequential.** The basic building blocks — pairs, functions, eithers (sums),
and choices (co-sums) — all read as **first this, then that.**

Let's take pairs. In many programming language, `(A, B)` is the type of a pair of `A` and `B`.
This approach is not sequential: both types assume equal position.

In Par, the pair type is instead `(A) B`. The second type being outside of the parentheses is
essential. It allows us to sequentially continue the type without the burden of nesting.

Compare `(A, (B, (C, D)))` against `(A) (B) (C) D`.

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

## Linearity

Par is based on [linear logic](https://en.wikipedia.org/wiki/Linear_logic), and with that comes a
**linear type system.** That means: **some values must be used exactly once.**

These are called **linear values.** You can't copy them, and you can't throw them away.
They must be consumed, **exactly once** — in a way their type allows.

This might sound limiting, but it opens the door to something powerful.

When a value must be used — and can only be used once — it becomes possible to **model communication.**
Think about a channel that expects you to send a message. If you don’t send one — or send
two — things fall apart.

With linearity, Par gives you channels where that simply can’t happen.

That's the foundation of [session types](https://en.wikipedia.org/wiki/Session_type),
and Par supports them at its core.

But not every type needs that kind of strictness. Some values should be copyable, droppable,
and passed around freely.

So Par distinguishes between:

- **Linear types,** which must be used exactly once.
- **Non-linear types,** which can be used any number of times — including zero.

Let’s look at what falls into each category.

### Which types are non-linear?

These are:

- All [**primitives**](./structure/primitive_types.md): `Int`, `Nat`, `String`, `Char`
- [**Unit**](./types/unit.md)
- [**Either**](./types/either.md)
- [**Pair**](./types/pair.md)
- [**Recursive**](./types/recursive.md)
- [**Box**](./types/box.md)
- [**Exists**](./types/exists.md), only if the body is non-linear — which is rare, since the
  type variable it introduces is itself linear
- Any type **composed entirely** of non-linear parts

### Which types are linear?

All the rest:

- [**Function**](./types/function.md)
- [**Choice**](./types/choice.md)
- [**Iterative**](./types/iterative.md)
- [**Exists**](./types/exists.md), **in most cases,** because they introduce a linear type variable
- [**Forall**](./types/forall.md)
- [**Continuation**](./types/continuation.md)
- Any type that **contains** a linear component, even deeply

If a type has a linear piece **anywhere inside it,** it becomes linear — unless that part is
wrapped in a [box](./types/box.md).
