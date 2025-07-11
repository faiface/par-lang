# Introduction

**Par** is affectionatelly named after the most bemusing connective of
[linear logic](https://en.wikipedia.org/wiki/Linear_logic): ⅋, pronounced _"par"_.
That's because Par is based directly on (classical) linear logic, as an experiment to
see where this paradigm can take us.

[Jean-Yves Girard](https://en.wikipedia.org/wiki/Jean-Yves_Girard) — the author of linear logic,
and [System F](https://en.wikipedia.org/wiki/System_F), among other things — wrote on the page 3
of his [first paper on linear logic](https://www.sciencedirect.com/science/article/pii/0304397587900454):

> The new connectives of linear logic have obvious meanings in terms of
parallel computation, especially the multiplicatives.

This was in 1987. In hindsight, it wasn't that obvious.

Par is an attempt to take that idea seriously — to turn linear logic into a practical programming language.

## Not eager. Not lazy. _Concurrent._

In Par, evaluation starts as soon as a value is defined. But you don’t wait for it to finish —
evaluation happens **concurrently.**

It’s a bit like a world where everything is `async`, and nothing ever needs an `await`. Even functions
start evaluating as soon as they're constructed — before they even receive an argument.

This model avoids blocking entirely — except when **choosing a branch.** That’s the only time things pause.
Want laziness? Wrap a value in a [choice](./types/choice.md). It won't evaluate until the choice is
selected. That’s all it takes.

This gives you precise control over evaluation — where needed, and never more.
**Evaluation order is part of the type system.** It’s explicit, safe, and plays well with concurrency.

## Types that describe behavior

Par’s types don’t just describe what values are — they describe what values do.

- A [function](./types/function.md) tells you what input is needed and what comes next.
- An [iterative](./types/iterative.md) value is like an object you can interact with repeatedly.
- A [choice](./types/choice.md) offers structured operations;
  an [either](./types/either.md) demands case analysis.
- A [box](./types/box.md) marks values as reusable and discardable.
- A [recursive](./types/recursive.md) type unfolds until it ends.
- And more...

In fact, Par's types are [session types](https://en.wikipedia.org/wiki/Session_type). They let you express
entire communication protocols — values that unfold in well-typed steps, over time.

That makes Par’s type system unusually powerful: It models **interaction,** not just data.

## Paradigms emerging from first principles

Par is naturally good in functional programming, but it doesn't end there. Other paradigms emerge out of the
underlying linear logic foundation.

One does it in a rather unique way: **object-oriented programming.**

An [`iterative choice`](./types/iterative.md) type behaves like an interface:
a collection of named operations, available repeatedly. To “implement” one, you just construct a value
that behaves accordingly — no classes, inheritance, or ceremony.

**Dynamic dispatch** is first-class, via [choice](./types/choice.md) types. You dispatch directly on values,
not on subclasses. This makes behavior easy to model and easy to pass around.

Values become objects, just by what they can do.

## Orthogonality goes wide, not deep

Par doesn’t have dependent types. It doesn’t have metaprogramming, higher-order kinds, or macro systems.

Instead of going deeper into complexity, **Par goes wider.**

Its design focuses on small, composable ideas. For Par, it's not that important to have a small _number_
of features. What's important is that each feature is small, and covers something no other feature does.

Most of those ideas are taken directly from classical linear logic.
Every type corresponds to a logical connective. Even recursion and polymorphism are built from first principles
— not bolted on.

And in doing so, it makes room for multiple paradigms to emerge:
- Functional programming with side-effects via linear handles.
- A unique object-oriented style, where interfaces are just types and implementations are just values.
- A concurrent foundation, where execution is implicit and non-blocking by default.

All of this comes from the same source: the logic underneath.

## An ambitious stride towards totality

As if session types and concurrency weren't enough, Par also aims to be **total.**

That means:
- **No exceptions or panics.**
- **No deadlocks.** Par imposes a structure where deadlocks are impossible to express.
- **No accidental non-termination.** By default, recursion and corecursion are checked to prevent infinite loops.

At the moment, the system isn't powerful enough to capture some more complex algorithms, so there's an
escape hatch. But, the eventual goal is to get rid of it.

## Let's dive in!

Par is a language in active development. It’s not production-ready — but it’s expressive, and ready to be
explored.

If you’re curious about what programming can look like when guided by logic — **turn the page.**
