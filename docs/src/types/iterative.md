# Iterative

We already covered one kind of self-referential types: [_recursive types_](./recursive.md). Now we cover
the other kind: _iterative types_. They are also known as
[_coinductive_](https://en.wikipedia.org/wiki/Coinduction), or corecursive types, because they enable
[_corecursion_](https://en.wikipedia.org/wiki/Corecursion).

In a nutshell:
- Values of **recursive types** _are_ something repeated _some_ number of times.
- Values of **iterative types** _can_ repeat something _any_ number of times.

Recursive types tell you how many times you need to step through them to reach the end. That's what
`.begin`/`.loop` does. If there is a `self`, you can always `.loop` through it, and proceed with the
recursion until you reach the end.

But iterative types let you tell _them_ how many times you want to repeat them. They have the ability
to unfold as many times as you like. It's up to you, the consumer, to proceed.

So, let's take a look at the **iterative types.**

An iterative type starts with the keyword `iterative` followed by a body that may contain any number
of occurrences of `self`. Notice that the pattern is the same as with [recursive types](./recursive.md).

The prototypical iterative type — and a good example to study — is an infinite sequence.

```par
type Sequence<a> = iterative choice {
    .close => !,
    .next => (a) self,
}
```

> **If there are nested `iterative` (or [`recursive`](./recursive.md)) types, it may be necessary to
> distinguish between them.** For that, we can attach **labels** to `iterative` and `self`. That's
> done with a slash: `iterative/label`, `self/label`. Any lower-case identifier can be used for the
> label.

Iterative types are **always linear,** regardless of what's in their bodies. As such, they can't be
dropped, nor copied.

Notice that we included a `.close` branch on the inner [choice](./choice.md). Since `Sequence<a>` is
a linear type, there would be no way to get rid of it if it only contained the `.next` branch.

Just like [recursive types](./recursive.md), iterative types can be equated with their _expansions_:

1. The original definition:
   ```par
   type Sequence<a> = iterative choice {
     .close => !,
     .next => (a) self,
   }
   ```
2. The first expansion:
   ```par
   type Sequence<a> = choice {
     .close => !,
     .next => (a) iterative choice {
       .close => !,
       .next => (a) self,
     },
   }
   ```
3. The second expansion:
   ```par
   type Sequence<a> = choice {
     .close => !,
     .next => (a) choice {
       .close => !,
       .next => (a) iterative choice {
         .close => !,
         .next => (a) self,
       },
     },
   }
   ```
4. And so on...

> Just like [`recursive`](./recursive.md) types, `iterative` types have an important restriction:
> **every `self` reference for an `iterative` must be guarded by a `choice`.** The `choice` doesn't
> have to be right next to the `iterative`, but it *has* to be somewhere in-between `iterative` and
> `self`:
>
> ```par
> type ValidSequence<a> = iterative (a) choice {
>   .close => !,
>   .next => self,  // Okay. This `self` is guarded by a `choice`.
> }
> 
> type InvalidSequence<a> = iterative (a) self  // Error! Unguarded `self` reference
> ```

So, if both [recursive](./recursive.md) and iterative types can be equated with their expansions,
**what's the difference?** The difference lies in their construction and destruction:
- **Recursive types** are **constructed step by step** and **destructed by loops**.
- But, **iterative types** are **constructed by loops** and **destructed step by step**.

Let's see what that means!

## Construction

Values of iterative types are constructed using standalone `begin`/`loop` expressions. They start with
`begin`, followed by an expression of the body type. Inside this expression, use a standalone `loop`
in the `self` places of the body type to go back to the corresponding `begin`.

> Just like with [recursive's](./recursive.md) `.begin`/`.loop`, it's possible to use labels to
> distinguish between nested `begin`/`loop` (and `.begin`/`.loop`) uses. Just use a slash:
> `begin/label` and `loop/label`.

Here's a simple `Sequence<Int>` that produces the number `7` forever:

```par
dec SevenForever : Sequence<Int>
def SevenForever = begin case {
  .close => !,
  .next  => (7) loop,
}
```

The `.next` branch produces a pair, as per the sequence's body type, with the second element being
the new version of the sequence. Here we use `loop` to accomplish that corecursively, looping back
to the `begin`.

The corecursive meaning of `begin`/`loop` can again be understood by seeing its expansions:

1. The original code:
   ```par
   def SevenForever = begin case {
     .close => !,
     .next  => (7) loop,
   }
   ```
2. The first expansion:
   ```par
   def SevenForever = case {
     .close => !,
     .next  => (7) begin case {
       .close => !,
       .next  => (7) loop,
     },
   }
   ```
3. The second expansion:
   ```par
   def SevenForever = case {
     .close => !,
     .next  => (7) case {
       .close => !,
       .next  => (7) begin case {
         .close => !,
         .next  => (7) loop,
       },
     },
   }
   ```
4. And so on...

**Retention of local variables** works the same as in [recursive's](./recursive.md) `.begin`/`.loop`.
With iterative types, we can use it to carry and **update the internal state** of the iterative object.

For example, here's an infinite sequence of fibonacci numbers:

```par
def Fibonacci: Sequence<Nat> =
  let (a, b)! = (0, 1)!
  in begin case {
    .close => !,
    .next =>
      let (a, b)! = (b, Nat.Add(a, b))!
      in (a) loop
  }
```

This is very useful. In most programming languages, constructing a similar `Fibonacci` object would
require defining a `class` or a `struct` describing the internal state, and then updating it in
methods. In Par, iterative objects can be constructed using anonymous expressions, with no need
of specifying their internal state by a standalone type: **the internal state is just local variables.**

In the `Fibonacci`'s case, the internal state is non-linear. That's why we're able to return a bare unit
in the `.close` branch: `a` and `b` get dropped automatically.

Let's take a look at a case where the internal state is linear! Suppose we need a function that takes
an arbitrary sequence of integers, and increments its items by `1`, producing a new sequence.

```par
dec Increment : [Sequence<Int>] Sequence<Int>
def Increment = [seq] begin case {
  .close => let ! = seq.close in !,
  .next =>
    let (x) seq = seq.next
    in (Int.Add(x, 1)) loop
}

def FibonacciPlusOne = Increment(Fibonacci)
```

In this case, we need to explicitly close the input `seq` in the `.close` branch. It's linear, so
we can't just drop it.

### The escape-hatch from totality: `unfounded`

Just like with [recursive destruction](./recursive.md#the-escape-hatch-from-totality-unfounded),
it may happen that your iterative construction is total — meaning it never enters an infinite,
unproductive loop — yet not accepted by Par's type checker. In such cases, it's possible to
disable Par's totality checking by replacing `begin` with `unfounded`.

## Destruction

Iterative types don't have any special syntax for destruction. Instead, we just operate on their bodies
directly, as if they were expanded.

For example, here's a function to take the first element from a sequence and close it:

```par
def Head = [type a] [seq: Sequence<a>]
  let (x) seq = seq.next
  in let ! = seq.close
  in x
```

Using [recursion](./recursive.md), we can destruct an iterative type many times. Here's a function to
take the first N elements of a sequence and return them in a list:

```par
dec Take : [type a] [Nat, Sequence<a>] List<a>
def Take = [type a] [n, seq] Nat.Repeat(n).begin.case {
  .end! => let ! = seq.close in .end!,
  .step remaining =>
    let (x) seq = seq.next
    in .item(x) remaining.loop
}
```
