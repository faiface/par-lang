# Implicit generics

Par already supports explicit generics via [`forall`](./forall.md):

```par
dec Swap : [type a, b] [(a, b)!] (b, a)!
def Swap = [type a, b] [pair]
  let (first, second)! = pair
  in (second, first)!
```

With `forall`, callers pass types explicitly when they use `Swap`:

```par
def Pair = ("Hello!", 42)!
def Swapped = Swap(type String, Int)(Pair)
```

That is precise and fully explicit. But in many everyday cases, the types are
obvious from the value you are passing. For those cases, Par also has **implicit
generics**.

## Syntax

Implicit generics are not a standalone type. The `<a, ...>` binder is an
extension of a **[function](./function.md) type**: it must appear immediately
before the `[...]` of that type.

```par
<a, b>[Arg] Res
```

You cannot write `<a>` in front of `either`, `choice`, `box`, or anything else.

The key rule is local inference:

**Implicit type variables are inferred only from the single argument
immediately following the `<...>` binder.**

This is also reflected in the syntax: the bracket right after `<...>` must
contain exactly one argument type. If you need multiple value arguments, you
still write a curried function type.

So this is valid:

```par
dec Concat : <a>[List<a>] [List<a>] List<a>
```

But this is not valid syntax:

```par
dec Concat : <a>[List<a>, List<a>] List<a>
```

When calling such a function, you can still pass all arguments at once:

```par
Concat(left, right)
```

The type is still written in curried form, because inference must happen from
that first argument alone.

One more important point: there is no syntax for “manually specifying” implicit
type arguments. When designing an API you choose, for each type variable,
whether it is implicit or explicit:

- If it is implicit (`<a>`), it is always inferred.
- If it is explicit (`[type a]`), it is always specified by the caller.

This is intentional: it keeps call sites predictable.

## Construction

Start with the `Swap` example, but make it implicit:

```par
dec Swap : <a, b>[(a, b)!] (b, a)!
```

To construct a value of this type, you also introduce the implicit type names
at the term level, and then receive the value argument:

```par
dec Swap : <a, b>[(a, b)!] (b, a)!
def Swap = <a, b>[pair]
  let (first, second)! = pair
  in (second, first)!
```

This is intentionally different from `forall`: an implicit generic and an
explicit `forall` generic are different types, and they do not subtype into one
another.

You can also use implicit generics when the type variable is nested inside a
larger argument:

```par
dec Flatten : <a>[List<List<a>>] List<a>
```

## Destruction

To use an implicit generic, you call it like an ordinary function:

```par
def Swapped = Swap(Pair)
```

Here `a = String` and `b = Int` are inferred from the type of `Pair` (the
argument immediately following `<a, b>`).

If you want to steer inference, you can still add annotations locally, for
example by ascribing a type to an expression:

```par
type T in expr
```

For example (this is a bit contrived, but it shows the point):

```par
Concat(type List<Nat> in *(), numbers)
```

If you wrote `Concat(*(), numbers)`, the empty list `*()` would lead `a` to be
inferred as `either {}` (an impossible type), so inference would not pick `Nat`
for you.

## When implicit generics are a bad fit

Implicit generics infer their type variables from the *argument value*. This
works great when that argument already has a clear type (globals, variables,
fully typed expressions), but it becomes harder when the argument’s type cannot
be inferred without annotations.

A classic example is an anonymous function. The `box` here is not the point —
it just happens that `Map` takes a boxed function.

```par
Map(numbers, box Int.ToString)        // fine: Int.ToString has a known type
Map(numbers, box [x] Int.ToString(x)) // may fail without more info
```

(`Map` is discussed in [Box](./box.md).)

In the second call, the type of `[x] ...` cannot be inferred unless the type of
`x` is specified. If the function’s type parameters are being inferred from the
mapper argument itself, you end up in a chicken-and-egg situation.

The usual fix is to add an annotation:

```par
Map(numbers, box [x: Int] Int.ToString(x))
```

This is why implicit generics can be *made difficult* by higher-order arguments
like lambdas: if the type of the lambda argument is not already specified, the
lambda’s type won’t be inferred.

For example, if `Map` tried to infer the result type from the mapper itself:

```par
dec Map : <a>[List<a>] <b>[box [a] b] List<b>
```

then a call like this may fail:

```par
Map(numbers, box [x] Int.ToString(x))
```

because the type of `x` is not specified, so the type of the function cannot be
inferred, so `b` cannot be inferred either. You end up needing to annotate `x`:

```par
Map(numbers, box [x: Int] Int.ToString(x))
```

Many “map-like” APIs therefore prefer to infer the input type and keep the
output type explicit, e.g.:

```par
dec Map : <a>[List<a>] [type b] [box [a] b] List<b>
```

With this type, the mapper is checked against the expected type `box [a] b`, so
`x` can be inferred as `a` and usually needs no annotation. Picking the result
type `b` is also often more natural than restating the argument type.

## Implicit generics and pairs

Implicit generics also exist for [pair](./pair.md) types. This is the implicit
counterpart of [`exists`](./exists.md).

Just like with functions, the `<a, ...>` binder is part of the following pair
type: it must appear right before the `(...)` of that pair type.

Here is a simple example. `AnyDrop` stores a value of some unknown type, plus a
small “vtable” for dropping it (a boxed [choice](./choice.md)):

```par
type AnyDrop = <a>(a) box choice {
  .drop(a) => !,
}
```

This is the implicit counterpart of the existential type:

```par
type DropMe = (type a) (a) box choice {
  .drop(a) => !,
}
```

### Construction

Notice that you do not specify the hidden type when constructing an `AnyDrop`:

```par
def Example: AnyDrop = (7) box case {
  .drop(_) => !,
}
```

### Destruction

Dually, when you unpack an implicit-generic pair, you introduce a local type
variable:

```par
let <a>(value) vtable = Example
vtable.drop(value)
```

This is the “mirror image” of implicit-generic functions: there you introduce
`<a, ...>` at construction time and inference happens at call time; here you
construct without naming `a`, and introduce `a` when unpacking.
