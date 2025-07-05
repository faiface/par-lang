# Function

A function transforms an argument into a result. The syntax for function types is designed to work well
with the rest of the type system, and resembles the syntax for [pairs](./pair.md), because the two are
[dual](../processes/duality.md) to one another.

A function type consists of two types — the argument, and the result — the former enclosed in square
brackets.

```par
type Function = [Int] String
```

If the result is another function, we can use _syntax sugar_ to write it more concisely:

```par
type BinaryFunction1 = [Int] [Int] Int
type BinaryFunction2 = [Int, Int] Int
// these two are exactly the same type
```

This is the preferred way to define functions of multiple arguments.

**Functions are [linear](TODO).** While a globally defined function may be called any number of times,
a function stored in a local variable can (and must) only be called once:

```par
dec Add : [Int, Int] Int
def Add = [x, y] Int.Add(x, y)

// a global function may be called many times
def Six = Add(1, Add(2, 3))  // Okay.

// but a function in a local variable can be only called once
def Illegal =
  let inc = Add(1)
  in Add(inc(2), inc(3))       // Error!
```

[Linearity](TODO) brings a lot of expressivity that wouldn't be possible otherwise. After all, the main
purpose of _Par_ is to explore where this new paradigm arising from linear types and duality can take us.

> **Non-linear functions are a planned feature,** but in the meanwhile, the same can be achieved by a
> combination of [iterative](./iterative.md) and [choice](./choice.md) types. Here's an example, utilizing
> the full syntax of the language. If you're reading in this documentation in order, **come back here** after
> reading up on:
> - [Choice](./choice.md) types.
> - [Iterative](./iterative.md) types.
> - [Forall](./forall.md) types (generics).
> - [`do`-expressions](../processes/do_expression.md).
> 
> Or play with it regardless.
> 
> ```par
> type Mapper<a, b> = iterative choice {
>   .close    => !,
>   .apply(a) => (b) self,
> }
> 
> dec MapList : [type a, b] [List<a>, Mapper<a, b>] List<b>
> def MapList = [type a, b] [list, mapper] list.begin.case {
>   .end! => do {
>     mapper.close
>   } in .end!,
> 
>   .item(x) xs => do {
>     mapper.apply(x)[y]
>   } in .item(y) xs.loop
> }
> 
> // Strings = *("1", "2", "3", "4", "5")
> def Strings =
>   let numbers: List<Int> = *(1, 2, 3, 4, 5)
>   in MapList(type Int, String)(numbers, begin case {
>     .close    => !,
>     .apply(n) => (Int.ToString(n)) loop,
>   })
> ```

## Construction

Function values bind their argument inside square brackets, followed by an expression computing
the result.

```par
dec Double : [Int] Int
def Double = [number] Int.Mul(2, number)
```

Multi-argument functions — or more precisely: functions returning other functions — can be expressed
using the same kind of a syntax sugar as available for their types:

```par
dec Concat : [String, String] String
// the same as `[String] [String] String`

def Concat = [left, right]
  String.Builder.add(left).add(right).build
```

Patterns for deconstructing [pairs](./pair.md) and [units](./unit.md) can be used inside the
square brackets:

```par
dec Swap : [(String, Int)!] (Int, String)!
def Swap = [(x, y)!] (y, x)!
```

Par uses [bi-directional type-checking](https://arxiv.org/abs/1908.05839). It's a style of type-checking
that can infer a lot of types, but does not try to guess ahead. Functions are one of the types that it
cannot fully infer.

```par
def Identity = [x] x  // Error! The type of `x` must be known.
```

If the type of a function isn't known ahead of time, at least the type of its argument must be specified
explicitly:

```par
def Identity = [x: String] x  // Okay.
```

For **generic functions**, read up on [_forall_ types](./forall.md).

> **_Par_ has an unusual take on recursion,** thanks to its ambitious stride towards [totality](TODO).
> Naive recursion by self-reference is not allowed. In other words,
> **a function can't directly call itself.**
> 
> ```par
> def Infinity = Int.Add(1, Infinity)  // Error! Cyclic dependency.
> ```
> 
> Instead, [recursive](./recursive.md) and [iterative](./iterative.md) types are used for recursion
> and [corecursion](https://en.wikipedia.org/wiki/Corecursion), respectively. Read up on them to learn
> more.
> 
> Par's powerful `begin`/`loop` syntax is a single, universal construct for cyclic computations.
> It serves well in recursive functions, [iterative](./iterative.md) objects, and imperative-looking loops
> in [process syntax](../processes.md).
> 
> Forbidding functions from calling themselves may seem limiting at first, but `begin`/`loop` makes up
> for it with its perky handling of local variables, and its ability to be used deep in expressions,
> removing any need for recursive helper functions.

## Destruction

Calling a function has the familiar syntax:

```par
def Ten = Double(5)  // `Double` defined above
```

Functions with multiple arguments may be called by comma-separating the arguments inside
the parentheses:

```par
def HelloWorld1 = Concat("Hello ", "World")  // `Concat` defined above
def HelloWorld2 = Concat("Hello ")("World")

def HelloWorld3 =
  let partial = Concat("Hello ")
  in partial("World")
```

All three versions do the same thing.

The word _destruction_ is especially apt here, due to [linearity](TODO) of functions. If a function
is stored in a local variable, calling it destroys the variable, as discussed above.
