# Par: Easy Concurrency with Linear Logic

[![Discord](https://img.shields.io/discord/1336795503634939977)](https://discord.gg/8KsypefW99)
[![Discussion](https://img.shields.io/github/discussions/faiface/par-lang)](https://github.com/faiface/par-lang/discussions)
[![Sponsor](https://img.shields.io/github/sponsors/faiface)](https://github.com/sponsors/faiface)

![Screenshot](screenshots/playground.png)

Par is a new experimental programming language with the long-term goal of bringing classical linear logic into the mainstream. There are few languages like it right now, and we embrace this with syntax that doesn't look like other languages. Par's syntax focuses on giving the right intuition, and hopefully you'll find that the way it works is actually very simple to grasp.

To install Par, `git clone` this repository, `cd` into it, and `cargo run`.

### The Basics

In most languages, we might start the tour with a "hello world" program, some arithmetic, or a fibonacci program. However, Par being experimental, it doesn't yet have any of those features! The language focuses on getting the research parts correct first, namely the concurrency based on linear logic.

We'll nevertheless start by looking at the simplest possible program in Par:

```
dec x : !
def x = !
```

This is a program that simply stops, but there are still a couple things we should unpack here. First, notice that `dec` declares the type of a value, using `:`, and `def` defines the value itself, using `=`. 

Second, the way we talk about types in Par is completely separated from how we talk about values. That means they can reuse each other's syntax, so `!` is a value whose type is also called `!`. As you might expect, `!` is the *only* value of this type, and doing nothing and providing no information.

Lastly, Par programs don't have a specific "entrypoint" like a `main` function in other languages. Instead, if you write this program in the playground, the `run` button lets you choose a definition to execute, such as `x`.

Let's move on to something a little more interesting: functions!

```
dec foo : [!] !
def foo = [arg] arg
```

We're once again reusing syntax for the values and their types. On the "type-level," `[a] b` is the type of a function taking an argument of type `a` and returning an argument of type `b`. So, as you might imagine, on the "value-leve" `[x] body` is an anonymous function (also known as a lambda) with a parameter named `x` that can be used in the `body`. Let's try calling our function:

```
dec x : !
def x = !

dec foo : [!] !
def foo = [arg] arg

dec main : !
def main = foo(x)
```

Calling functions will hopefully look familiar! If a function `f` has type `[a] b`, and `x` has type `a`, then you can call `f` like `f(x)`, which has type `b`.

If you want a function multiple parameters in Par, you "curry" it, writing something like `[x] [y] body` with a type that looks something like `[a] [b] c`. Then you might call it like `func(arg1)(arg2)`.

One way that Par differs *a lot* from other languages, though, is that it's *linear.* This means every function parameter must be used exactly once. The following function is NOT a valid Par program:

```
dec invalid : [!] [!] !
def invalid = [x] [y] x
```
```
2| def invalid = [x] [y] x
                         ^
Cannot end this process before handling `y`.
```

The fact that the program ends without mentioning `y` makes it invalid, because every function parameter must be used exactly once. I'm not going to pretend there's a loophole: this function is simply impossible in Par. Therefore linearity is really a new *paradigm* of software development, since programs must be structured around this restriction, very much like how functional programming has its own idioms for working around purity. Here at Par, we claim that the benefits of linearity outway their costs. There will be more said on these benefits later.

Now, of course, we keep using this uninformative `!` type, so we're not really computing anything of interest. Let's try making a more interesting type, so we can do more interesting computation:

```
type Bool = either {
    .true!
    .false!
}
```

`type` declares a new type synonym, `either` defines an either type, known in other languages as tagged unions, sum types, loaded enums, etc. 

The "variants" or "constructors" need to start with `.` for reasons that will be more clear later. They also must take an argument, so we say that their parameter type is `!`, the type with no information, as a way of pretending that they don't take an argument. We use the constructors in the way you'd expect:

```
dec true : Bool
def true = .true!
```

The linearity restriction only applies to function parameters: global definitions, whether they're global variables or either-type constructors, can be used as many times as we like.

Now, introducing either-typed values (using constructors) is only half the story. Given an either-typed value, we need to be able to know which constructor was used to construct it, by "pattern-matching:"

```
dec destruct_bool : [Bool] !
def destruct_bool = [b] b {
    .true! => !
    .false! => !
}
```

Par doesn't have a `case`, `match`, or `switch` keyword for this. Instead you simply put the cases, wrapped in curly braces, after the value. In this case, that value is `b`.

Notice that if you run `destruct_bool` in the playground, a GUI widget appears, letting you specify an input for it (either `true` or `false`). For this function nothing really happens when you pick one, because our function doesn't actually do anything. So let's finally write a function that computes something:

```
dec negate : [Bool] Bool
def negate = [b] b {
    .true! => .false!
    .false! => .true!
}
```

Running *this* in the playground shows the negation of what you click. Pretty cool!

todo: recursive and corecursive types

### Concurrency

todo
- `char k { ... }`
- `k <> x`
- duality
- type-level negation

### It was concurrency all along!

todo
- linear logic
- desugaring/process-syntax