# Par: Simple Concurrency with Linear Logic

[![Discord](https://img.shields.io/discord/1336795503634939977)](https://discord.gg/8KsypefW99)
[![Discussion](https://img.shields.io/github/discussions/faiface/par-lang)](https://github.com/faiface/par-lang/discussions)
[![Sponsor](https://img.shields.io/github/sponsors/faiface)](https://github.com/sponsors/faiface)

![Screenshot](screenshots/playground.png)

Par is a new experimental programming language with the long-term goal of bringing recent powerful developments in programming language theory into the mainstream. There are few languages like it right now, and we embrace this with syntax that doesn't look like other languages. Par's syntax focuses on giving the right intuition, and hopefully you'll find that the way it works is actually very simple to grasp. 

To be more specific, the theory that powers Par finds the fundamental mechanics underlying familiar programming concepts, and the syntax is designed for programming with these more powerful tools. Then we can build all the familiar concepts ourselves, but they just look a little different, reflecting how they were built.

To install Par, `git clone` this repository, `cd` into it, and `cargo run`.

### The Basics

In most languages, we might start the tour with a "hello world" program, some arithmetic, or a fibonacci program. However, Par being experimental, it doesn't yet have any of those features! The language focuses on getting the research parts correct first, namely the concurrency based on linear logic.

We'll nevertheless start by looking at the simplest possible program in Par:

```
dec x : !
def x = !
```

`!` is the "unit type" in Par, sometimes written in other languages as `()`. As usual, it is a type with one value, and the type and value are written the same way: in Par, they're both written `!`. A unit value has no information and does nothing; this is a program that simply stops. `dec` declares the type of a top-level definition, and `def` gives the definition in code.

Par programs don't have a specific "entrypoint" like a `main` function in other languages. Instead, if you write this program in the playground, the `run` button lets you *choose* a definition to execute, such as `x`.

Let's try making a more interesting type, so we can do more interesting computation:

```
type Bool = either {
    .true !
    .false !
}
```

Either-types are known in other languages as tagged unions, sum types, loaded enums, etc. 

The constructors of an either-type must take an argument, but `.true` and `.false` don't really need any extra information, so we use the "no-information" type `!`. In the case of `!`, we usually don't put a space before it. For example, here's how we could use `.true` in our code:

```
dec my_true : Bool
def my_true = .true!
```

Given a value of an either-type, we can "pattern match" to decide what to do based on how it was constructed:

```
dec not_my_true : Bool
def not_my_true = my_true {
    .true! => .false!
    .false! => .true!
}
```

Pattern-matching on an either-type is actually using a "choice-type." A choice type offers you a set of alternatives you can choose from, like the lever in the Trolley Problem, deciding which path to to go down next. These can be written on their own, without pattern-matching anything:

```
type MyChoice = {
    .true! => Bool
    .false! => Bool
}

dec my_choice : MyChoice
def my_choice = {
    .true! => .false!
    .false! => .true!
}
```

Then we use an either-type constructor to choose the path like so:

```
dec the_chosen_one : Bool
def the_chosen_one = my_choice.false!
// note: the_chosen_one is .true! because that's how it responds to the .false! choice
```

This might make the dot in either-type constructors make a bit more sense, suddenly :)

When given a value of a choice-type, you get to choose which route to take. So when constructing such a value, you have to be prepared for every route which might be asked of you. It's the opposite of an either-type, where the constructor chooses the alternative, and using the value means being prepared for every possibility. That's why we have the constructor of an either-type choose the alternative of a choice!

Now let's talk about functions in Par. As usual, the types are written the same as the values. Here's a function that just returns its argument:

```
dec same_bool : [Bool] Bool
def same_bool = [b] b
```

The parameter goes in the square brackets, and anything that follows is the body of the function. To all a function, Par uses the familiar syntax:

```
dec same_true : Bool
def same_true = same_bool(.true!)
```

For the (rare) need to control the length of a function body, Par uses `{...}` instead of parentheses like other languages:

```
dec same_true2 : Bool
def same_true2 = { [b] b }(.true!)
```

We can combine what we know to make a program that's slightly more interesting than what we've done so far:

```
dec negate : [Bool] Bool
def negate = [b] b {
    .true! => .false!
    .false! => .true!
}
```

Running *this* in the playground shows the negation of what you click. Pretty cool!

One way that Par differs *a lot* from other languages, though, is that it's *linear.* This means every function parameter must be used exactly once. The following function is NOT a valid Par program:

```
dec invalid : [Bool] [Bool] Bool
def invalid = [x] [y] x
```
```
2| def invalid = [x] [y] x
                         ^
Cannot end this process before handling `y`.
```

The fact that the program ends without mentioning `y` makes it invalid, because every function parameter must be used exactly once. I'm not going to pretend there's a loophole: this function is simply impossible in Par. Therefore linearity is really a new *paradigm* of software development, since programs must be structured around this restriction, very much like how functional programming has its own idioms for working around purity. Here at Par, we claim that the benefits of linearity outway their costs. There will be more said on these benefits later.

todo: universal, existential, recursive, and corecursive types

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