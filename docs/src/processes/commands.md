# Commands

A process is a sequence of commands, and `let` statements. Now that we've covered
[`let` statements](./do_expression.md#the-let-statement), it's time to look at commands. We'll do
that by using them in small, but realistic examples.

First, **what even is a command?** It's similar to a statement in imperative programming, but there
is an important difference. In imperative programming, a statement is usually an expression
that's evaluated not for its result, but for its side-effects. In contrast, commands in Par are
_not_ expressions, they are a distinct syntactic category.

Every command has a **subject,** a channel — usually a local variable — that the command is
operating on. After executing the command, the subject **changes its type.** This is the important
distinction from imperative statements. It's the distinction that brings proper, and ergonomic
session typing to this imperative-looking process syntax.

**Let's see that in action!**

## Sending to a `String.Builder`

The built-in `String.Builder` type is defined this way:

```par
type String.Builder = iterative choice {
  .add(String) => self,
  .build => String,
}
```

It's basically an object, in an OOP-fashion, with two methods: `.add` and `.build`. At the top
level, it's an [iterative](../types/iterative.md) [choice](../types/choice.md): an object that
can be repeatedly interacted with.

We construct an empty `String.Builder` using the built-in definition of the same name:

```par
def LetsBuildStrings = do {
  let builder = String.Builder
```

### Selection

Now, we have a local variable `builder` of the type `String.Builder`.

When learning about [iterative](../types/iterative.md) types, we learned that we can treat them
as their underlying body. For `String.Builder`, it's a **choice type,** and the command for those is
the **selection command.**

All commands start with their subject. Here it's the `builder` variable. The selection command
itself then looks the same as the usual [destruction of a choice](../types/choice.md#destruction).

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder.add        // selection command
```

That's it! Now, here's the crucial bit: after selection, `builder` **changes its type** to the type
of the selected branch. Here's the one we selected:

```par
  .add(String) => self,
```

The argument on the left side of `=>` is just a syntax sugar for a [function](../types/function.md).
De-sugared, it is:

```par
  .add => [String] self,
```

Therefore, the type of `builder` after this `builder.add` command becomes a function:

```par
builder: [String] iterative choice {
  .add(String) => self,
  .build => String,
}
```

> The `self` in the original branch got replaced by its corresponding `iterative` — in
> this case, the original `String.Builder`.

### Sending

For a **function type,** we have the **send command.** It's just like a function call — but
as a command, it doesn't have a result. Instead, it turns the subject itself to the result.

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder.add        // selection command
  builder("Hello")   // send command
```

> You may have noticed, that the **selection** and **send** commands behave the same as a
> combination of a regular destruction and re-assignment of the variable. In code:
>
> ```par
>   let builder = builder.add
>   let builder = builder("Hello")
> ```
>
> For these two, the behavior matches perfectly! It's a good way to build intuition about what
> these commands mean. However, this simple translation stops working as we get into the _`.case`_
> and _receive_ commands.

After sending the string, `builder` turns back into the original `String.Builder`, so we can
keep adding more content to it.

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder.add        // selection command
  builder("Hello")   // send command
  builder.add        // selection command
  builder(", ")      // send command
  builder.add        // selection command
  builder("World")   // send command
  builder.add        // selection command
  builder("!")       // send command
```

### Chaining commands

This is rather noisy, but we can improve it! Multiple consecutive **commands on the same subject,**
can be **chained** together, without repeating the subject.

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder.add("Hello")
  builder.add(", ")
  builder.add("World")
  builder.add("!")
```

Or even:

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder
    .add("Hello")
    .add(", ")
    .add("World")
    .add("!")
```

I like the first variant better, though.

To complete the `do` expression, let's just return the constructed string:

```par
def LetsBuildStrings = do {
  let builder = String.Builder
  builder.add("Hello")
  builder.add(", ")
  builder.add("World")
  builder.add("!")
} in builder.build  // = "Hello, world!"
```

## Receiving from a `List<String>`
