# Basic Program Structure

Before we start writing our own package, it's helpful to see Par in action in the playground.

Let's open it:

```
$ par playground
```

Press **Compile**, then open the **Run** menu and pick a built-in definition from `core`.

<!-- TODO(screenshot): add an updated playground screenshot showing the Run menu on an empty program. -->

As a first example, choose one of the built-in numeric helpers from the `core` package. `Int.Mod`
computes the non-negative remainder of an integer modulo a natural number:

<!-- TODO(screenshot): add an updated playground screenshot showing Run -> Packages -> core -> Int -> Mod. -->

An **automatic UI** shows up, telling us to input the arguments expected by the selected definition.
After confirming them, we get a result:

<!-- TODO(screenshot): add an updated playground screenshot showing the result UI for Int.Mod. -->

This automatic UI is a feature of the **playground,** not of the Par language itself. Nobody made a
specific interface for `Int.Mod`. Instead, the playground looked at its type — here, a function from
two numbers to a number result — and generated a small interface for interacting with it.

That makes the playground a nice way to explore built-in definitions, and later your own definitions
too.

On the next page, we'll create a fresh package and look at what actually lives inside a Par module.
