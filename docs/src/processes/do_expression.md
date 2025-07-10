# The `do` Expression

Most of the time, it's not desirable to use _process syntax_ for the whole program. Instead, it's usually best to
just insert some commands where appropriate. That's what the `do` expression is all about. In real Par programs,
**most of the explicit commands will be found in `do` expressions.** And, since we're already familiar with
_expression syntax_, `do` expressions are a good place to start adding commands to otherwise expression-based programs.

A `do` expression starts with the keyword `do`, then a sequence of commands (without separators) enclosed in
curly braces, followed by the keyword `in`, and finally the resulting expression.

It executes the commands first, then evaluates to the expression after `in`.

```par
def MyName: String = do { } in "Michal"
```

The above `do` expression contains no commands, so its result is simply `"Michal"`.

## The `let` Statement

Before getting onto actual commands — those that manipulate channels — there is one non-command that can occur in
a process: the `let` statement. Just like the [`let`/`in` expression](../structure/let_expressions.md), it assigns
a variable. The only difference is: the `let` statement doesn't contain the `in` keyword. And, since it's a process,
there can be more of them one after another.

```par
dec DisplayPlusEquation : [Nat, Nat] String
def DisplayPlusEquation = [a, b] do {
  let c = Nat.Add(a, b)
  let a = Int.ToString(a)
  let b = Int.ToString(b)
  let c = Int.ToString(c)
} in String.Builder.add(a).add("+").add(b).add("=").add(c).build

def Test = DisplayPlusEquation(3, 4)  // = "3+4=7"
```

This is, in fact, the **idiomatic way to assign multiple variables in an expression.**
