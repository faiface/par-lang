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
