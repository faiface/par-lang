# Statements

Statements are used in **process syntax** to
- destruct values: [_Command_]
- bind values: [_LetStatement_]

Process syntax is a series of statements, where all are nonterminating. In a `channel` expression the last one will be terminating, making it an exception.

Process syntax is introduced by the _Proces_ Rule:

> **<sup>Syntax</sup>**\
> _Process_ : (_Statement_ (`;`<sup>?</sup> _Statement_) (`;`<sup>?</sup> _TerminatingStatement_)<sup>?</sup>)<sup>?</sup>
>
> _Statement_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Command_] \
> &nbsp;&nbsp; | [_LetStatement_]
>
> _TerminatingStatement_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_TerminatingCommand_]

It is used in the following places:
- [do expressions](./expressions.md#do-expressions):
  ```par
  let x = do { process } in value
  ```
  The process here may not use terminating statements.

- [channel expressions](expressions.md#channel-expressions):
  ```par
  let x = chan dual { process }
  ```
  The process here must use a terminating statement. It constructs `x` by destructing its dual, `dual`.



## Let Statements

> **<sup>Syntax</sup>**\
> _LetStatement_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `let` [_Pattern_] `=` [_Expression_]

Let statements are used to create values in processes. They are the only constructive statements, as commands always destruct.

In `do` expressions, they are used to bind the values used in the value after `in`:
```par
let true_and_false = do {
  let x: Bool = .true!
  let y: Bool = .false!
} in (x, y)!
```

In `chan` expressions, they can be used to construct a value that is then linked with a value of dual type:
```par
def just_true = chan return: chan Bool {
  // constructing the return value
  let b: Bool = .true!
  // linking it
  return <> b
}
// is equivalent to
def just_true = chan return: chan Bool {
  // destructing the result
  return.true!
}
```

[_Command_]: ./statements/commands.md
[_LetStatement_]: #let-statements
[_Pattern_]: patterns.md
[_Expression_]: expressions.md
[_TerminatingCommand_]: ./statements/commands.md
[_Type_]: types.md