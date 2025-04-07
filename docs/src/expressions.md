# Expressions

> **<sup>Syntax</sup>**\
> _Expression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Construction_] \
> &nbsp;&nbsp; | [_Application_] \
> &nbsp;&nbsp; | [_LetExpression_](#let-expressions) \
> &nbsp;&nbsp; | [_DoExpression_](#do-expressions) \
> &nbsp;&nbsp; | [_ChanExpression_](#channel-expressions)

Every expression desugars to a [channel expression](#channel-expressions). In the most simple way, that is
```par
expr
// is equivalent to
chan dual {
  dual <> expr
}
```
This can now be further translated into process syntax by rewriting the [link](./statements/commands.md#link-commands) `dual <> expr`. Rules for that are provided in every expression rule.

## Primary Expressions

> **<sup>Syntax</sup>**\
> _PrimaryExpression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [ID] \
> &nbsp;&nbsp; | _GroupedExpression_
>
> _GroupedExpression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `{` _Expression_ `}`

Par uses `{` and `}` for grouping expressions together.
Together with names like `x` they form the primary expressions, which can be [destructed](./expressions/application.md) using expressions.

Primary expressions appear [here](./expressions/application.md) in the grammar.

Examples:
```par
dec apply_id : [type T] [T] T
def apply_id = [type T] [x] {[y] y}(x)
```

Primary expressions can be linked via:
```par
dual <> id // can't be further simplified

dual <> {expr}
// is just
dual <> expr
```

## Let Expressions

> **<sup>Syntax</sup>**\
> _LetExpression_ : `let` [_Pattern_] `=` _Expression_ `in` _Expression_

The second expression must use all bindings of the pattern (which must be [irrefutable](patterns.md#irrefutable-note)) due to linearity.

Let expressions make code easier to read by creating an expression in multiple steps. Compare:
```par
let x = call_a_long_function(and_another_one(y))
in (.true!, x, .false!)!
// with
(.true!, call_a_long_function(and_another_one(y)), .false!)!
```
But arguably even morer useful are the destruction-by-pattern capabilities let expressions offer:
```par
// this expression is not possible
// without let or do expressions
let (b1, b2)! = copy_bool(b)
in (b1, not(b2))!
```
[Let statements](./statements.md#let-statements) are very similar, so when multiple let expressions in a row would be required, it's often more convenient to switch to those and put them inside [do expressions](#do-expressions).

Let expressions can be linked via:
```par
dual <> let p = x in y
// is equivalent to
let p = x
dual <> y
```

## Do Expressions

> **<sup>Syntax</sup>**\
> _DoExpression_ : `do` `{` [_Process_] `}` `in` _Expression_

Do expressions allow [process syntax](statements.md) inside an expression context:
```par
do { 
  commands... 
} in result
// is equivalent to
chan return {
  commands...
  return <> result
}
```
Linearity requires that all leftover bindings from the process must be used in the expression at the end.

Do expressions are useful for...
```par
// ...binding multiple values with let
do {
  let v1 = e1
  ...
  let vn = en
} in result
// is preferred over
let v1 = e1 in
...
let vn = en in
result

// ...destructing values
do {
  drop(x1)?
  drop(x2)?
} in result
```
Expressions construct, commands destruct. 
Because of this, when a value is destructed, process syntax (for example via a do expression) is the way to go.

Do expressions can be linked via:
```par
dual <> do { proc } in y
// is equivalent to
proc
dual <> y
```

## Channel Expressions

> **<sup>Syntax</sup>**\
> _ChanExpression_ : `chan` [ID] [_Annotation_]<sup>?</sup> `{` [_Process_] `}`

The name declared after the channel may be used inside the process and must be fully destructed inside or moved out of there.

The expression `chan a: A { ... }` has type `chan A`. Conversely, if `chan b { ... }` has type `B`, `b` has type `chan B`.

Note, that `chan A`, the [channel type](./types.md#channel-types), is not a separate type. It merely transforms the `A` to its dual, according to the [duality equations](./types.md#duality-equations).

These expressions are key to exploiting duality. The expression `chan c { ... }` spawns a process, and at the same time creates a channel, whose one end is accessible as `c` inside the process, and the other end is returned from the expression. The types of these are dual.

Channel expressions are the only expression which is not syntax sugar. Under the hood, all expressions are syntax sugar for channel expressions.

Par even has an intermediate representation in which all expressions are channel expressions.

A channel expression constructs a value by destructing a value of its dual. For example:
```par
dec is_even : [Nat] Bool
def is_even = chan return: (Nat) chan Bool {
  // destruct return in this process

  return[n]
  // return is now of type chan Bool
  // and n is of type Nat

  // destruct n
  n begin {
    .zero! => {
      // fully destruct return
      return.true!
    }
    .succ => {
      // n is now its former predecessor
      n {
        .zero! => {
          // n was 1
          return.false!
        }
        .succ => {
          n loop
        }
      }
    }
  }
}
```
Learn more about destructing values using commands [here](./statements/commands.md).

A more elaborate example is reversing a list using the generator pattern:
```par
dec reverse : [type T] [List<T>] List<T>

// We construct the reversed list by destructing its dual: `chan List<T>`.
def reverse = [type T] [list] chan yield {
  let yield: chan List<T> = list begin {
    // The list is empty, give back the generator handle.
    .empty!       => yield,
    // The list starts with an item `x`.
    .item(x) rest => do {
      // Traverse into the rest of the list first.            
      let yield = rest loop
      // After that, produce `x` on the reversed list.          
      yield.item(x)                  
    } in yield // Finally, give back the generator handle.
  }
  // At the very end, signal the end of the list.
  yield.empty!                       
}
```

[_Pattern_]: ./patterns.md
[_Construction_]: ./expressions/construction.md
[_Application_]: ./expressions/application.md
[ID]: ./lexical.md#names
[_Process_]: ./statements.md
[_Annotation_]: ./types.md
