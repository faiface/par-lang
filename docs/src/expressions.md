# Expressions

> **<sup>Syntax</sup>**\
> _Expression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Construction_] \
> &nbsp;&nbsp; | [_Application_] \
> &nbsp;&nbsp; | [_LetExpression_] \
> &nbsp;&nbsp; | [_DoExpression_] \
> &nbsp;&nbsp; | [_ChanExpression_]

Some expressions are evaluated strictly, some lazily. todo: which

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

## Let Expressions

> **<sup>Syntax</sup>**\
> _LetExpression_ : `let` [_Pattern_] `=` _Expression_ `in` _Expression_

The second expression must use all bindings of the pattern (which must be irrefutable) due to linearity.

## Do Expressions

> **<sup>Syntax</sup>**\
> _DoExpression_ : `do` `{` [_Process_] `}` `in` _Expression_

Do expressions are syntax sugar for channel expressions (todo: specify how). Linearity requires that all leftover bindings from the process must be used in the expression at the end.

## Channel Expressions

> **<sup>Syntax</sup>**\
> _ChanExpression_ : `chan` [ID] [_Annotation_]<sup>?</sup> `{` [_Process_] `}`

The name declared after the channel may be used inside the process and must be fully destructed there.

The expression `chan a: A { ... }` has type `chan A`. Conversely, if `chan b { ... }` has type `B`, `b` has type `chan B`.

[_Pattern_]: ./patterns.md