# Let Expressions

> **<sup>Syntax</sup>**\
> _LetExpression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `let` [_Pattern_] `=` [_Expression_] `in` [_Expression_]

Exposes the bindings of the pattern in the expression after `in`.

```par
y = let x = 5 in x * x // 25
```

[_Expression_]: ../expressions.md
[_Pattern_]: ../patterns.md