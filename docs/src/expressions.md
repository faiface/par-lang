# Expressions

<!--"expression"
  ::=& #raw("let") "pattern" #raw("=") "expression" #raw("in") "expression" \
  |& #raw("do") #raw("{") "process"^? #raw("}") #raw("in") "expression" \
  |& #raw("chan") "id" #raw("{") "process"^? #raw("}") "[annotation?]" \
  //"construction" \
  |& #raw("(") "expression" #raw(")") "expression" comment("construct" tensor"-type") \
  |& #raw("[") "pattern" #raw("]") "expression" comment("construct" lolli"-type") \
  |& #raw(".") "id" "expression" comment("construct variant of" oplus"-type") \
  |& #raw("{") (#raw(".") "id" (#raw("(") "pattern "#raw(")"))^* #raw("=>") "expression")^* #raw("}") comment("construct" with"-type") \
  |& #raw("!") comment(unit) \
  |& #raw("begin") "loop-label"^? "expression" comment("place comefrom") \
  |& #raw("loop") "loop-label"^? comment("goto") \
  |& #raw("(") #raw("<") "type" #raw(">") #raw(")") "expression" comment("construct" exists"-type") \
  |& #raw("[") #raw("<") "type" #raw(">") #raw("]") "expression" comment("construct" forall"-type") \
  |& #raw("&") "expression" comment("derelict: construct" !"-type") \
  |& "tag" "expression" comment("add tag") \
  |& "primary-expression" "suffix"^?-->

> **<sup>Syntax</sup>**\
> _Expression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `let` [_Pattern_] `=` _Expression_ `in` _Expression_ \
> &nbsp;&nbsp; | ... \
> &nbsp;&nbsp; | `(` _Expression_ `)` _Expression_ \
> &nbsp;&nbsp; | ...

A `let` expression exposes the bindings of the pattern in the expression after `in`.

```par
y = let x = 5 in x * x // 25
```

\\[
\frac{p:A^\perp \quad x:A \quad y\\{p/x\\}:B}{\texttt{let $p$ = $x$ in $y$}:B}
\\]


---

## Tuple Expressions

A tuple expression is of the form `(expr) expr`:
```par
pair = (1) 0
```
It constructs a value of a \\( \otimes \\)-type (a [tuple type](./types/tuples.md))

\\[
\frac{x:A \quad y:B}{\texttt{($x$) $y$}:A \otimes B}
\\]

```par
x : A
y : B

pair : (A) B
pair = (x) y
```

---

...

[_Pattern_]: ./patterns.md