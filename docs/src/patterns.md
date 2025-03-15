# Patterns

> **<sup>Syntax</sup>**\
> _Pattern_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _PatternNoAlt_ <!--\
> &nbsp;&nbsp; | [_AltPattern_](#alternatives)-->
>
> _PatternNoAlt_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_BindingPattern_](#binding-patterns) \
> &nbsp;&nbsp; | [_Unit_](#the-unit-pattern) \
> &nbsp;&nbsp; | [_PairPattern_](#pair-patterns) <!--\
> &nbsp;&nbsp; | [_VariantPattern_](#variant-patterns) \
> &nbsp;&nbsp; | [_BlankPattern_](#the-blank-pattern) -->\
> &nbsp;&nbsp; | [_GroupedPattern_](#grouped-patterns) \
> &nbsp;&nbsp; | [_ExistentialPattern_](#existential-patterns)
>
> _PatternList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _PatternNoAlt_ (`,` _PatternNoAlt_)<sup>*</sup> `,`<sup>?</sup>

There are two properties of a pattern: What names it *binds* and whether it's *refutable*.

Patterns can appear in several places:
- in a `let` binding:
  ```par
  // as an expression
  let p = x in y

  // or in process syntax
  let p = x
  rest...
  ```
  All bindings of `p` must be used in `y` or `rest...` respectively.
  Here, `p` must be irrefutable.

- between `[` and `]`
  ```par
  // in a function expression
  [p] body

  // destructing a pair (process syntax)
  tail[p]
  rest...
  ```
  All bindings of `p` must be used in `body` or `rest...` respectively.
  Here, `p` must also be irrefutable.

<!-- - in pattern matching, i.e. either destruction
  ```par
  x {
    p1 => y1,
    p2 => y2,
    ...
  }
  ```
  Here, the patterns `p1`, `p2`, ... don't have to be irrefutable. All bindings of a pattern `p` must be used in the corresponding branch `y` though.

  The matching must be *exhaustive*, i.e. every possible value of `x` must be matched in some branch. If multiple branches would match `x`, the first one is used.-->
- in pattern matching, i.e. either destruction/choice construction
  ```par
  // either destruction
  x {
    .label (p) rest_payload => y
    ...
  }
  // in process syntax, the rest_payload isn't present
  // (instead, x becomes it)

  // choice construction
  {
    .label (p) => y
    ...
  }
  ```
  All bindings of `p` must be used in `y`.
  Here, `p` must also be irrefutable.

## Binding Patterns

> **<sup>Syntax</sup>**\
> _BindingPattern_ : [ID] [_Annotation_]<sup>?</sup> <!--(`@` _PatternNoAlt_)<sup>?</sup>-->

<!--The optional `@` and what comes after is called a *subpattern*.

If no subpattern is present, the pattern always matches and binds to its name:-->

The pattern `name` or `name: T` is always irrefutable and binds `name` to `x` when matching `x`.

The pattern `name` can be used on a value which type is known. Otherwise, `name: T` must be used, which can only be used on a value of type `T`.

```par
let x = y in x
// is just
y

// it is most often used for function expressions
let lambda = [x] expr_using_x

// or the similar pair destructions
let tuple: (A, B, C)! = (a, b, c)!
tuple[first]
// now first is a
// and tuple is (b, c)!
```

If the type of the value matched is already known, a type annotation would have to match it.
Such an annotation can be useful for declaring functions, though:
```par
// no extra type annotation/declaration needed
def negate = [b: Bool] b {
  .true! => .false!
  .false! => .true!
}
```

<!--If a subpattern is present, however, `name @ subp` matches exactly when `subp` matches. Generally, it is refutable. When matching `x`, it binds `name` to `x`. All bindings of `subp` are consumed by `name`, i.e. they can't be used anymore. For that reason, the [blank pattern](#the-blank-pattern) `_` may be used inside `subp`.
```par
// example
```-->

## The Unit Pattern

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

*<sup>
[Type](types.md#the-unit-type)
| [Constructing Expression](./expressions/construction.md#the-unit-expression)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

The unit pattern `!` is always irrefutable and binds nothing.
It can only be used on values of type `!`.

```par
// if there is a value of a type T,
// there is a function [!] T
def returns_true: [!] Bool = [!] .true!

// ! can be used to destroy a unit
def drop_two_bools: [Bool, Bool] ! = 
  [b1, b2] let ! = drop(b1) in drop(b2)

// though process syntax is generally used for this
def drop_two_bools: [Bool, Bool] ! = [b1, b2] do {
  drop(b1)?
  drop(b2)?
} in !

dec drop_bool : [Bool] ! 
?def drop_bool = [b] {
?  .true! => !
?  .false! => !
?}
```

## Pair Patterns

> **<sup>Syntax</sup>**\
> _PairPattern_ : `(` _PatternList_ `)` _Pattern_

*<sup>
[Type](types.md#pair-types)
| [Constructing Expression](./expressions/construction.md#pair-expressions)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

- A pair pattern `(p) q` is irrefutable if and only if both `p` and `q` are irrefutable
- When `p` can be used on type `A` and `q` on type `B`, `(p) q` can be used on type `(A) B`
- `(p) q` matches `(a) b` if and only if `p` matches `a` and `q` matches `b`
- When matching `(a) b`, the bindings are those of `p` matching `a`, together with those of `q` matching `b`

Having multiple patterns between `(` and `)` is just syntax sugar:
```par
// the pattern
(p, q) r
// is equivalent to
(p) (q) r
```

A pair pattern is used to destruct a value of a [pair type](types.md#pair-types):
```par
dec uncurry : [type A, B, C] [[A, B] C] [(A, B)!] C
def uncurry = [type A, B, C] [f] [(a, b)!] f(a, b)
```


<!--## Variant Patterns

> **<sup>Syntax</sup>**\
> _VariantPattern_ : [_Label_](types.md) _PatternNoAlt_

*<sup>
[Type](types.md#either-types)
| [Constructing Expression](./expressions/construction.md#either-selections)
</sup>*

- A variant pattern `.la p` can only be used on type `either { .la A, ... }` and `p` must be able to be used on type `A`
- The pattern cannot be used if the type of the value being matched on is unknown
- When it can match, it is irrefutable if and only if the `either` has exactly one variant
- `.la p` matches `x` if and only if `x` is of the variant `.la` and `p` matches the payload of `x`
- Its bindings are those of `p`

Variant patterns are almost exclusively used in destructing values of either types (in [expressions](./expressions/application.md#either-destructions) or [statements](./statements/commands.md#todo)):
```par
dec get_second : [type T] [List<T>] (Option<T>) List<T>
def get_second = [type T] [list] list {
  .item(fst).item(snd) rest => (.some snd) .item(fst) rest,
  other => (.none!) other
}
```
-->

## Grouped Patterns

> **<sup>Syntax</sup>**\
> _GroupedPattern_ : `{` _Pattern_ `}`

A grouped pattern is equivalent to its body.

## Existential Patterns

> **<sup>Syntax</sup>**\
> _ExistentialPattern_ : `(` `type` [ID] `)` _Pattern_

*<sup>
[Type](types.md#existential-types)
| [Constructing Expression](./expressions/construction.md#existential-constructions)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

- An existential pattern `(type X) p` can only be used on an existential type `(type T) A` and `p` must be able to be used on type `A`
- When it can match, it is irrefutable if and only if `p` is irrefutable
- `(type X) p` matches `(type T) a` if and only if `p` matches `a`
- When matching `(type T) a`, the bindings are those of `p` matching `a`, and `X` is bound to `T`

Having multiple types between `(` and `)` is just syntax sugar:
```par
// the pattern
(type X, Y) p
// is equivalent to
(type X) (type Y) p
```

An existential pattern is used to destruct a value of an [existential type](types.md#existential-types)
```par
type Any = (type T) T

def any_test: Any = do {
  let x: Any = (type Bool) .true!
  let (type X) x_val = x
  // X = Bool
  // x_val = .true!
  let y: X = x_val
} (type X) y
```

<!--## Alternatives

> **<sup>Syntax</sup>**\
> _Alternatives_ : `|`<sup>?</sup> _PatternNoAlt_ (`|` _PatternNoAlt_)<sup>+</sup>

- The alternatives `a | b | ...` can be used on type `T` if `a`, `b`, ... can all be used on type `T`
- Together, they're an irrefutable pattern if and only if (todo)
-->

## Future

More extensive pattern matching, along with more types of patterns is planned in the future. See [here](future.md) for more.