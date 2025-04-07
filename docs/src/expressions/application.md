# Application Expressions

> **<sup>Syntax</sup>**\
> _Application_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ \
> &nbsp;&nbsp; | [_MatchExpression_](#match-expressions) \
> &nbsp;&nbsp; | [_LoopApplication_](#recursive-destructions)
>
> _Applicable_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_PrimaryExpression_] \
> &nbsp;&nbsp; | [_FunctionCall_](#function-calls) \
> &nbsp;&nbsp; | [_ChoiceSelection_](#choice-selections) \
> &nbsp;&nbsp; | [_RecursiveDestruction_](#recursive-destructions) \
> &nbsp;&nbsp; | [_UniversalSpecialization_](#universal-specializations)

While [constructions](construction.md) construct values, applications destruct them. Some types do not have an application expression for destruction -- they use [patterns](../patterns.md) instead.

An application is always of the form: _Applicable_ _Suffix_.
Every application corresponds to a [command](../statements/commands.md), which looks the same. In contrast to commmand receivers however, applicables don't become the result of this expression.

All applications can be linked via:
```par
dual <> app suffix
// is equivalent to
let a = app
a suffix
dual <> a
```
Note that if app is a local variable, the `let` is not needed.

## Function Calls

> **<sup>Syntax</sup>**\
> _FunctionCall_ : _Applicable_ `(` [_ExpressionList_] `)`

*<sup>
[Destructs Type](../types.md#function-types)
| [Destructs Expression](construction.md#function-expressions)
| [Statement](../statements/commands.md#send-commands)
</sup>*

Having multiple expressions between `(` and `)` is just syntax sugar:
```par
f(a, b)
// is equivalent to
f(a)(b)
```

If `f` is of type `[A] B` and `a` is of type `A`, the function call `f(a)` is of type `B`.

```par
def function: [A] B

let a: A = ...
let b: B = function(a)
```

A function call is equivalent to a [send command](../statements/commands.md#send-commands):
```par
// in process syntax
let b = f(a)
// is equivalent to
let b = f
b(a)
```

## Choice Selections

> **<sup>Syntax</sup>**\
> _ChoiceSelection_ : _Applicable_ [_Label_]

*<sup>
[Destructs Type](../types.md#choice-types)
| [Destructs Expression](construction.md#choice-constructions)
| [Statement](../statements/commands.md#signal-commands)
</sup>*

If `x` is of type `{ ..., .label => T, ... }`, the choice selection `x.label` is of type `T`.

```par
type BoolChoice = {
  .true => Bool,
  .false => Bool,
}

let bc: BoolChoice = ...
let choose_true = bc.true
```

[Iterative types](../types.md#iterative-types) have no special destruction syntax, instead they are finitely destructed as their underlying type. Most often they're seen as iterative choice types:
```par
type Stream<T> = iterative {
  .close => !,
  .next => (T) self
}

let nats: Stream<Nat> = ...

// finitely destruct nats
do {
  let (first) rest = nats.next
  let (second) rest = nats.next
  let ! = rest.close
} in (first, second)!
```

A choice selection is equivalent to a [signal command](../statements/commands.md#signal-commands):
```par
// in process syntax
let y = x.label
// is equivalent to
let y = x
y.label
```

## Match Expressions

<!--
> **<sup>Syntax</sup>**\
> _MatchExpression_ : _Applicable_ `{` (_Pattern_ `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`
-->
> **<sup>Syntax</sup>**\
> _MatchExpression_ : _Applicable_ `{` ([_Label_] (`(` [_ReceivePatterns_] `)`)<sup>\*</sup> ([ID] | `!`) `=>` [_Expression_] `,`<sup>?</sup>)<sup>\*</sup> `}`

*<sup>
[Destructs Type](../types.md#either-types)
| [Destructs Expression](construction.md#either-selections)
| [Statement](../statements/commands.md#match-commands)
</sup>*

Match expressions are similar to `match` in Rust or `case` in Haskell. Their pattern matching abilities are currently limited but will expand in the [future](../future.md).

If `x` is of type `either { .l1 T1, ..., .ln Tn }`
and
- `p1` is a [pattern](../patterns.md) for type `T1`, and so on, until `pn` is a pattern for type `Tn`
- `y1`, ..., `yn` are all of type `U`

then `x { .l1 p1 => y1, ..., .ln pn => yn }` is of type `U`. It evaluates to the `yi` which label `.li` was matched.
```par
type Option<T> = either {
  .none!,
  .some T
}

let o: Option<Nat> = ...

let o_add1: Option<Nat> = o {
  .none! => .none!,
  .some n => .some.succ n,
}
```

## Recursive Destructions

> **<sup>Syntax</sup>**\
> _RecursiveDestruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `unfounded`<sup>?</sup> `begin` [_LoopLabel_]<sup>?</sup>
>
> _LoopApplication_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `loop` [_LoopLabel_]<sup>?</sup>

*<sup>
[Destructs Type](../types.md#recursive-types)
| [Statement](../statements/commands.md#recursive-commands)
</sup>*

A `loop` corresponds to the innermost `begin` with the same loop label. `loop` without a label can only correspond to `begin` without a label.

If `x` is of type `recursive T`, then `begin x` is of type `T`, replacing each corresponding `self` of `T` with `recursive T`.

A `loop` corresponding to this `begin` can be used on values of type `recursive T`. Its behavior is equivalent to "pasting" the `begin` and every application that follows it.

<div class="warning">

Due to totality, `loop` can only be called on a descendant of the value `begin` was called on. I.e. on a value which type is a "`self`" correponding to the recursive type which `begin` was called on.

If that is not the case, the unsafe `unfounded begin` must be used, which leaves it up to the programmer to ensure totality.
</div>

Consider the recursive type
```par
type List<T> = recursive either {
  .empty!,
  .item(T) self,
}
```

This is a total `begin`-`loop`
```par
dec reverse : [type T] [List<T>] List<T>
def reverse = [type T] [list] do {
  let rev = .empty!
} in list begin {
  .empty! => rev,
  .item(head) tail => do {
    let rev = .item(head) rev
  } in tail loop
  // tail corresponds to the self in the
  // .item(T) self
  // branch
}
```
This is a total `unfounded begin`-`loop`, as the totality checker currently doesn't recognize this loop as total.
```par
dec reverse : [type T] [List<T>] List<T>
def reverse = [type T] [list] list unfounded begin {
  .empty! => .empty!,
  .item(head) tail => do {
    let (left, right)! = split(type T)(.item(head) tail)
  } in concat(type T)(
    left loop,
    right loop,
  )
}

/// splits the list into two lists
/// of equal length
/// (the left list may be one longer)
dec split : [type T] [List<T>] (List<T>, List<T>)!

/// concatenates two lists
dec concat : [type T] [List<T>, List<T>] List<T>
```
This is a nontotal `unfounded begin`-`loop`. Caution is required when using these to not lose totality.
```par
def infinite_loop: ! = do {
  let list: List<!> = .item(!).empty!
} in list unfounded begin {
  .empty! => !, // this is total
  .item(head) tail => do {
    let list: List<!> = .item(head) tail
  } in list loop, // this is not total
}
```

## Universal Specializations

> **<sup>Syntax</sup>**\
> _UniversalSpecialization_ : _Applicable_ `(` `type` [_TypeList_] `)`

*<sup>
[Destructs Type](../types.md#universal-types)
| [Destructs Expression](construction.md#universal-constructions)
| [Statement](../statements/commands.md#receive-type-commands)
</sup>*

Having multiple types between `(` and `)` is just syntax sugar:
```par
x(type T, U)
// is equivalent to
x(type T)(type U)
```

If `x` is of the universal type `[type T] R`, the specialization `f(type X)` is of type `R`.

These expressions often "instantiate generic functions"
```par
def id = [type T] [x: T] x

def id_bool: [Bool] Bool = id(type Bool)
def id_unit: [!] ! = id(type !)
```

[ID]: ../lexical.md#names
[_PrimaryExpression_]: ../expressions.md#primary-expressions
[_Expression_]: ../expressions.md
[_ExpressionList_]: ../expressions.md
[_Label_]: ../types.md
[_ReceivePatterns_]: construction.md#choice-constructions
[_LoopLabel_]: ../statements/commands.md#recursive-commands
[_TypeList_]: ../types.md