# Types

At the heart of Par lies its type system, representing linear logic.

> **<sup>Syntax</sup>**\
> _Type_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_NamedType_](#named-types) \
> &nbsp;&nbsp; | [_PairType_](#pair-types) \
> &nbsp;&nbsp; | [_FunctionType_](#function-types) \
> &nbsp;&nbsp; | [_EitherType_](#either-types) \
> &nbsp;&nbsp; | [_ChoiceType_](#choice-types) \
> &nbsp;&nbsp; | [_Unit_](#the-unit-type) \
> &nbsp;&nbsp; | [_Bottom_](#the-bottom-type) \
> &nbsp;&nbsp; | [_RecursiveType_](#recursive-types) \
> &nbsp;&nbsp; | [_IterativeType_](#iterative-types) \
> &nbsp;&nbsp; | [_Self_](#recursive-types) \
> &nbsp;&nbsp; | [_Loop_](#iterative-types) \
> &nbsp;&nbsp; | [_ExistentialType_](#existential-types) \
> &nbsp;&nbsp; | [_UniversalType_](#universal-types) \
> &nbsp;&nbsp; | [_ChannelType_](#channel-types) <!--\
> &nbsp;&nbsp; | _ReplicableType_ \
> &nbsp;&nbsp; | _TaggedType_ -->
>
> _TypeList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Type_ (`,` _Type_)<sup>\*</sup> `,`<sup>?</sup>
>
> _TypeArguments_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` _TypeList_ `>`
>
> _Label_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `.` [_ID_]

## Named Types

> **<sup>Syntax</sup>**\
> _NamedType_ : [_ID_] [_TypeArguments_]<sup>?</sup>

Defined via type aliases, named types can always be replaced with their definition without changing meaning.

```par
let x: Option<T> = .none!
// is equivalent to
let x: either { .none!, .some T } = .none!
```

## Pair Types

> **<sup>Syntax</sup>**\
> _PairType_ : `(` _TypeList_ `)` _Type_

Having multiple types between the `()` is just syntax sugar:
```par
type T = (A, B)!
// is equivalent to
type T = (A) (B) !
```

`!` is the unit for tuples, i.e. `(A, B)!` and `(A) B` are equivalent.

Values are created using [pair expressions](./expressions.md#tuple-expressions):
```par
let a: A = ...
let b: B = ...

let pair: (A) B = (a) b
```
and they can be destructed using [pair patterns]() or [receive commands]():
```par
type ABC = { .a!, .b!, .c! }

let triple: (ABC, ABC, ABC)! = (.a!, .b!, .c!)!

// pattern matching
let (first) rest = triple
// first = .a!
// rest = (.b!, .c!)!

// commands
do {
  rest[second]
  // after this command:
  // rest = (.c!)! 
  // second = .b!
} in ...
```

Mathematically, `(A) B` is \\(A \otimes B\\). For session types, it means "send `A` and continue as `B`".

## Function Types

> **<sup>Syntax</sup>**\
> _FunctionType_ : `[` _TypeList_ `]` _Type_

Having multiple types between the `[]` is just syntax sugar:
```par
type T = [A, B] R
// is equivalent to
type T = [A] [B] R
```

Values are created using [function expressions](./expressions.md#function-expressions):
```par
let id: [A] A = [a] a
```
and destructed by [calling]() the function:
```par
let a: A = ...
let also_a = id(a) // call id
```

Mathematically, `[A] B` is a [linear](./linearity.md) function \\(A \multimap B\\). For session types, it means "receive `A` and continue as `B`".

## Either Types

> **<sup>Syntax</sup>**\
> _EitherType_ : `either` `{` (_Label_ _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`

An either type is a classical sum type aka. tagged union. Every value of such a type consists of a label (called "constructor") and a value of the type corresponding to the label (its "payload").

```par
// the most basic sum type
type Bool = either {
  .true!  // constructor "true" with payload !
  .false! // constructor "false", also with payload !
}

// a slightly more complex example
type TwoOrNone<T> = either {
  .none!      // constructor "none" with "no" payload (using !)
  .two(T, T)! // constructor "some" with "two" payloads
}
```

Values are created by calling one of the constructors with its payload.
```par
let no_bool: TwoOrNone<Bool> = .none!

let both_bools: TwoOrNone<Bool> = .two(.true!, .false!)!
```

Mathematically, `either { .a A, .b B }` is \\(A \oplus B\\). For session types, it means "select from `A` or `B`".
An empty either type `either {}` is therefore \\(\mathbf{0}\\), the empty type.
In Haskell, it's called `void` and in Rust it's `!` (not to be confused with the `!` in Par). 
There is a function from it to every type:
```par
def absurd: [type T] [either {}] T = [type T] [x] x {}
```
This function can never be called though.

Either types are often used as [recursive](#recursive-types) types.

## Choice Types

> **<sup>Syntax</sup>**\
> _ChoiceType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `{` (_Label_ (`(` _ReceiverList_ `)`)<sup>\*</sup> `=>` _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`
>
> _ReceiverList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _TypeList_ \
> &nbsp;&nbsp; | `type` [_ID_List_]

A choice type is dual to an [either](#either-types) type. Constructing a value of an either type is "making a choice" and similarly, destructing such a value looks exactly like constructing a value of a choice type.
It consists of several "destructors".

```par
// choice of two
type BoolChoice<A, B> = {
  .true => A
  .false => B
}

// destruct a Bool
def negate(b: Bool): Bool = b {
  .true! => .false!
  .false! => .true!
}

// construct a choice
def negate_choice: BoolChoice<Bool, Bool> = {
  .true => .false!
  .false => .true!
}

// define negate using the choice
// featuring selecting from the choice type value
def also_negate: [Bool] Bool = [b] b {
  .true! => negate_choice.true
  .false! => negate_choice.false
}
```
`.cons => [A] B` can also be written as `.cons(A) => B`

A choice type represents an interface for interacting with data. While an either type describes its underlying data, a choice type describes what can be done with it.
```par
// creating an interface
type Stack<Unwrap, T> = iter {
  .push(T) => loop
  .pop => (Option<T>) loop
  .unwrap => Unwrap
}

// implementing it
dec list_stack : [type T] [List<T>] Stack<List<T>, T>
def list_stack = [type T] [list] begin {
  .push(x) => let list: List<T> = .item(x) list in loop
  .pop => list {
    .empty! => (.none!) let list: List<T> = .empty! in loop,
    .item(head) tail => (.some head) let list = tail in loop
  }
  .unwrap => list
}

def main = do {
  let list: List<Bool> = .item(.false!).empty!
  let stack: Stack<List<Bool>, Bool>
    = list_stack(type Bool)(list)

  stack.push(.true!)
  stack.push(.false!)
  stack.pop
} in stack
```
todo: Is this a good example?

Mathematically, `{ .a => A, .b => B }` is \\(A \mathbin{\\&} B\\). For session types, it means "offer a choice of `A` or `B`".
An empty choice `{}` is therefore \\(\top\\) and has exactly one value, `{}`. There is a function to it from every type:
```par
def terminate: [type T] [T] {} = [type T] [x] {}
```
The result of this function can never be used though (todo: correct?).

Choice types are often used as [iterative](#iterative-types) types.

## The Unit Type

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

Unit is a type providing no information. In C(++) it's called `void`, in Rust it's `()` (and it can be thought of as an empty tuple in Par as well). There is exactly one value of type `!`, and it's also `!`.
```par
let unit: ! = !
```
Every value of a type `A` corresponds to a function `[!] A`:
```par
def select: [type T] [T] [!] T = [type T] [x] [!] x
// uncurrying makes this clear
// [T] [!] T = [T, !] T ≅ [(T) !] T ≅ [T] T

def extract: [type T] [[!] T] T = [type T] [f] f(!)
```

For some types there is a function `[A] !`. 
Those can be destroyed without any transformation.
```par
// Types constructed only from ! are droppable
def drop_bool: [Bool] ! = [b] b {
  .true! => !
  .false! => !
}

// Functions are not droppable in general
def drop_impossible: [[Bool] Bool] ! = todo
```
<!--// Replicables are droppable
def drop_repl: [type T] [&T] ! = !-->

Mathematically, `!` is \\(\mathbf{1}\\), the unit for \\(\otimes\\).

## The Bottom Type

> **<sup>Syntax</sup>**\
> _Bottom_ : `?`

The bottom `?` is dual to the unit `!`. 
It's mostly used in process syntax to destruct the unit:
```par
let triple: (A, B, C)! = ...

let reverse = do {
  triple[a, b, c]?
} in (c, b, a)!
```

Mathematically, `?` is \\(\bot\\), the unit for \\(⅋\\). So \\(\bot \mathbin{⅋} A = \mathbf{1} \multimap A \cong A\\) (as seen before for the [unit](#the-unit-type) type).

## Recursive Types

> **<sup>Syntax</sup>**\
> _RecursiveType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `rec` [_LoopLabel_]<sup>?</sup> _Type_
>
> _Self_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `self` [_LoopLabel_]<sup>?</sup>

A recursive type can be used within itself via `self`

Recursive types are mostly used in conjunction with either types:
```par
type List<T> = rec either {
  .empty!
  .item(T) self
}
```
<!--// another way of defining a recursive type is the following:
// Node is not recursive
type Node<Next> = either {
  .base!
  .step Next
}
// Defining the recursive type
type Rec = rec Node<self>

// We have the following subtyping relation:
// Node<rec Node<self>> <: rec Node<self>-->

Values of recursive types terminate (todo: correct?)
```par
// a simple List
let l: List<Bool> = .item(.true!).item(.false!).empty!
```
Mathematically, a recursive either type represents an inductive type.
Constructors without `self` are the base cases while those with `self` represent
inductive steps.

A function from a recursive type is defined using induction:
```par
// recursive (inductive) type representing
// the natural numbers
type Nat = rec either { 
  .zero!, 
  .succ self
}

dec is_even : [Nat] Bool
// induction over n (marked by applying begin-loop)
def is_even = [n] n begin {
  // base case(s)
  .zero! => .true!
  // inductive step(s)
  .succ pred => not(pred loop)
}
```

## Iterative Types

> **<sup>Syntax</sup>**\
> _IterativeType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `iter` [_LoopLabel_]<sup>?</sup> _Type_
>
> _Loop_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `loop` [_LoopLabel_]<sup>?</sup>

todo: General, mathematical, constraints

An iterative type can be used within itself via `loop`

Iterative types are mostly used in conjunction with choice types, for example:
```par
type Repl<T> = iter {
  .copy => (loop, loop)!
  .drop => !
  .unwrap => T
}
```

<!--Here we construct a value of an iterative type. Note the similarity
between this and destructing a recursive type.
```par
?type Bool = either { .true!, .false! }
?type Nat = rec either { .zero!, .succ self }
?type List<T> = rec either { .empty!, .item(T) self }
?
// construct a list of Repl<Bool>
// i.e. a value of a recursive type
//
// at the same time, destruct a value
// of the recursive type Nat
dec repeat : [Nat, Repl<Bool>] List<Bool>
def repeat = [n, b] n begin {
  .zero! => do { b.drop? } in .empty!
  .succ pred => let (b, c)! = b.copy in .item(c.unwrap) pred loop
}

// construct a value of the iterative Repl<Bool>
dec repl_bool : [Bool] Repl<Bool>
def repl_bool = [b] begin {
  .copy => b {
    .true! => (let b: Bool = .true! in loop, let b: Bool = .true! in loop)!
    .false! => (let b: Bool = .false! in loop, let b: Bool = .false! in loop)!
  }
  .drop => b {
    .true! => !
    .false! => !
  }
  .unwrap => b
}
?
?def main = repeat(.succ.succ.succ.zero!, repl_bool(.true!))
```-->

Values of iterative types may be infinite (todo: correct?)
```par
type Inf<T> = iter (T) loop

def infinite_bools: Inf<Bool> = begin (.true!) loop
```
This infinite value can be constructed but there is now way of fully destructing (so: using) it.

Mathematically, an iterative choice type represents a coinductive type.
Destructors without `loop` break the iteration, while those containing `loop` yields and continues.

A function to an iterative type is defined using coinduction (iteration):
<!--```par
// iterative (coinductive) type representing
// an infinite stream
type Stream<T> = iter either { 
  .close!, 
  .next(T) loop
}

dec alternate_true_false : [Nat] Bool
// induction over n (marked by applying begin-loop to n)
def is_even = [n] n begin {
  // base case(s)
  .zero! => .true!
  // inductive step(s)
  .succ pred => not(pred loop)
}
```-->
```par
// construct a value of the iterative Repl<Bool>
dec repl_bool : [Bool] Repl<Bool>
// coinduction (marked by an independent begin-loop)
def repl_bool = [b] begin {
  // yield "(b, b)" and continue (coinductive step)
  .copy => b {
    .true! => (let b: Bool = .true! in loop, let b: Bool = .true! in loop)!
    .false! => (let b: Bool = .false! in loop, let b: Bool = .false! in loop)!
  }
  // break !
  .drop => b {
    .true! => !
    .false! => !
  }
  // break b
  .unwrap => b
}
```

## Existential Types

> **<sup>Syntax</sup>**\
> _ExistentialType_ : `(` `type` [_ID_List_] `)` _Type_

Having multiple types between the `()` is just syntax sugar:
```par
type T = (type A, B) X
// is equivalent to
type T = (type A) (type B) X
```

Existential types mirror tuple types but they're qualified over types rather than values.
They are used to obscure their underlying type.
```par
?type Bool  = either { .true!, .false! }
?type Nat = recursive either { .zero!, .succ self }
?
type Any = (type T) T

?def main = chan user {
// create values of the existential Any type
// note that both have exactly the same type!
let any1: Any = (type Bool) .true!
let any2: Any = (type Nat) .succ.zero!
?
?user(any1)
?user(any2)
?user!
?}
```

The qualifying types and values can both be extracted from a value of such a type:
```par
let any: Any = ...
let (type X) x = any
let y: X = x
```
todo: This doesn't work in Par currently. Is this intended?

Mathematically, `(type T) A` is \\(\exists\ T: A\\).

## Universal Types

> **<sup>Syntax</sup>**\
> _UniversalType_ : `[` `type` [_ID_List_] `]` _Type_

Having multiple types between the `[]` is just syntax sugar:
```par
type T = [type A, B] X
// is equivalent to
type T = [type A] [type B] X
```

Existential types mirror function types but they're qualified over types rather than values.
They're also similar to parameterized types.
They are used to obscure their underlying type.
```par
type UnivEndo = [type T] [T] T
type ParamEndo<T> = [T] T

// a value of an universal type must be defined
// for all types
let id: UnivEndo = [type T] [x] x

// a specialized version can be represented using
// a parameterized type
let id_bool: ParamEndo<Bool> = id(type Bool)
```

Mathematically, `[type T] A` is \\(\forall\ T: A\\).

## Channel Types

> **<sup>Syntax</sup>**\
> _ChannelType_ : `chan` _Type_

`chan A` represents a channel accepting an `A`:
```par
def just_true: Bool = chan yield {
  let c: chan Bool = yield
  c.true!
}
```

A more elaborate example can be seen [here](https://github.com/faiface/par-lang/blob/main/examples/flatten.par)

A `chan A` can be linked with an `A`, annihilating both and ending the process.
```par
def just_true: Bool = chan yield {
  let c: chan Bool = yield
  let b: Bool = .true!
  c <-> b
}
```
Note that `b <-> c` would have been equally valid.

Mathematically, `chan A` is \\(A^\perp\\), i.e. the dual type to `A`. Every type has a dual. We get for example:

| type | dual |
| ---- | ---- |
| `T` | `chan T` |
| `chan T` | `T` |
| `(A) B` | `[A] chan B` |
| `[A] B` | `(A) chan B` |
| `either { .a A, .b B }` | `{ .a => chan A, .b => chan B }` |
| `{ .a => A, .b => B }` | `either { .a chan A, .b chan B }` |
| `!` | `?` |
| `?` | `!` |
| `rec T` | `iter chan T` |
| `iter T` | `rec chan T` |
| `self` | `loop` |
| `loop` | `self` |
| `(type T) A` | `[type T] chan A` |
| `[type T] A` | `(type T) chan A` |

Moreover, `chan A ≅ [A]?` is an isomorphism
```par
dec i : [type A] [chan A] [A]?
def i = [type A] [ch] chan receive {
  // receive is of type (A)!
  receive[a]?
  ch <-> a
}

dec j : [type A] [[A]?] chan A
def j = [type A] [annihilate] chan a {
  annihilate(a)!
}
```

So the dual of a type can be used to destruct a value.
