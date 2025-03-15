# Types

At the heart of Par lies its type system, representing linear logic.

> **<sup>Syntax</sup>**\
> _Type_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_NamedType_](#named-types) \
> &nbsp;&nbsp; | [_Unit_](#the-unit-type) \
> &nbsp;&nbsp; | [_PairType_](#pair-types) \
> &nbsp;&nbsp; | [_FunctionType_](#function-types) \
> &nbsp;&nbsp; | [_EitherType_](#either-types) \
> &nbsp;&nbsp; | [_ChoiceType_](#choice-types) \
> &nbsp;&nbsp; | [_RecursiveType_](#recursive-types) \
> &nbsp;&nbsp; | [_IterativeType_](#iterative-types) \
> &nbsp;&nbsp; | [_ExistentialType_](#existential-types) \
> &nbsp;&nbsp; | [_UniversalType_](#universal-types) \
> &nbsp;&nbsp; | [_Bottom_](#the-bottom-type) \
> &nbsp;&nbsp; | [_ChannelType_](#channel-types) \
> &nbsp;&nbsp; | _Self_ <!--\
> &nbsp;&nbsp; | _ReplicableType_ \
> &nbsp;&nbsp; | _TaggedType_ -->
>
> _Self_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `self` [_LoopLabel_]<sup>?</sup>
>
> _TypeList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Type_ (`,` _Type_)<sup>\*</sup> `,`<sup>?</sup>
>
> _TypeArguments_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` _TypeList_ `>`
>
> _Label_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `.` [ID]

## Named Types

> **<sup>Syntax</sup>**\
> _NamedType_ : [ID] _TypeArguments_<sup>?</sup>

Defined via [type aliases](items.md#type-definitions), named types can always be replaced with their definition without changing meaning.

```par
let x: Option<T> = .none!
// is equivalent to
let x: either { .none!, .some T } = .none!
```

## The Unit Type

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

*<sup>
[Dual](#the-bottom-type)
| [Constructing Expression](./expressions/construction.md#the-unit-expression)
| [Pattern](./patterns.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

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

## Pair Types

> **<sup>Syntax</sup>**\
> _PairType_ : `(` _TypeList_ `)` _Type_

*<sup>
[Dual](#function-types)
| [Constructing Expression](./expressions/construction.md#pair-expressions)
| [Pattern](./patterns.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

Having multiple types between `(` and `)` is just syntax sugar:
```par
type T = (A, B) R
// is equivalent to
type T = (A) (B) R
```

While `(A, B)!` and `(A) B` are both valid ways to define a pair of `A` and `B`, depending on the context, one might be more convenient than the other:
```par
// convert (A, B)! into (A) B
def i : [(A, B)!] (A) B = [x]
  let (a, b)! = x in (a) b
// and back
def j : [(A) B] (A, B)! = [x]
  let (a) b = x in (a, b)!

// a good use case of (A) B
type List<T> = recursive either {
  .empty!
  .item(T) self
}
// can now be created like this:
let bool_list: List<Bool> =
  .item(.true!).item(.false!).empty!

// in most cases, (A, B)! is the safer bet
// as it uses more friendly syntax
type Pair<T, T> = (T, T)!

let bool_pair: Pair<Bool> =
  (.true!, .false!)!
```

Values are created using [pair expressions](./expressions/construction.md#pair-expressions):
```par
let a: A = ...
let b: B = ...

let pair: (A) B = (a) b
```
and they can be destructed using [pair patterns]() or [receive commands]():
```par
let triple: (A, B, C)! = (a, b, c)!

// pattern matching
let (first) rest = triple
// first = a
// rest = (b, c)!

// commands
do {
  rest[second]
  // after this command:
  // rest = (c)! 
  // second = b
} in ...
```

Mathematically, `(A) B` is \\(A \otimes B\\). For session types, it means "send `A` and continue as `B`".

## Function Types

> **<sup>Syntax</sup>**\
> _FunctionType_ : `[` _TypeList_ `]` _Type_

*<sup>
[Dual](#pair-types)
| [Constructing Expression](./expressions/construction.md#function-expressions)
| [Destructing Expression](./expressions/application.md#function-calls)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

Having multiple types between `[` and `]` is just syntax sugar:
```par
type T = [A, B] R
// is equivalent to
type T = [A] [B] R
```

Values are created using [function expressions](./expressions/construction.md#function-expressions):
```par
let add1: [Nat] Nat = [n] .succ n
```
and destructed by [calling](./expressions/application.md#function-calls) the function:
```par
let one: Nat = .succ.zero!
let two = add1(one)
```

Mathematically, `[A] B` is a [linear](./linearity.md) function \\(A \multimap B\\). For session types, it means "receive `A` and continue as `B`".

## Either Types

> **<sup>Syntax</sup>**\
> _EitherType_ : `either` `{` (_Label_ _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`

*<sup>
[Dual](#choice-types)
| [Constructing Expression](./expressions/construction.md#either-selections)
| [Destructing Expression](./expressions/application.md#either-destructions)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

An either type is the usual sum type aka. a tagged union (in Rust, it's an `enum`). Every value of such a type consists of a label, marking the variant, and a value of the type corresponding to the label (its "payload").

```par
// the most basic sum type
type Bool = either {
  .true!  // variant "true" with payload !
  .false! // variant "false", also with payload !
}

// a slightly more complex example
type TwoOrNone<T> = either {
  .none!      // variant "none" with "no" payload (using !)
  .two(T, T)! // variant "some" with "two" payloads
}
```

Values are created by attaching a label to its required payload.
Note that the corresponding either type must always be known when labeling an expression. A [type annotation]() can be used for that.
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
> &nbsp;&nbsp; &nbsp;&nbsp; `{` (_Label_ (`(` _ReceiveTypes_ `)`)<sup>\*</sup> `=>` _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`
>
> _ReceiveTypes_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _TypeList_ \
> &nbsp;&nbsp; | `type` [_ID_List_]

*<sup>
[Dual](#either-types)
| [Constructing Expression](./expressions/construction.md#choice-constructions)
| [Destructing Expression](./expressions/application.md#choice-selections)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

A choice type is dual to an [either](#either-types) type. Constructing a value of an either type is "making a choice" and similarly, destructing such a value looks exactly like constructing a value of a choice type.
It consists of several labels that can be used as signals to destruct the receiver.

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
type Stack<T, Unwrap> = iterative {
  .push(T) => self
  .pop => (Option<T>) self
  .unwrap => Unwrap
}

// implementing it
dec list_stack : [type T] [List<T>] Stack<T, List<T>>
def list_stack = [type T] [list] begin {
  .push(x) => let list: List<T> = .item(x) list in loop
  .pop => list {
    .empty! => (.none!) let list: List<T> = .empty! in loop,
    .item(head) tail => (.some head) let list = tail in loop
  }
  .unwrap => list
}

def main = do {
  let stack = list_stack(type Bool)(.empty!)
  stack.push(.true!)
  stack.push(.false!)
} in stack
```
For an explanation of `iterative`-`self` and `begin`-`loop`, see [iterative types](#iterative-types)

Mathematically, `{ .a => A, .b => B }` is \\(A \mathbin{\\&} B\\). For session types, it means "offer a choice of `A` or `B`".
An empty choice `{}` is therefore \\(\top\\) and has exactly one value, `{}`. There is a function to it from every type:
```par
def immortalize: [type T] [T] {} = [type T] [x] {}
```
The result of this function can never be used though.

Choice types are often used as [iterative](#iterative-types) types.

## Recursive Types

> **<sup>Syntax</sup>**\
> _RecursiveType_ : `recursive` [_LoopLabel_]<sup>?</sup> _Type_

*<sup>
[Dual](#iterative-types)
| [Destructing Expression](./expressions/application.md#recursive-destructions)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

A recursive type can be used within itself via `self`.

If no loop label is present, `self` corresponds to the innermost `recursive`/`iterative`. Else to the one with the same loop label.

Recursive types are mostly used in conjunction with either types:
```par
type List<T> = recursive either {
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
type Rec = recursive Node<self>

// We have the following subtyping relation:
// Node<recursive Node<self>> <: recursive Node<self>-->

Values of recursive types always terminate. They have to be constructed finitely.
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
type Nat = recursive either { 
  .zero!, 
  .succ self
}

dec is_even : [Nat] Bool
// induction over n (marked by applying begin-loop)
def is_even = [n] n begin {
  // base case(s)
  .zero! => .true!
  // inductive step(s)
  // pred loop is analogous to an inductive hypothesis
  .succ pred => not(pred loop)
}
```

## Iterative Types

> **<sup>Syntax</sup>**\
> _IterativeType_ : `iterative` [_LoopLabel_]<sup>?</sup> _Type_

*<sup>
[Dual](#recursive-types)
| [Constructing Expression](./expressions/construction.md#iterative-constructions)
| [Constructing Statement](./statements/commands.md#todo)
</sup>*

An iterative type can be used within itself via `self`.

If no loop label is present, `self` corresponds to the innermost `recursive`/`iterative`. Else to the one with the same loop label.

Iterative types are mostly used in conjunction with choice types, for example:
<!--```par
type Repl<T> = iterative {
  .copy => (self, self)!
  .drop => !
  .unwrap => T
}
```-->
```par
type Stream<T> = iterative {
  .close => !
  .next => (T) self
}
```

<!--Here we construct a value of an iterative type. Note the similarity
between this and destructing a recursive type.
```par
?type Bool = either { .true!, .false! }
?type Nat = recursive either { .zero!, .succ self }
?type List<T> = recursive either { .empty!, .item(T) self }
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

Values of iterative types may be infinite. In contrast to recursive types, such values can only be _destructed_ in finitely many steps.
```par
type Inf<T> = iterative (T) self

def infinite_bools: Inf<Bool> = begin (.true!) loop
```
This infinite value can be constructed but there is no way of fully destructing (so: using) it.

Mathematically, an iterative choice type represents a coinductive type.
Destructors without `loop` break the iteration and return, while those containing `loop` yield and continue.

A function to an iterative type is defined using coinduction (iteration):
<!--```par
// iterative (coinductive) type representing
// an infinite stream
type Stream<T> = iterative { 
  .close => !, 
  .next => (T) self
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
<!--```par
// construct a value of the iterative Repl<Bool>
dec repl_bool : [Bool] Repl<Bool>
// coinduction (marked by an independent begin-loop)
def repl_bool = [b] begin {
  // yield "(b, b)" and
  // continue (coinductive step), written as `loop`
  .copy => b {
    .true! => (let b: Bool = .true! in loop, let b: Bool = .true! in loop)!
    .false! => (let b: Bool = .false! in loop, let b: Bool = .false! in loop)!
  }
  // break, yield !
  .drop => b {
    .true! => !
    .false! => !
  }
  // break, yield b
  .unwrap => b
}
```-->
```par
?type Nat = recursive either { .zero!, .succ self }
?
// construct a stream of all natural numbers
// in form of the iterative Stream<Nat>
def nat_stream: Stream<Nat> = 
  let n: Nat = .zero! in 
  // coinduction (independent begin-loop)
  begin {
    // break, return !
    .close => drop(n),

    .next => do {
      let (next, n)! = copy(n)
      let n: Nat = .succ n
    } in
      // yield the next number 
      (n1)
      // continue (coinductive step)
      loop 
  }

// helpers

def drop: [Nat] ! = [n] n begin {
  .zero! => !
  .succ pred => pred loop
}

def copy: [Nat] (Nat, Nat)! = [n] n begin {
  .zero! => (.zero!, .zero!)!
  .succ pred => let (p1, p2)! = pred loop
    in (.succ p1, .succ p2)!
}
```
<!--```par
// fibonacci sequence
def fib: Stream<Nat> = do {
  let n: Nat  = .succ.zero!
  let p: Nat = .succ.zero!
  // coinduction
} in begin {
    // break, return !
    .close => do {
      drop(n)?
      drop(p)?
    } in !

    .next => do {
      let (n1, n2)! = copy(n)
      let (p1, p2)! = copy(p)
      let p = n1
      let n = add(n2, p2)
    } in
      // yield
      (p1)
      // continue
      loop
  }
def add: [Nat, Nat] Nat = [a, b] a begin {
  .zero! => b,
  .succ pred => .succ pred loop
}
```-->

## Existential Types

> **<sup>Syntax</sup>**\
> _ExistentialType_ : `(` `type` [_ID_List_] `)` _Type_

*<sup>
[Dual](#universal-types)
| [Constructing Expression](./expressions/construction.md#existential-constructions)
| [Pattern](./patterns.md#todo)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

Having multiple types between `(` and `)` is just syntax sugar:
```par
type T = (type A, B) X
// is equivalent to
type T = (type A) (type B) X
```

Existential types mirror pair types but they're qualified over types rather than values.
They are used to encapsulate their underlying type.
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

Mathematically, `(type T) A` is \\(\exists\ T: A\\).

## Universal Types

> **<sup>Syntax</sup>**\
> _UniversalType_ : `[` `type` [_ID_List_] `]` _Type_

*<sup>
[Dual](#existential-types)
| [Constructing Expression](./expressions/construction.md#universal-constructions)
| [Destructing Expression](./expressions/application.md#universal-specializations)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

Having multiple types between `[` and `]` is just syntax sugar:
```par
type T = [type A, B] X
// is equivalent to
type T = [type A] [type B] X
```

Values of universal types can be instantiated for any type. These types syntactically mirror function types but they're qualified over types rather than values.
They're also similar to parameterized types.
They are used to obscure their underlying type.
```par
// compare the three types of parameterizing types
type UnivEndo = [type T] [T] T
type ParamEndo<T> = [T] T
type ExistEndo = (type T) [T] T

// a value of an universal type must be defined
// for all types
let id: UnivEndo = [type T] [x] x

// a specialized version can be represented using
// a parameterized type
let id_bool: ParamEndo<Bool> = id(type Bool)

// this type encapsulates the Bool
let id_bool_2: ExistEndo = (type Bool) id_bool
```
For a more interesting example, see [(list reverse)]().

Mathematically, `[type T] A` is \\(\forall\ T: A\\).

## The Bottom Type

> **<sup>Syntax</sup>**\
> _Bottom_ : `?`

*<sup>
[Dual](#the-unit-type)
| [Constructing Statement](./statements/commands.md#todo)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

The bottom `?` is dual to the unit `!`.
```par
def main: Bool = chan user {
  user.true
  // user now has type ?
  user!
}
```

Mathematically, `?` is \\(\bot\\), the unit for \\(⅋\\). So \\(\bot \mathbin{⅋} A = \mathbf{1} \multimap A \cong A\\) (as seen before for the [unit](#the-unit-type) type).

## Channel Types

> **<sup>Syntax</sup>**\
> _ChannelType_ : `chan` _Type_

*<sup>
[Dual](#types)
| [Constructing Expression](expressions.md#channel-expressions)
| [Destructing Statement](./statements/commands.md#todo)
</sup>*

`chan A` represents a channel accepting an `A`:
```par
def just_true: Bool = chan yield {
  let c: chan Bool = yield
  c.true!
}
```
`chan` is merely a type transformer, turning a type into its dual.
For example, `chan chan T` is _equal_ to `T` (not just isomorphic).

A more elaborate example can be seen [here](https://github.com/faiface/par-lang/blob/main/examples/flatten.par)

A `chan A` can be linked with an `A` (using `<>`), annihilating both and ending the process.
```par
def just_true: Bool = chan yield {
  let c: chan Bool = yield
  let b: Bool = .true!
  c <> b
}
```
Note that `b <> c` would have been equally valid.

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
| `recursive T` | `iterative chan T` |
| `iterative T` | `recursive chan T` |
| `self` | `self` |
| `(type T) A` | `[type T] chan A` |
| `[type T] A` | `(type T) chan A` |

Moreover, `chan A ≅ [A]?` is an isomorphism
```par
dec i : [type A] [chan A] [A]?
def i = [type A] [ch] chan receive {
  // receive is of type (A)!
  receive[a]?
  ch <> a
}

dec j : [type A] [[A]?] chan A
def j = [type A] [annihilate] chan a {
  // a is of type A
  annihilate(a)!
}
```

So the dual of a type can be used to destruct a value.

[ID]: ./lexical.md
[_ID_List_]: ./lexical.md
[_LoopLabel_]: ./expressions.md