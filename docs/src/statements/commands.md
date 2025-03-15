# Commands

> **<sup>Syntax</sup>**\
> _Command_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_ContinueCommand_](#the-continue-command) \
> &nbsp;&nbsp; | [_SendCommand_](#send-commands) \
> &nbsp;&nbsp; | [_ReceiveCommand_](#receive-commands) \
> &nbsp;&nbsp; | [_SignalCommand_](#signal-commands) \
> &nbsp;&nbsp; | [_MatchCommand_](#match-commands) \
> &nbsp;&nbsp; | [_BeginCommand_](#recursive-commands) \
> &nbsp;&nbsp; | [_SendTypeCommand_](#send-type-commands) \
> &nbsp;&nbsp; | [_ReceiveTypeCommand_](#receive-type-commands)
>
> _TerminatingCommand_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_BreakCommand_](#the-break-command) \
> &nbsp;&nbsp; | [_LinkCommand_](#link-commands) \
> &nbsp;&nbsp; | [_Loop_](#recursive-commands)
>
> _Receiver_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [ID]

## Link Commands

> **<sup>Syntax</sup>**\
> _LinkCommand_ : _Receiver_ `<>` [_Expression_]

Link commands are symmetric, i.e. `a <> b` is the same as `b <> a`. On the right side, any expression can be used, however.

The types on both sides must be dual to each other.
The link then annihilates both and ends the process.

```par
def f: A = chan return: chan A {
  let a: A = ...
  return <> a
}
```

## The Break Command

> **<sup>Syntax</sup>**\
> _BreakCommand_ : _Receiver_ `!`

*<sup>
[Dual](#the-continue-command)
| [Destructs Type](../types.md#the-bottom-type)
| [Destructs Channel](../types.md#the-unit-type)
</sup>*

A break command destructs a `?` and ends the process.
It is used in channels, as destructing a `?` is constructing its dual `!`.
```par
def unit: ! = chan bottom: ? {
  bottom!
}
```

## The Continue Command

> **<sup>Syntax</sup>**\
> _ContinueCommand_ : _Receiver_ `?`

*<sup>
[Dual](#the-break-command)
| [Destructs Type](../types.md#the-unit-type)
| [Destructs Channel](../types.md#the-bottom-type)
| [Pattern](../patterns.md#the-unit-pattern)
| [Constructing Expression](../expressions/construction.md#the-unit-expression)
</sup>*

A continue is used to destruct a `!`:
```par
dec drop_bool : [Bool] !

let b1: Bool = ...
let b2: Bool = ...
// get rid of b1
do {
  // drop_bool(b1) returns !
  // this is destructed by ?
  drop_bool(b1)?
} in b2
```

## Send Commands

> **<sup>Syntax</sup>**\
> _SendCommand_ : _Receiver_ `(` [_ExpressionList_] `)`

*<sup>
[Dual](#receive-commands)
| [Destructs Type](../types.md#function-types)
| [Destructs Channel](../types.md#pair-types)
| [Expression](../expressions/application.md#function-calls)
| [Constructing Expression](../expressions/construction.md#function-expressions)
</sup>*

Having multiple expressions between `(` and `)` is just syntax sugar:
```par
// the command
r(x, y)
// is equivalent to
r(x)(y)
```

A send command can destruct ("call") a function:
```par
do {
  let b: Bool = .true!
  let negate: [Bool] Bool = ...

  // call negate
  negate(b)
  // negate is now the negation of b
} in negate
// evaluates to .false!
```

It can also be used in channels: Destructing `[A] chan B` to construct `(A) B`
```par
def true_false: (Bool) Bool = chan return: [Bool] chan Bool {
  return(.true!)
  // return is now a chan Bool
  return <> .false!
}
```

## Receive Commands

> **<sup>Syntax</sup>**\
> _SendCommand_ : _Receiver_ `[` [_PatternList_] `]`

*<sup>
[Dual](#send-commands)
| [Destructs Type](../types.md#pair-types)
| [Destructs Channel](../types.md#function-types)
| [Pattern](../patterns.md#pair-patterns)
| [Constructing Expression](../expressions/construction.md#pair-expressions)
</sup>*

Having multiple patterns between `(` and `)` is just syntax sugar:
```par
// the pattern
r[p, q]
// is equivalent to
r[p][q]
```

A receive command can destruct a pair:
```par
dec reverse : [type A, B] [(A) B] (B) A
def reverse = [type A, B] [pair] do {
  // receive first value
  pair[first]
  // if pair was (a) b :
  // first is now a
  // pair is now b
} in (pair) first
```

It can also be used in channels: Destructing `(A) chan B` to construct `[A] B`
```par
def negate: [Bool] Bool = chan return: (Bool) chan Bool {
  // receive the argument
  return[arg]
  // return is now a chan Bool
  return <> arg {
    .true! => .false!
    .false! => .true!
  }
}
```

## Signal Commands

> **<sup>Syntax</sup>**\
> _SignalCommand_ : _Receiver_ [_Label_]

*<sup>
[Dual](#match-commands)
| [Destructs Type](../types.md#choice-types)
| [Destructs Channel](../types.md#either-types)
| [Expression](../expressions/application.md#choice-selections)
| [Constructing Expression](../expressions/construction.md#choice-constructions)
</sup>*

A signal command can destruct a choice:
```par
type Stream<T> = iterative {
  .close => !
  .next => (T) self
}

dec first_two : [type T] [Stream<T>] (T, T)!
def first_two = [type T] [stream] do {
  // signal next
  stream.next
  // stream is now (T) Stream<T>
  stream[first]
  // stream is now again Stream<T>

  // combine both operations
  stream.next[second]

  // close the stream
  stream.close?
} in (first, second)!
```
It can also be used in channels: Destructing a choice type to construct an either type:
```par
// chan Bool is equal to
type ChanBool = {
  .true => ?
  .false => ?
}

def just_true: Bool = chan return: ChanBool {
  // signal true
  return.true
  // return is now ?
  return!

  // return.true! would have been equally valid
}
```

## Match Commands

> **<sup>Syntax</sup>**\
> _MatchCommand_ : _Receiver_ `{` ([_Label_] (`(` [_ReceivePatterns_] `)`)<sup>\*</sup> `!`<sup>?</sup> `=>` `{` [_Process_] `}`)<sup>\*</sup> `}`
<!-- maybe also allow statement instead of {process} -->

*<sup>
[Dual](#signal-commands)
| [Destructs Type](../types.md#either-types)
| [Destructs Channel](../types.md#choice-types)
| [Expression](../expressions/application.md#either-destructions)
| [Constructing Expression](../expressions/construction.md#either-selections)
</sup>*

Patterns in `(...)` after the label are equivalent to a receive command in the body:
```par
x {
  .label(a) => { rest... }
}
// is equivalent to
x {
  .label => {
    x[a]
    rest...
  }
}
```
`(a, b)` is also equivalent to `(a)(b)` here.

Similarly, a `!` afterwards is equivalent to a continue in the body:
```par
x {
  .label(...)! => { rest... }
}
// is equivalent to
x {
  .label(...) => {
    x?
    rest...
  }
}
```

A match command can destruct an either type:
```par
def drop_bool: [Bool] ! = [b] do {
  // match on b
  b {
    .true => {
      // b is now !
      b?
    }
    // combine both 
    // (moving ? over => makes it !)
    .false! => {}
  }
} in !
```
It can also be used in channels: Destructing an either type to construct a choice type:
```par
// choice of two
type BoolChoice<A, B> = {
  .true => A
  .false => B
}

// dual type
type ChanBoolChoice<A, B> = either {
  .true chan A
  .false chan B
}

dec negate_choice : BoolChoice<Bool, Bool>
def negate_choice = chan return: ChanBoolChoice<Bool, Bool> {
  return {
    .true => {
      // return is now of type chan Bool
      return.false!
    }
    .false => { return.true! }
  }
}
```

## Recursive Commands

> **<sup>Syntax</sup>**\
> _SendCommand_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `begin` _LoopLabel_<sup>?</sup>
>
> _LoopCommand_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `loop` _LoopLabel_<sup>?</sup>
> 
> _LoopLabel_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `:` [ID]

*<sup>
[Destructs Type](../types.md#recursive-types)
| [Destructs Channel](../types.md#iterative-types)
| [Expression](../expressions/application.md#recursive-destructions)
</sup>*

If no loop label is present, `loop` corresponds to the innermost `begin`. Else to the `begin` with the same loop label.

A recursive command can destruct a recursive type:
```par
def list_and: [List<Bool>] Bool = [list] do {
  let result: Bool = .true!

  // destruct list recursively
  list begin {
    // after begin, list is of type
    // either { .empty!, .item(Bool) List<Bool> }
    // notice the absence of recursive
    .item => {
      // list is now of type (Bool) List<Bool>
      list[b]
      b {
        .true! => {}
        .false! => {
          drop_bool(result)?
          let result: Bool = .false!
        }
      }
      // list is now of type List<Bool>
      // go to begin
      list loop
    }
    .empty! => {}
  }
} in result
```
Note that
```par
.item => {
  list[b]
  ...
}
```
could have been replaced with
```par
.item(b) => {
  ...
}
```

A recursive command can also be used in channels: Destructing a recursive (here: either) type to construct an iterative (here: choice) type:
```par
// chan Stream<Bool> is
type ChanStreamBool = recursive either {
  .close?
  .next[T] self
}

def alt_true_false: Stream<Bool> = chan return: ChanStreamBool {
  let next: Bool = .true!
  // begin recursive destruction
  return begin {
    // return is now of type 
    // either { .close?, .next[T] ChanStreamBool }
    // again, notice the absence of recursive
    .close => {
      // return is now ?
      // handle next first
      drop_bool(next)?
      return!
    }
    .next => {
      // return is now of type [T] ChanStreamBool
      // handle next first
      next {
        .true! => {
          let yield = .true!
          let next = .false!
        }
        .false! => {
          let yield = .false!
          let next = .true!
        }
      }
      return(yield)
      // return is now of type ChanStreamBool
      return loop

      // return(yield) loop would have been equally valid
    }
  }
}

// in expression syntax:
def alt_true_false: Stream<Bool> = do {
  let next: Bool = .true!
} in begin {
  .close => drop_bool(next),
  .next => let (yield: Bool, next: Bool)! = next {
    .true! => (.true!, .false!)!
    .false! => (.false!, .true!)!
  } in (yield) loop
}
```

## Send Type Commands

> **<sup>Syntax</sup>**\
> _SendTypeCommand_ : _Receiver_ `(` `type` [_ID_List_] `)`

*<sup>
[Dual](#receive-type-commands)
| [Destructs Type](../types.md#universal-types)
| [Destructs Channel](../types.md#existential-types)
| [Expression](../expressions/application.md#universal-specializations)
| [Constructing Expression](../expressions/construction.md#universal-constructions)
</sup>*

Having multiple types between `(` and `)` is just syntax sugar:
```par
// the command
r(type T, U)
// is equivalent to
r(type T)(type U)
```

A send command can destruct ("specialize") a universal type:
```par
def id: [type T] [T] T = [type T] [x] x

def just_true = do {
  let b: Bool = .true!
  
  let f = id
  // specialize f
  f(type Bool)
  // f is now of type [Bool] Bool
  f(b)
  // f is now .true!
} in f
```

It can also be used in channels: Destructing `[type X] chan T` to construct `(type X) T`
```par
type Any = (type T) T

def true_as_any: Any = chan return: [type T] chan T {
  return(type Bool)
  // return is now type chan Bool
  return.true!

  // could have been combined to
  // return (type Bool) .true!
}
```

## Receive Type Commands

> **<sup>Syntax</sup>**\
> _SendTypeCommand_ : _Receiver_ `[` `type` [_ID_List_] `]`

*<sup>
[Dual](#send-type-commands)
| [Destructs Type](../types.md#existential-types)
| [Destructs Channel](../types.md#universal-types)
| [Pattern](../patterns.md#existential-patterns)
| [Constructing Expression](../expressions/construction.md#existential-constructions)
</sup>*

Having multiple names between `[` and `]` is just syntax sugar:
```par
// the pattern
r[type X, Y]
// is equivalent to
r[type X][type Y]
```

A receive command can destruct an existential type:
```par
def complicated_any_id: (Any) Any = [x] do {
  // receive the type
  x[type X]
  // x is now of type X
  let y: X = x
} in (type X) y
```

It can also be used in channels: Destructing `(type T) chan R` to construct `[T] R`
```par
def id: [type T] [T] T = chan return: (type T) (T) chan T {
  return[type T]
  // return is now of type (T) chan T
  return[x]
  // return is now of type chan T
  return <> x
}
```

[ID]: ../lexical.md
[_Expression_]: ../expressions.md
[_ExpressionList_]: ../expressions.md
[_PatternList_]: ../patterns.md
[_ID_List_]: ../lexical.md