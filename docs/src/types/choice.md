# Choice

The famous slogan of [sum types](./either.md) is: **_Make illegal states unrepresentable!_**

_Choice types_ — the dual of sum types, also known as _codata_ — deserve an equally potent
slogan:

> **_Make illegal operations unperformable!_**

Choice types are somewhat related to _interfaces_, like in Go, or Java, but I encourage you to
approach them with a fresh mind. The differences are important enough to consider choice types
their own thing.

A _choice type_ is defined by a finite number of branches, each with a different name and a result.

Values of a choice type are objects that can (and must) be destructed using one of the available
branches, to obtain its result.

```par
type ChooseStringOrNumber = choice {
  .string => String,
  .number => Int,
}
```

A choice type is spelled with the keyword `choice`, followed by curly braces enclosing a
comma-separated list of branches.

Each branch has a lower-case name prefixed by a period, followed by `=>`, and a single obligatory
result type.

```par
choice {
  .branch1 => Result1,
  .branch2 => Result2,
  .branch3 => Result3,
}
```

If the result is a function, we can use _syntax sugar_, and move the argument to the left side
of the arrow, inside round parentheses:

```par
type CancellableFunction<a, b> = choice {
  .cancel => !,
  //.apply => [a] b,
  .apply(a) => b,
}
```

Like [functions](./function.md), **choice types are [linear](TODO).** A value of a choice type may not be
dropped, or copied. It must be destructed exactly once, using one of its branches.

> Choice types are frequently used together with [_iterative_](./iterative.md) types to define objects
> that can be acted upon repeatedly. For example, the [built-in](../standard_library.md) `Console` type
> obtained as a handle to print to the standard output is an _iterative choice_:
> 
> ```par
> type Console = iterative choice {
>   .close => !,
>   .print(String) => self,
> }
> ```
> 
> Then it can be used to print multiple lines in order:
> 
> ```par
> def Main = Console.Open
>   .print("First line.")
>   .print("Second line.")
>   .print("Third line.")
>   .close
> ```

## Construction

Values of choice types are constructed using standalone `case` expressions.

```par
def Example: ChooseStringOrNumber = case {
  .string => "Hello!",
  .number => 42,
}
```

Each branch inside the curly braces follows the same syntax as the branches in the corresponding
type, except with types replaced by their values.

```par
def IntToString: CancellableFunction<Int, String> = case {
  .cancel => !,
  //.apply => [s] Int.ToString(s),
  .apply(s) => Int.ToString(s),
}
```

Unlike [patterns](TODO) in `.case` branches of [_either_ types](./either.md), branches in `case`
expressions of choice types don't have a payload to bind: they produce a result. However, we can
still bind function arguments on the left side of the arrow.

## Destruction

Choices are destructed by _selecting_ a branch, transforming it into the corresponding result.
We do it by applying `.branch` after a value of a choice type.

```par
def Number = Example.number  // = 42
```

Above, we defined the type `CancellableFunction<a, b>`, and a value of that type: `IntToString`.
[Bare functions](./function.md) are linear, so we must call them, but the _cancellable function_
gives us a choice of either calling it, or not.

We can use this to define a _map_ function for optional values:

```par
type Option<a> = either {
  .none!,
  .some a,
}

dec MapOption :
  [type a, b]
  [Option<a>]
  [CancellableFunction<a, b>]
  Option<b>

def MapOption = [type a, b] [option, func] option.case {
  .none! => let ! = func.cancel in .none!,
//                  \_________/
  .some x => let y = func.apply(x) in .some y,
//                   \________/
}

def Result = MapOption(type Int, String)(.some 42, IntToString)  // = .some "42"
```

This example also shows that in Par, you don't have to be shy about writing your types on
multiple lines. The syntax is designed for that.
