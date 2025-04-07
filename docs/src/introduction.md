# Introduction

Par (<span style="font-family: Noto Sans">⅋</span>) is a concurrent programming language bringing the expressive power of linear logic into practice.

This reference contains not only the complete grammar and specification of Par but also an extensive collection of examples, explanations and best practices.

## Overview

Par is a multi-layer language with an intermediate representation in itself.

A simple program like
```par
type HW = either { .hello_world! }

def main: HW = .hello_world!
```
is compiled to a program fully written in _process syntax_:
```par
?type HW = either { .hello_world! }
?
def main: HW = chan user {
  user.hello_world
  user!
}
```
Par is centered around concurrency and session typing, all in the framework of linear logic.
What does that mean?

- Types are _linear_, i.e. a value must be used exactly once.
  You might know the type system of Rust, where a value must be used at most once.
- Ultimately, everything in Par is a _channel_.
  - A list sends every item in order and then closes
  - A function receives its argument and becomes the result
  - An infinite stream can be signaled to either yield the next item or close
- Channels communicate with each other by 
  - sending signals (the names with a dot in front)
  - sending values
  - closing each other
- Everything has a dual in Par: A value can be created by destroying its dual (see [channel expressions](expressions.md#channel-expressions))
- This can all be abstracted away in [expressions](expressions.md) and [types](types.md) or be exposed as [statements](statements.md) in _process syntax_.

Putting all of this together, Par manages to be a functional language while also allowing imperative-style code and mutability.

For example, a mutable stack can be implemented like this ([explanation](types.md#choice-types)):
```par
?type Bool = either { .true!, .false! }
?type List<T> = recursive either { .empty!, .item(T) self }
?type Option<T> = either { .none!, .some T }
?
type Stack<Unwrap, T> = iterative {
  .push(T) => self
  .pop => (Option<T>) self
  .unwrap => Unwrap
}

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
  let list: List<Bool> = .empty!
  let stack = list_stack(type Bool)(list)
  // stack currently represents an empty list

  // the following operations mutate stack
  stack.push(.true!)
  // stack now represents a singleton of .true!
  stack.push(.false!)
  // stack now represents a two-element list
} in stack
```
Running this in the [playground](#getting-started) you can push and pop elements, or inspect the underlying data using unwrap.

For a complete tutorial, see the [Readme](#resources)

## Getting Started

To use Par, clone the repository
```sh
$ git clone https://github.com/faiface/par-lang.git
```
and run the app
```sh
$ cd par-lang
$ cargo run
```
Note: If you don't have Rust and Cargo installed, [do that first](https://doc.rust-lang.org/cargo/getting-started/installation.html)

This will launch the Par playground.
Some example code is already written for you.
Just press <kbd>Compile</kbd> and <kbd>Run</kbd> to run any definition from the program on the left.

## Community

To ask questions or discuss ideas, join our [Discord](https://discord.gg/8KsypefW99).
