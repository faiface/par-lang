# Items

> **<sup>Syntax</sup>**\
> _Item_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_TypeDefinition_](#type-definitions) \
> &nbsp;&nbsp; | [_Declaration_](#definitions) \
> &nbsp;&nbsp; | [_Definition_](#definitions)

## Definitions

> **<sup>Syntax</sup>**\
> _Declaration_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `dec` [_ID_] `:` [_Type_]
> 
> _Definition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `def` [_ID_] (`:` [_Type_])<sup>?</sup> `=` [_Expression_]

<!--
Alternatively:
> _Definition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `def` [_ID_] (`(` _DefPatternList_ `)`)<sup>\*</sup> `:` [_Type_] `=` [_Expression_]
>
> _DefPatternList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _DefPattern_ (`,` _DefPattern_)<sup>\*</sup> `,`<sup>?</sup>
> &nbsp;&nbsp; | `type` [_ID_List_]
>
> _DefPattern_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_PatternNoTopAlt_] `:` [_Type_]

todo: patterns left of `=`
-->

`def` defines a replicable value usable throughout the file in which it was defined.
```par
// define a static value
dec greeting : String
def greeting = "Hello, World!"
// or all-in-one
def greeting: String = "Hello, World!"

// define a function
def negate: [Bool] Bool = [b] b {
  .true! => .false!
  .false! => .true!
}

// define a function receiving types
dec pop : [type T] [List<T>] (Option<T>) List<T>
def pop = [type T] [list] list {
  .empty! => (.none!) .empty!
  .item(head) tail => (.some(head)) tail
}
```
<!--
(or)
```par
// define a static value
def greeting: String = "Hello, World!"

// define a function
def negate(b: Bool): Bool = b {
  .true! => .false!
  .false! => .true!
}
// equivalent to
def negate2: [Bool] Bool = [b] negate(b)

// define a function receiving types
dec pop : [type T] [List<T>] (Option<T>) List<T>
def pop(type T)(list: List<T>) = list {
  .empty! => (.none!) .empty!
  .item(head) tail => (.some(head)) tail
}
```
-->
Note: In reality, `greeting` has the [replicable]() type `&String`, while `negate` also has a replicable type: `&[Bool] Bool`.

## Type Definitions

> **<sup>Syntax</sup>**\
> _TypeDefinition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `type` [_ID_] _TypeParameters_<sup>?</sup> `=` [_Type_]
>
> _TypeParameters_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` _TypeParameter_ (`,` _TypeParamter_)<sup>\*</sup> `,`<sup>?</sup> `>`
>
> _TypeParameter_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_ID_]

A type definition defines a type alias. (Proposal: Automatically add a tag in some cases for either and choice types)
```par
// simple type alias
type Boolean = Bool

// the definition of Bool
type Bool = either {
  .true!
  .false!
}

// parameterized type alias
type Option<T> = either {
  .none!
  .some T
}
```


[_ID_]: ./lexical.md
[_Type_]: ./types.md
[_PatternList_]: ./patterns.md
[_PatternNoTopAlt_]: ./patterns.md
[_Expression_]: ./expressions.md