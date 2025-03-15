# Items

> **<sup>Syntax</sup>**\
> _Item_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_TypeDefinition_](#type-definitions) \
> &nbsp;&nbsp; | [_Declaration_](#definitions) \
> &nbsp;&nbsp; | [_Definition_](#definitions)

## Definitions

> **<sup>Syntax</sup>**\
> _Declaration_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `dec` [ID] `:` [_Type_]
> 
> _Definition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `def` [ID] [_Annotation_]<sup>?</sup> `=` [_Expression_]

`def` defines a global definition usable throughout the file in which it was defined.
It can be used as many times as one desires, instantiating itself every time it's used.
```par
// define a static value
dec unit : !
def unit = !
// or all-in-one
def unit: ! = !

// define a function
def negate: [Bool] Bool = [b] b {
  .true! => .false!
  .false! => .true!
}

// define a function receiving types
dec pop : [type T] [List<T>] (Option<T>) List<T>
def pop = [type T] [list] list {
  .empty! => (.none!) .empty!
  .item(head) tail => (.some head) tail
}
```

## Type Definitions

> **<sup>Syntax</sup>**\
> _TypeDefinition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `type` [ID] _TypeParameters_<sup>?</sup> `=` [_Type_]
>
> _TypeParameters_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` _TypeParameter_ (`,` _TypeParamter_)<sup>\*</sup> `,`<sup>?</sup> `>`
>
> _TypeParameter_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [ID]

A type definition defines a type alias, not a "new type". All types in Par are structural. <!--(Proposal: Automatically add a tag in some cases for either and choice types)-->
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


[ID]: ./lexical.md
[_Type_]: ./types.md
[_PatternList_]: ./patterns.md
[_PatternNoTopAlt_]: ./patterns.md
[_Expression_]: ./expressions.md