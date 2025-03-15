# Application Expressions

> **<sup>Syntax</sup>**\
> _Application_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ \
> &nbsp;&nbsp; | [_EitherDestruction_] \
> &nbsp;&nbsp; | [_LoopApplication_]
>
> _Applicable_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_PrimaryExpression_] \
> &nbsp;&nbsp; | [_FunctionCall_] \
> &nbsp;&nbsp; | [_ChoiceSelection_] \
> &nbsp;&nbsp; | [_RecursiveDestruction_] \
> &nbsp;&nbsp; | [_UniversalSpecialization_]

While [constructions](construction.md) construct values, applications destruct them.

## Function Calls

> **<sup>Syntax</sup>**\
> _FunctionCall_ : _Applicable_ `(` [_ExpressionList_] `)`

*<sup>
[Destructs Type](../types.md#function-types)
| [Destructs Expression](construction.md#function-expressions)
| [Statement](../statements/commands.md#todo)
</sup>*

## Choice Selections

> **<sup>Syntax</sup>**\
> _ChoiceSelection_ : _Applicable_ [_Label_]

*<sup>
[Destructs Type](../types.md#choice-types)
| [Destructs Expression](construction.md#choice-constructions)
| [Statement](../statements/commands.md#todo)
</sup>*

## Either Destructions

<!--
> **<sup>Syntax</sup>**\
> _EitherDestruction_ : _Applicable_ `{` (_Pattern_ `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`
-->
> **<sup>Syntax</sup>**\
> _EitherDestruction_ : _Applicable_ `{` ([_Label_] (`(` [_ReceivePatterns_] `)`)<sup>\*</sup> ([ID] | `!`) `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`

*<sup>
[Destructs Type](../types.md#either-types)
| [Destructs Expression](construction.md#either-selections)
| [Statement](../statements/commands.md#todo)
</sup>*

Similar to `match` in Rust or `case` in Haskell.

## Recursive Destructions

> **<sup>Syntax</sup>**\
> _RecursiveDestruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `begin` [_LoopLabel_]<sup>?</sup>
>
> _LoopApplication_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `loop` [_LoopLabel_]<sup>?</sup>

*<sup>
[Destructs Type](../types.md#recursive-types)
| [Statement](../statements/commands.md#todo)
</sup>*

If no loop label is present, `loop` corresponds to the innermost `begin`. Else to the `begin` with the same loop label.

## Universal Specializations

> **<sup>Syntax</sup>**\
> _UniversalSpecialization_ : _Applicable_ `(` `type` [_TypeList_] `)`

*<sup>
[Destructs Type](../types.md#universal-types)
| [Destructs Expression](construction.md#universal-constructions)
| [Statement](../statements/commands.md#todo)
</sup>*


[ID]: ../lexical.md#todo