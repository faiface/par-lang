# Lexical Structure

## Comments

> **<sup>Lexer</sup>**\
> LINE_COMMENT :\
> &nbsp;&nbsp; &nbsp;&nbsp; `//` (\~\\n)<sup>*</sup>
>
> BLOCK_COMMENT :\
> &nbsp;&nbsp; &nbsp;&nbsp; `/*` (BLOCK_COMMENT | \~`*/`)<sup>*</sup> `*/`

Comments have no effect on the program.
```par
// This is a line comment

/*
 * And this is a multiline (block) comment.
 * The stars on the left are purely for aesthetics
 */
```

## Names

> **<sup>Lexer</sup>**\
> ID :\
> &nbsp;&nbsp; &nbsp;&nbsp; (ID_START ID_CONT<sup>*</sup>)<sub>_Expect [keywords](#keywords)_</sub>
>
> ID_START :\
> &nbsp;&nbsp; &nbsp;&nbsp; [`a`-`z` `A`-`Z`]
>
> ID_CONT :\
> &nbsp;&nbsp; &nbsp;&nbsp; [`_` `a`-`z` `A`-`Z` `0`-`9`]

> **<sup>Syntax</sup>**\
> _ID_List_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; ID (`,` ID)<sup>*</sup> `,`<sup>?</sup>

Some valid identifiers are:
- `snake_case`
- `PascalCase`
- `letters123`

Some invalid identifiers are:
- `chan`, a keyword
- `3D`, starts with a number
- `kebab-case`, same as `kebab - case`

## Keywords

| Keyword | Usage |
|---------|-------|
| `type` | [Define a type](items.md#type-definitions), Existentials, Universals |
| `dec` | [Declare](items.md#definitions) a value |
| `def` | [Define](items.md#definitions) a value |
| `chan` | [Channel expressions](expressions.md#channel-expressions), [Dualize types](types.md#channel-types) |
| `let` | Let [expressions](expressions.md#let-expressions) and [statements](statements.md#let-statements) |
| `do` | [Do expressions](expressions.md#do-expressions) |
| `in` | [Let expressions](expressions.md#let-expressions), [Do expressions](expressions.md#do-expressions) |
| `begin`, `loop` | [Iterative constructions](./expressions/construction.md#iterative-constructions), Recursive destruction ([expression](./expressions/application.md#recursive-destructions) or [statement](./statements/commands.md#recursive-commands)) |
| `either` | [Either types](types.md#either-types) |
| `recursive` | [Recursive types](types.md#recursive-types) |
| `iterative` | [Iterative types](types.md#iterative-types) |
| `self` | [Recursive](types.md#recursive-types) and [iterative](types.md#iterative-types) types |
| `unfounded` | Escape totality checker in recursive [expressions](./expressions/application.md#recursive-destructions) and [statements](./statements/commands.md#recursive-commands) |

## Punctuation

| Symbol | Usage |
|--------|-------|
| `=` | Definitions ([values](items.md#definitions) and [types](items.md#type-definitions)), Let [expressions](expressions.md#let-expressions) and [statements](statements.md#let-statements) |
| `:` | [Type annotations](types.md), Value [declarations](items.md#definitions), [Loop labels](./statements/commands.md#recursive-commands) |
| `,` | Various enumerations |
| `!` | Unit [type](./types.md#the-unit-type), [expression](./expressions/construction.md#the-unit-expression), [pattern](./patterns.md#the-unit-pattern), [Break command](./statements/commands.md#the-break-command) |
| `?` | [Bottom type](./types.md#the-bottom-type), [Continue command](./statements/commands.md#the-continue-command) |
| `.` | Labels of either or choice types, used in various places
| `<>` | [Link commands](./statements/commands.md#link-commands) |
| `<` | Type [parameters](items.md) and [arguments](types.md) |
| `>` | Type [parameters](items.md) and [arguments](types.md) |
| `=>` | Choice [types](./types.md#choice-types) and [constructions](./expressions/construction.md#choice-constructions), Match [expressions](./expressions/application.md#match-expressions) and [commands](./statements/commands.md#match-commands) |
| `(` ... `)` | Pairs, function application |
| `[` ... `]` | Functions, pair destruction |
| `{` ... `}` | Either and choice types, processes |

## Whitespace

Whitespace in Par serves merely the purpose of separating tokens.
Whitespace characters are:
- The whitespace `' '`
- Tabs `'\t'`, `'\v'`
- Newline `'\n'`
- Carriage return `'\r'`
