# Notation

## Grammar

The grammar of Par is declared in _Lexer_ and _Syntax_ blocks. It uses the following notation:

- Sequences of capital letters like DIGIT represent lexer tokens

- Italic names in CamelCase like _Item_ represent parser nonterminals

- Character sequences in monospace like `type` represent literals

- Characters may be represented using escape sequences, like \\n

- - x<sup>?</sup> means x, zero or once
  - x<sup>*</sup> means x, zero or more
  - x<sup>+</sup> means x, once or more
  - x<sup>a..b</sup> means at least a and at most b of x

- x | y means either x or y

- [ and ] group characters together, like [`x` `X`] or [`a`-`z`]

- character sequences (and groups of them) can be negated by ~, like ~\\n

- ( and ) group arbitrary rules for precedence