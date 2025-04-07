# Source Files

> **<sup>Syntax</sup>**\
> _ParFile_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Item_]<sup>*</sup>

This rule is the entry point for every file written in Par.

Currently, every Par program consists entierely of [items](items.md):
- Type definitions
  ```par
  type TypeName = SomeType
  ```

- Value definitions and declarations
  ```par
  // type and value together
  def name: Type = value

  // separate type declaration
  dec name : Type
  def name = value
  ```

[_Item_]: items.md