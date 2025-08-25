# Proposal: Aliases

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines aliases - which allows you to define alternative names for types (incl. closed generic types) and static members.

## Syntax

The `alias` directive appears at the top of a file, either outside or inside a namespace declaration.

### Type alias

```raven
alias SB = System.StringBuilder

alias IntList = System.Collections.Generic.List<int>
```

The name `SB` will be an alias for `System.StringBuilder`.

The type aliased is required to be specified in fully qualified form (`System.StringBuilder`). This is to resolve ambiguities.

### Member alias

For static members such as: methods, fields, or properties.

```raven
alias PrintLine = System.Console.WriteLine

PrintLine("Test")
```