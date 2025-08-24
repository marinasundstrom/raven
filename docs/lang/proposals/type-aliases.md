# Proposal: Type aliases

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines type aliases - the `alias` directive.

## Syntax

The `alias` directive appears at the top of a file, either outside or inside a namespace declaration.

```raven
alias SB = System.StringBuilder

alias IntList = System.Collections.Generic.List<int>
```

The name `SB` will be an alias for `System.StringBuilder`.

The type aliased is required to be specified in fully qualified form (`System.StringBuilder`). This is to resolve ambiguities.