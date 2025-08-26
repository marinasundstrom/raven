# Proposal: Aliases

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines aliases - which allow you to define alternative names for
namespaces, types (including closed generic types, tuples, and type unions), and static members.

## Syntax

The `alias` directive appears at the top of a file, either outside or inside a
namespace declaration.

### Namespace alias

```raven
alias IO = System.IO
```

The name `IO` will be an alias for the `System.IO` namespace.

### Type alias

```raven
alias SB = System.StringBuilder

alias IntList = System.Collections.Generic.List<int>
alias Pair = (int, string)
alias Number = int | string
```

The name `SB` will be an alias for `System.StringBuilder`.

The type aliased is required to be specified in fully qualified form
(`System.StringBuilder`) unless it is a type expression such as a tuple or
type union. This is to resolve ambiguities.

### Member alias

For static members such as methods, fields, or properties. Aliasing a method
binds a specific overload; repeat the directive with the same alias name to
alias additional overloads.

```raven
alias PrintLine = System.Console.WriteLine

PrintLine("Test")
```

