# Proposal: Collection expressions

> ⚠️ 🧩 This proposal has been partly implemented

## Syntax

Collection expressions provide a terse way to construct arrays and other list-like containers.

### Basic syntax

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
```

> ℹ️ Collection expressions are target-typed. When no target type is available the expression produces an array whose element type is the most specific common base type of all items.

### Empty expressions

An empty collection is written as `[]` and is interpreted according to the target type:

```raven
let numbers: int[] = []
let strings: List<string> = []
```

### Spread operations

Existing collections can be spread into a new expression using the `..` operator, mirroring C#'s spread syntax:

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
let dc = ["Superman", "Batman", "Flash"]
let comicCharacters = [..marvel, "Black Widow", ..dc]
```

Each `..expr` enumerates `expr` and inserts its elements in order. Spread segments may appear anywhere in the expression and multiple segments can be mixed with individual items:

```raven
let mixed = [0, ..marvel, 1, ..dc]
```

If `expr` evaluates to `null` a runtime exception is produced, matching the behavior of C# collection expressions.

### List comprehension

We can use list comprehension to generate sequences from within a collection expression.

We support common expression types inline - provided they return a collection.

```raven
let x = [ for i in 1 .. 10 -> i * 2 ]

// Produces: 1, 4, 6, 10, ...
```

It also works like this:

```raven
let x = [ 1, for i in 1 .. 3 -> i, 42 ]

// Produces: 1, 2, 3, 42

// Where 42 is its own element
```

In this regard, it is similar to the spread operation.

