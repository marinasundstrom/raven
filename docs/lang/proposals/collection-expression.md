# Proposal: Collection expressions

> ⚠️ 🧩 This proposal has been partly implemented

## Syntax

### Basic syntax

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
````

> ℹ️ Collection expressions are target-typed. But when there is no target type, it should infer array type of the most common base type for the elements. This might change

### Spread operations

Lists can be spread into other lists:

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
let marvel = ["Superman", "Batman", "Flash"]
let comicCharacters = [.. marvel, "Black Widow", .. dc]
```

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