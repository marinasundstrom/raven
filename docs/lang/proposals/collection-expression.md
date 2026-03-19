# Proposal: Collection expressions

> ⚠️ 🧩 This proposal has been partly implemented

## Syntax

Collection expressions provide a terse way to construct arrays and other list-like containers.
Bare collection expressions are immutable-by-default; prefixing the literal with `!` selects a mutable default instead. Pipe-delimited literals `[| ... |]` opt into explicit CLR-array syntax.

### Basic syntax

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
let names = [1, 2, 3]
let scratch = ![1, 2, 3]
let fixed = [|1, 2, 3|]
```

> ℹ️ Collection expressions are target-typed. When no target type is available, plain literals infer list-family defaults: bare literals fall back to `ImmutableList<T>`, `![...]` literals fall back to `List<T>`, and `[| ... |]` literals fall back to ordinary CLR arrays. Target-typed `[...]` expressions can still bind to arrays when the surrounding type requires one.

Separator-less forms such as `[]` and `![value]` still rely on target typing when Raven cannot infer an element type.

### Empty expressions

An empty collection is written as `[]` and is interpreted according to the target type:

```raven
let numbers: int[] = []
let strings: List<string> = []
```

`![]` is also target-typed because the literal still does not provide enough information to infer an element type on its own.

### Default inference

Without an explicit target type, collection expressions infer list-family defaults:

```raven
let values = [1, 2, 3]    // inferred as ImmutableList<int>
let scratch = ![1, 2, 3]  // inferred as List<int>
```

Use `[| ... |]` when you want explicit array defaults and fixed-length array inference:

```raven
let values = [|1, 2, 3|]    // inferred as int[3]
let expanded = [|...values, 4|]  // inferred as int[4]
```

If you want another concrete collection type, provide that type explicitly:

```raven
let values: int[] = [1, 2, 3]
let queue: Queue<int> = [1, 2, 3]
```

### Spread operations

Existing collections can be spread into a new expression using the `...` operator:

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
let dc = ["Superman", "Batman", "Flash"]
let comicCharacters = [...marvel, "Black Widow", ...dc]
```

Each `...expr` enumerates `expr` and inserts its elements in order. Spread segments may appear anywhere in the expression and multiple segments can be mixed with individual items:

```raven
let mixed = [0, ...marvel, 1, ...dc]
```

Bare range elements also expand inline. Inside a collection expression, `a..b` and
`a..<b` contribute the produced sequence rather than a single `Range` value:

```raven
let values: int[] = [1..3]          // [1, 2, 3]
let mixed: int[] = [1, 3..4, 9]     // [1, 3, 4, 9]
let exclusive: int[] = [1..<4]      // [1, 2, 3]
```

If `expr` evaluates to `null` a runtime exception is produced, matching the behavior of C# collection expressions.

#### Spread inference (no explicit target)

When no target type is present:

* Spread operands contribute their element types to inference.
* The resulting collection still defaults to `ImmutableList<T>` or `List<T>` depending on whether the literal is bare or prefixed with `!`.
* `[| ... |]` switches that targetless fallback to CLR arrays.
* If you want another collection type, add an explicit target type.

```raven
let list: ImmutableList<int> = [2; 3; 4]
let inferred = [7, ...list, 5] // ImmutableList<int>

let a: ImmutableList<int> = [1]
let b: List<int> = [2]
let x = [...a, ...b] // ImmutableList<int>
let y: int[] = [...a, ...b] // ok
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
