# Proposal: Collection expressions

> ⚠️ 🧩 This proposal has been partly implemented

This proposal now documents the remaining design space around collection expressions. The
implemented syntax is specified in [`../spec/language-specification.md`](../spec/language-specification.md)
and [`../spec/grammar.ebnf`](../spec/grammar.ebnf). Any new changes should extend that
shipped surface rather than reintroduce the older draft syntax that previously appeared here.

## Syntax

Collection expressions provide a terse way to construct arrays and other list-like containers.
Bare collection expressions are immutable-by-default; prefixing the literal with `!` selects a mutable default instead. Pipe-delimited literals `[| ... |]` opt into explicit CLR-array syntax.
Collection expressions may also use dictionary-shaped elements to build dictionary-like targets.

### Basic syntax

```raven
let marvel = ["Tony Stark", "Spiderman", "Thor"]
let names = [1, 2, 3]
let scratch = ![1, 2, 3]
let fixed = [|1, 2, 3|]
let lookup = ["a": 1, "b": 2]
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

Dictionary entries follow the same immutable-by-default rule as other collection expressions:

```raven
let lookup = ["a": 1, "b": 2] // ImmutableDictionary<string, int>
let mutableLookup = !["a": 1, "b": 2] // Dictionary<string, int>
```

With an explicit target type, Raven binds through an accessible `Add(key, value)` method:

```raven
let lookup: IReadOnlyDictionary<string, int> = ["a": 1, "b": 2]
let mutable: Dictionary<string, int> = ["x": 10, "y": 20]
```

Dictionary-shaped literals are intentionally homogeneous: once a collection expression contains
dictionary-shaped elements, the whole literal is treated as dictionary-shaped. Supported forms are:

```raven
["a": 1, "b": 2]
[..."a": 1, ...otherMap, "b": 2]
[for item in items => item.Name: item.Value]
[for item in items if item.Enabled => item.Name: item.Value]
```

Plain positional elements, ranges, and value-producing list comprehensions cannot be mixed into the
same dictionary-shaped literal.

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

### Comprehensions

Raven currently supports one `for` clause with an optional `if` filter.

List comprehensions:

```raven
let squares = [for i in 1..10 => i * 2]
let evenSquares = [for i in numbers if i % 2 == 0 => i * i]
let ranged = [for i in 1..<10 => i]
```

Dictionary comprehensions:

```raven
let lengths = [for text in values => text: text.Length]
let filtered = [for item in items if item.Enabled => item.Name: item.Value]
let doubled = [for val (key, value) in pairs => key: value * 2]
```

Current rules:

- `=>` is required before the selector.
- `for ... in source` also accepts range expressions as the source.
- Comprehension targets may be identifiers or patterns. Pattern targets follow the same
  matching rules as `for` statements, so non-matching elements are skipped.
- Outer `let` / `val` / `var` on a pattern-targeted comprehension supplies the binding
  mode for otherwise bare captures.
- A literal becomes dictionary-shaped as soon as it contains a dictionary entry,
  dictionary spread, or dictionary comprehension.
- Dictionary-shaped literals cannot be mixed with positional elements, range elements,
  or value-producing list comprehensions.

Examples of invalid mixtures:

```raven
[1, "a": 2]
[1..3, "a": 2]
[for n in numbers => n, "a": 2]
```

### Remaining design questions

The implemented surface intentionally stays small. Follow-up proposals can extend it from here.

Candidate additions worth evaluating separately:

- identity list comprehensions such as `[for item in items]` and
  `[for item in items if predicate]`
- multiple `for` clauses or local `let` clauses, if Raven later wants a larger query syntax
