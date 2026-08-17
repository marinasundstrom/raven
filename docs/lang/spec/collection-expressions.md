# Collection expressions

Collection expressions create arrays, lists, dictionaries, spans, and other
collection types with a compact bracket syntax.

```raven
let numbers = [1, 2, 3]
let names: List<string> = ["Ada", "Lin", "Grace"]
let scores = ["Ada": 10, "Lin": 12]
```

Elements are evaluated from left to right. A trailing comma is allowed, and
elements can be separated by commas, semicolons, or newlines:

```raven
let colors = [
    "red"
    "green"
    "blue"
]
```

## Immutable, mutable, and array forms

When no surrounding type determines the collection kind, Raven uses the
literal's opening syntax to choose a default:

| Form | Default type |
| --- | --- |
| `[a, b]` | `ImmutableList<T>` |
| `![a, b]` | `List<T>` |
| `[|a, b|]` | CLR array `T[N]` or `T[]` |

```raven
let immutable = [1, 2, 3]       // ImmutableList<int>
let mutable = ![1, 2, 3]        // List<int>
let array = [|1, 2, 3|]         // int[3]
```

The same defaults apply when elements use newline or semicolon separators. The
`!` marker makes mutable collection creation intentional; an explicit target
type can choose a different mutable or immutable collection without it:

```raven
let queue: Queue<int> = [1, 2, 3]
let list: List<int> = [1, 2, 3]
```

## Spreading existing collections

Prefix an element with `...` to enumerate another collection and insert all of
its values at that position:

```raven
let middle = [2, 3, 4]
let combined = [1, ...middle, 5]
let array = [|0, ...middle, 6|]
```

The spread source must implement `System.Collections.IEnumerable`, either
directly or through a generic interface such as `IEnumerable<T>`. Its elements
must be convertible to the destination element type. A source that is not
enumerable reports `RAV2022`.

A bare range element expands in the same way:

```raven
let inclusive = [1..3]       // 1, 2, 3
let halfOpen = [1..<4]       // 1, 2, 3
let surrounded = [0, 1..3, 4]
```

Here `..` includes the upper bound and `..<` excludes it. The range contributes
its values rather than a single `System.Range` object.

## Collection comprehensions

A comprehension produces one collection element for each value from a source:

```raven
let squares = [for n in numbers => n * n]
let evenSquares = [for n in numbers if n % 2 == 0 => n * n]
let rangeSquares = [for n in 1..10 => n * n]
```

The optional `if` filters values before the selector runs. The source may be a
range or any value accepted by `for`.

The iteration target can be a pattern:

```raven
let entries = [(1, "Ada"), (2, "Bob")]
let selected = [for let (2, name) in entries => name]
```

Pattern targets follow the same rules as pattern-targeted `for` statements.
Elements that do not match are skipped. The filter runs only after the pattern
has matched, and the outer `let`, `val`, or `var` supplies the binding mode for
otherwise bare captures.

Comprehensions are implemented as collection-building loops and use the same
target typing and element conversions as ordinary collection elements.

## Dictionary expressions

Write `key: value` to create a dictionary entry:

```raven
let byName = ["Ada": 10, "Lin": 12]
let mutableByName = !["Ada": 10, "Lin": 12]
```

Without a target type, the bare form produces
`ImmutableDictionary<TKey, TValue>` and the `!` form produces
`Dictionary<TKey, TValue>`.

Dictionary expressions support spreads and comprehensions:

```raven
let more = ["Grace": 14]
let merged = [..."Ada": 10, ...more, "Lin": 12]

let lengths = [for key in [|"a", "bb"|] => key: key.Length]
let doubled = [for let (key, value) in [("a", 1), ("b", 2)] =>
    key: value * 2]
```

`...key: value` inserts one entry in spread position. A dictionary spread must
provide values compatible with
`IEnumerable<KeyValuePair<TKey, TValue>>` after key and value conversions.

If any element has dictionary shape, the whole expression is dictionary-shaped.
It cannot mix dictionary entries with positional elements, range elements, or a
comprehension that produces only values.

## Target typing

The surrounding context can determine both the collection type and its element
type:

```raven
let numbers: int[] = [1, 2, 3]
let names: List<string> = ["Ada", "Lin"]
let lookup: IReadOnlyDictionary<string, int> = ["a": 1, "b": 2]
let span: System.Span<int> = [1, 2, 3]
let readOnly: System.ReadOnlySpan<int> = [1, 2, 3]
```

### Array targets

For a target `T[]`, Raven allocates a one-dimensional array and converts every
element to `T`. A spread must enumerate values convertible to `T`.

For a fixed-length target `T[N]`, Raven checks the element count whenever it can
prove it statically. Ordinary elements contribute one item, and a spread from a
fixed-length `T[M]` contributes `M`. A proven mismatch is a compile-time error.

Multidimensional arrays such as `T[,]` are not created by collection-expression
syntax. Create them through a runtime API or another existing value, then use
normal indexing and assignment.

Span targets use array-backed storage, so the resulting `Span<T>` or
`ReadOnlySpan<T>` remains valid for its normal scope. Both ordinary and spread
elements are supported.

### Collection targets

A non-array target must have an accessible parameterless constructor and an
instance `Add` method. Raven constructs the target and calls `Add` for each
element. The method's parameter type determines element conversions.

A dictionary target uses the same builder model but requires an accessible
`Add(key, value)` method. Keys and values are converted to its two parameter
types.

### Inference without a target

Without an expected type, Raven finds the best common element type. Spreads and
ranges contribute their element types. Numeric conversions can produce a common
type:

```raven
let values = [1, 2.0] // ImmutableList<double>
```

Inference does not fall back to `object`, `System.ValueType`, or an interface
merely to make unrelated elements fit. Supply an explicit target when that
heterogeneous shape is intentional:

```raven
let values: object[] = [1, true]
```

Dictionary expressions infer key and value types separately.

Collection element types also participate in generic method inference before
overload resolution supplies a final collection target. For example,
`Task.WhenAll([Task.FromResult(1)])` infers the result type `int` instead of
first widening the element to the non-generic `Task` type.

An empty collection expression has no element type to infer and therefore
requires a target:

```raven
let names: string[] = []
let queue: Queue<int> = []
let span: System.Span<byte> = []
```

With a target, Raven creates the corresponding empty array, initialized
collection, or empty span.

## Element access

Use brackets after a collection value to read an element through its array or
indexer behavior:

```raven
let values = [1, 42, 3]
let answer = values[1]
```

Index and range access are described under [Index, range, and bitwise
operators](operators.md).
