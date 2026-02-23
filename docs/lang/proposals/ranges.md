# Proposal: Ranges

> ✅ Implemented. From-end indices and ranges now produce `System.Index` and
> `System.Range` values, with support for start-bound ranges in `for`
> expressions.

This proposal outlines ranges. This feature allows you to specify a range that can be passed much like indices to any list or array type to obtain items within that range.

## Syntax

Alone range syntax produces a `Range`.

```raven
let range: Range = ..2
```

But used with collections they are passed like indices in various contexts, such as to indexers and methods accepting `Range`:

```raven
let no = [1,2,3,4,5,6]
let set1 = list[..2]
````

### Patterns

Here are some common range pattern:

```raven
let no = [1,2,3,4,5,6]
let set1 = list[..2] // Select all up to the 2nd item
let set2 = list[3..] // Select from 3rd item to the 
let set2 = list[^..3] // Select from the 3 last items from the end
let set2 =  [^2..^0]
```

* `^` is the "from end" operator, and the symbol is commonly known as "hat".

## `For` support

Ranges can be used in `for` expressions when both bounds count from the start.
The start value defaults to `0` when omitted, and the upper bound is inclusive.
`for`-range iteration supports integral, floating-point, `char`, and `decimal`
boundaries.

```raven
import System.Console.*

for item in 2..5 {
    WriteLine(item * 2)
}
```

The range expression is translated into inclusive iteration that visits `2`,
`3`, `4`, and `5`. From-end bounds are rejected in `for` loops because the loop
has no length context.

`for` ranges also support an optional `by` clause:

```raven
for x in 0..10 by 2 {
    WriteLine(x)
}

for x in 10..0 by -3 {
    WriteLine(x)
}

for x in 0..10.0 by 0.1 {
    WriteLine(x)
}
```

The step must be non-zero. Positive steps iterate upward (`<= end`) and
negative steps iterate downward (`>= end`).

## Notes

* In .NET, is supported via the `System.Range` type. It works with `Span` also.
