# Proposal: Ranges

Endpoint syntax is shared across contexts: `..` excludes the end and `..=` includes
it. The [generic-bounds review](../consistent-ranges.md) covers subsequent capability
and library work.

> ✅ Implemented. From-end indices and ranges now produce `System.Index` and
> `System.Range` values, with support for start-bound ranges in `for`
> expressions.

A range describes bounds. Slicing uses those bounds to select part of a
collection; iteration uses them to delimit a progression. These operations are
not pattern matching.

## Range expressions and slicing

A standalone range expression produces a `System.Range` on .NET. A range can
also be passed to an indexer or method accepting `Range`.

```raven
let items = [|1, 2, 3, 4, 5, 6|]
let firstTwo = items[..2]
let fromFourth = items[3..]
let lastThree = items[^3..]
let throughThird = items[..=2]
```

`^n` is a from-end index. `^1` identifies the last element; `^0` is the boundary
after it and cannot be an included endpoint.

## Range patterns and sequence patterns

A range pattern such as `value is 1..=3` tests whether a value lies between bounds.
Sequence-pattern prefixes such as `..2` (a fixed segment) and `...let rest` (the remaining
elements) are separate constructs. Collection spreads use `...items`.
See [range and sequence patterns](../../spec/fundamental-patterns.md) and the
[compiler terminology](../../../compiler/range-endpoints.md#terminology).

## Iteration and progressions

Ranges can be used in `for` expressions when both bounds count from the start.
The start value defaults to `0` when omitted, and the upper bound is exclusive unless `..=` is used.
`for`-range iteration supports integral, floating-point, `char`, and `decimal`
boundaries.

```raven
import System.Console.*

for item in 2..=5 {
    WriteLine(item * 2)
}
```

The range expression is translated into inclusive iteration that visits `2`,
`3`, `4`, and `5`. From-end bounds are rejected in `for` loops because the loop
has no length context.

`for` ranges also support an optional `by` clause:

```raven
for x in 0..=10 by 2 {
    WriteLine(x)
}

for x in 10..=0 by -3 {
    WriteLine(x)
}

for x in 0..=10.0 by 0.1 {
    WriteLine(x)
}
```

The step must be non-zero. Positive steps iterate upward and negative steps
iterate downward. For `..=`, the end comparison is inclusive (`<=` or `>=`);
for `..`, it is exclusive (`<` or `>`).

## Notes

* In .NET, is supported via the `System.Range` type. It works with `Span` also.
