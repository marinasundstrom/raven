# Span Concepts

Raven's syntax tree exposes several span properties to describe how a node
relates to the original source text. Understanding the differences between
these spans is important when interpreting diagnostics or computing symbol
locations.

## Coordinate validity

`TextSpan(start, length)` requires nonnegative `start` and `length`, and their
sum must fit in `int`. Invalid inputs throw `ArgumentOutOfRangeException`; `End`
never wraps negative for a constructed span. Empty spans, including an empty span
at `int.MaxValue`, are valid. `FromBounds(start, end)` also requires `end >= start`.

## `Span`

`Span` represents the region of source text covered by a node, excluding any
leading or trailing trivia such as whitespace or comments. Terminator tokens
are included.

## `FullSpan`

`FullSpan` includes the node's leading and trailing trivia. It corresponds to
the exact text the parser consumed for the node.

## Line-coordinate values

`LinePositionSpan` is a positional record struct containing `Start` and `End`.
Its coordinates are init-only, and it supplies value equality, hashing,
deconstruction and copy/with semantics. Both maintained compiler implementations
use this contract. Constructor parameter names are `Start` and `End`; positional
construction is unchanged.

`FileLinePositionSpan`, `TextChange` and `TextChangeRange` likewise use record
value semantics. File spans compare the path and both coordinates; changes compare
the span and replacement text; change ranges compare the span and new length.
Their properties are init-only, and constructor parameter names match property
names. `TextChangeRange` retains its length-then-span positional constructor order.
`TextSpan` and `LinePosition` retain their existing explicit value equality,
hashing, ordering and validation. `NoChanges` on the change types exposes an
immutable empty collection rather than a mutable shared list.
