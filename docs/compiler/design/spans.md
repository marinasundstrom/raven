# Span Concepts

Raven's syntax tree exposes several span properties to describe how a node
relates to the original source text. Understanding the differences between
these spans is important when interpreting diagnostics or computing symbol
locations.

## `Span`

`Span` represents the region of source text covered by a node, excluding any
leading or trailing trivia such as whitespace or comments. Terminator tokens
are included.

## `FullSpan`

`FullSpan` includes the node's leading and trailing trivia. It corresponds to
the exact text the parser consumed for the node.

## `EffectiveSpan`

`EffectiveSpan` is similar to `Span` but trims a trailing newline terminator
token, if present. This is used when reporting diagnostics or determining
symbol locations so that the trailing newline is not highlighted.

