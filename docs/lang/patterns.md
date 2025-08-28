# Pattern Matching

Raven supports pattern matching to destructure values and test for specific shapes. Patterns are described in the [pattern syntax proposal](proposals/pattern-syntax.md) and appear in several contexts.

## `match` expression

The `match` expression evaluates an input value against a sequence of patterns and returns the result of the first matching arm:

```raven
match value with
| 0 => "zero"
| x: int => $"int {x}"
| _ => "other"
```

Literal-value types from [literal-type unions](proposals/literal-types.md) may be used directly in patterns, enabling exhaustive checks over finite sets of values.

