# Pattern Matching

Raven supports pattern matching to destructure values and test for specific shapes.

## Pattern syntax

Patterns align with Raven's style by placing the type after the bound identifier.

### Variable and type patterns

```raven
if expr is value { /* pattern with type inference */ }
if expr is value: Type { /* value bound when expr is Type */ }
```

The variable name precedes its optional type annotation.

### Constant patterns

```raven
if expr is 0 { /* ... */ }
if expr is "yes" { /* ... */ }
```

### Tuple patterns

```raven
if expr is (x: int, y, 0) { /* ... */ }
```

Each element follows the `name: type` convention.

### Property patterns

```raven
if expr is { name: n: string, age } { /* ... */ }
```

Nested bindings use the same `name: type` layout.

## `match` expression

The `match` expression evaluates an input value against a sequence of patterns and returns the result of the first matching arm:

```raven
match value with
| 0      => "zero"
| x: int => $"int {x}"
| _      => "other"
```

Literal-value types from [literal-type unions](proposals/literal-types.md) may be used directly in patterns, enabling exhaustive checks over finite sets of values.
