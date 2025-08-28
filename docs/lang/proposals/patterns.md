# Proposal: Patterns

> ⚠️ 🧩 This proposal has been partly implemented

Raven supports pattern matching to destructure values and test for specific shapes.

## Pattern syntax

Patterns align with Raven's style by placing the type after the bound identifier.

### Variable and type patterns

```raven
if expr is x: int {      // bind only if expr is int
    Console.WriteLine(x + 1)
}
```

The variable name precedes its optional type annotation.

**Optional:**

```raven
if expr is x {           // bind with inferred type
    Console.WriteLine(x)
}
```

### Constant patterns

```raven
if expr is 0 {
    Console.WriteLine("zero")
}

if expr is "yes" {
    confirm()
}
```

### Assignment statement

```raven
if expr is x: 0 {
    // matches only if expr == 0, binds x = 0
}
```

### Tuple patterns

```raven
if expr is (x: int, y, 0) {
    // first element must be int (bound as x)
    // second element bound as y
    // third must equal literal 0
}
```

Each element follows the `name: type` convention.

### Property patterns

```raven
if expr is { name as n: string, age } {
    Console.WriteLine("Name = ${n}, Age = ${age}")
}
```

Nested bindings use the same `name: type` layout. Use `as` to rename a field while binding its value.

## `match` expression

The `match` expression evaluates an input value against a sequence of patterns and returns the result of the first matching arm:

```raven
let result = match value {
    0        => "zero"
    x: int   => "an int ${x}"
    _        => "other"
}
```

Literal-value types from [literal-type unions](literal-types.md) may be used directly in patterns, enabling exhaustive checks over finite sets of values.

### Or-patterns and guards

Combine alternatives for a single arm using `or`, and add an expression guard with `when`:

```raven
match value {
    0 or 1            => "zero or one"
    x: int when x > 0 => "positive int"
    _                 => "other"
}
```
