# Proposal: Patterns

> ⚠️ 🧩 This proposal has been partly implemented

Raven supports pattern matching to destructure values and test for specific shapes.

## Pattern syntax

Patterns align with Raven's type annotation style by placing the type before the
bound identifier.

### Variable and type patterns

```raven
if expr is int x {      // bind only if expr is int
    Console.WriteLine(x + 1)
}
```

The type precedes the variable name. Future work may allow the type to be
inferred (`if expr is value { ... }`), but the current implementation requires an
explicit type annotation.

### Constant patterns

```raven
if expr is 0 {
    Console.WriteLine("zero")
}

if expr is "yes" {
    confirm()
}
```

### Typed discards

```raven
if expr is int _ {
    // matches any int while ignoring the value
}
```

### Tuple patterns

```raven
if expr is (int x, y, 0) {
    // first element must be int (bound as x)
    // second element bound as y
    // third must equal literal 0
}
```

Each element follows the standard `type name` convention.

### Property patterns

```raven
if expr is { name as string n, age } {
    Console.WriteLine("Name = ${n}, Age = ${age}")
}
```

Nested bindings use the same `type name` layout. Use `as` to rename a field while binding its value.

## `match` expression

The `match` expression evaluates an input value against a sequence of patterns and returns the result of the first matching arm:

```raven
let result = match value {
    0        => "zero"
    int x    => "an int ${x}"
    _        => "other"
}
```

Literal-value types from [literal-type unions](literal-types.md) may be used directly in patterns, enabling exhaustive checks over finite sets of values.

### Or-patterns and guards

Combine alternatives for a single arm using `or`, and add an expression guard with `when`:

```raven
match value {
    0 or 1            => "zero or one"
    int x when x > 0  => "positive int"
    _                 => "other"
}
```
