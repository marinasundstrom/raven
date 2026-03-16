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
if expr is (int x, val y, 0) {
    // first element must be int (bound as x)
    // second element bound as y
    // third must equal literal 0
}
```

Each element follows the standard `type name` convention.

### Collection and sequence-segment patterns

```raven
if expr is [val first, ..val rest] {
    Console.WriteLine(first)
}

if expr is [..2 val start, val end] {
    Console.WriteLine(start[0] + start[1] + end)
}
```

Plain element patterns consume one element. `..N pattern` consumes a fixed-size
subsequence, while `..pattern` / `...pattern` consume the remaining subsequence.
Inline captures remain explicit, so a segment capture is written `..2 val start`
or `..val rest`.

### Conditional pattern binding

```raven
if val (id, name) = person {
    WriteLine(name)
}
```

This is statement-form sugar for `if person is (val id, val name) { ... }`.
The leading binding keyword is required; Raven does not allow `if Pattern = expr`
without `let` / `val` / `var`.

The same rule applies to nominal deconstruction patterns:

```raven
if val Person(1, name, _) = person {
    WriteLine(name)
}
```

Nominal `Type(...)` patterns are driven by `Deconstruct`. Records support this
directly, and primary-constructor classes/structs with promoted public
parameters synthesize `Deconstruct` in declaration order.

### Property patterns

```raven
if expr is { name as string n, age } {
    Console.WriteLine("Name = ${n}, Age = ${age}")
}
```

Nested bindings use the same `type name` layout. Use `as` to rename a field while binding its value.

## `match` expression and statement

The `match` expression evaluates an input value against a sequence of patterns and returns the result of the first matching arm:

```raven
let result = value match {
    0        => "zero"
    int x    => "an int ${x}"
    _        => "other"
}
```

Raven also supports statement-position `match` as convenience syntax:

```raven
match value {
    0     => Console.WriteLine("zero")
    int x => Console.WriteLine("an int ${x}")
    _     => Console.WriteLine("other")
}
```

Both forms share binding and diagnostics (including exhaustiveness checks). In
statement position, the selected arm expression value is ignored.

Literal-value types from [literal-type unions](literal-types.md) may be used directly in patterns, enabling exhaustive checks over finite sets of values.

When a subtype/case arm is only partially covered by subpatterns, Raven reports
`RAV2110` unless a catch-all arm exists:

```raven
match expr {
    Lit(val value) => value
    Add(val left, val right) => left + right
    Sub(2, val right) => right   // RAV2110 without catch-all
    _ => 0                       // suppresses RAV2110 and closes exhaustiveness
}
```

### Or-patterns and guards

Combine alternatives for a single arm using `or`, and add an expression guard with `when`:

```raven
value match {
    0 or 1            => "zero or one"
    int x when x > 0  => "positive int"
    _                 => "other"
}
```

### Block arm expressions

Arm bodies can be block expressions in both forms:

```raven
val output = value match {
    0 => { "zero" }
    _ => {
        val fallback = "other"
        fallback
    }
}

match value {
    0 => { Console.WriteLine("zero") }
    _ => {
        Console.WriteLine("other")
        ()
    }
}
```
