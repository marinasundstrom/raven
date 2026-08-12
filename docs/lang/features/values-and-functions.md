# Values, functions, and control flow

Raven code usually begins with values and plain functions. A function does not
need a class wrapper when it represents a standalone operation.

## Choose immutable or mutable bindings

Use `let` when a name will continue to refer to the same value. Use `var` when
reassignment is part of the algorithm:

```raven
let greeting = "Hello"
var attempts = 0

attempts = attempts + 1
```

`let` does not make an object deeply immutable. It means the local name itself
will not be reassigned.

## Write a function

Declare a function with `func`. Parameter and return types make its boundary
visible, while local types can often be inferred:

```raven
func ShippingCost(weightKg: decimal) -> decimal {
    let basePrice = 12.50m
    return basePrice + weightKg * 1.75m
}
```

Use a top-level function for parsing, validation, formatting, lookup, or a
workflow step that has no natural owning object.

## Use decisions as values

An `if` can produce a value:

```raven
let label = if attempts == 0 {
    "Ready"
} else {
    "Retrying"
}
```

Use the statement form when the branches perform effects instead of producing
a value.

## Repeat work deliberately

Use `for` to iterate a sequence, `while` when a condition controls repetition,
and `loop` for an intentionally unconditional loop:

```raven
for name in names {
    Console.WriteLine(name)
}

loop {
    PollDevice()
    Thread.Sleep(1000)
}
```

Continue with [data modeling](data-modeling.md) when values need domain types,
or [patterns](patterns.md) when a decision depends on a value's shape.
