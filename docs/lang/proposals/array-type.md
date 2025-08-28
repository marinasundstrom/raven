# Proposal: Array type syntax

> ⚠️ This proposal has **NOT** been implemented

## Summary

Introduce `ArrayTypeSyntax` to express array types using brackets after a type name.

## Syntax

```
Type[]
```

The element type may be a simple or fully qualified name.

## Examples

```raven
let numbers: Int[] = [1, 2, 3]
let names: System.String[] = ["Tony", "Steve"]

fun head(values: Int[]): Int {
    return values[0]
}
```

Array types can be used anywhere a type is expected, including variable declarations, parameters, and return types.
