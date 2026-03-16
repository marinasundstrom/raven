# Proposal: Array type syntax

## Summary

Introduce `ArrayTypeSyntax` to express array types using brackets after a type name.

## Syntax

```
Type[]
Type[N]
```

The element type may be a simple or fully qualified name.

## Examples

```raven
let numbers: Int[] = [1, 2, 3]
let fixed: Int[4] = [1, 2, 3, 4]
let names: System.String[] = ["Tony", "Steve"]
let matrix: Int[,] = [[1, 2], [3, 4]]

fun head(values: Int[]): Int {
    return values[0]
}
```

Array types can be used anywhere a type is expected, including variable declarations, parameters, and return types. Multidimensional
arrays use commas inside the brackets (for example, `Int[,]` for a two-dimensional array), and jagged arrays repeat the bracketed
rank specifier (for example, `Int[][,,]`). For one-dimensional arrays, a numeric length inside the brackets declares a fixed-size
array shape (`Int[4]`) that the compiler tracks as a fixed length for conversions, deconstruction, and metadata round-tripping while
still emitting a normal CLR array at runtime.
