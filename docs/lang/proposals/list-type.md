# Proposal: List type syntax

> ⚠️ This proposal has **NOT** been implemented

## Summary

Introduce a built-in `List` type and a shorthand `[]` to denote lists. The bracket form mirrors [collection expressions](collection-expression.md) and produces a `ListTypeSyntax` node.

## Syntax

Explicit form:
```
List<T>
```

Shorthand form:
```
[]
```

## Examples

```raven
let heroes: List<String> = ["Iron Man", "Thor"]
let numbers: [] = [1, 2, 3]

fun printAll(values: []): Unit {
    for v in values {
        Console.WriteLine(v)
    }
}
```

List types can appear in variable declarations, parameters, and return positions. Target typing works the same as with collection expressions.
