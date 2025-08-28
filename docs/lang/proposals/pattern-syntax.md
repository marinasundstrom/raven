# Proposal: Pattern syntax

> ⚠️ This proposal has NOT been implemented

This document outlines a reimplementation of pattern matching syntax that fits Raven's style. The existing prototype borrows C#'s form (`if x is int i`) where the type occupies the center of the pattern. Raven favors putting type information after identifiers, as in parameter lists and variable bindings. Patterns should follow the same convention.

## Purpose

Align pattern declarations with the rest of the language and remove the C#-centric `is Type id` form. Patterns should read from the binding first and then optionally annotate its type.

## Syntax

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

## Future directions

The same pattern forms will apply inside a future `match` expression and other pattern contexts.

## Questions

- Should type annotations be required for tuple elements when needed for disambiguation?
- How should discards (`_`) interact with typed variables (`_: Type`)?
