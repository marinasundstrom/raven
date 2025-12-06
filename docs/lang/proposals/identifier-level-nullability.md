# Proposal: Identifier-Level Nullability

## Summary

Move Raven’s primary nullability declaration from the *type* to the *declared symbol* by placing `?` after the identifier in declarations. For example:

```raven
let x? : int
```

declares `x` as nullable and desugars to a nullable type for `x` (e.g. `NullableTypeSymbol(int)` for value types) while keeping the annotated type syntax (`int`) non-nullable. The compiler exposes this via an `IsNullable` flag on declared symbols and adjusts the effective type accordingly. Declaring nullability both on the identifier and the type is disallowed:

```raven
let x? : int? // error
```

This aligns Raven’s model with .NET’s underlying `NullableAttribute` encoding, which is attached to the declared symbol rather than the type itself.

## Motivation

Nullability in .NET is conceptually tied to **declared symbols**: fields, properties, parameters, locals, and returns carry nullability via `NullableAttribute` and `NullableContextAttribute`. Treating nullability as a property of the *type syntax* obscures this and leads to confusion such as “is `int?` the same as `x?`?” or “where does the attribute actually go?”

By shifting focus to identifier-level nullability, we can:

* Match how .NET actually encodes nullability in metadata.
* Make it explicit that *the variable/field/parameter* is nullable, not the “bare” type.
* Provide a clear, uniform place for nullability semantics (symbols) that plays nicely with flow/data-flow analysis.
* Avoid double-annotated constructs (`int?` plus a nullable symbol) that are hard to reason about.

## Design

### Surface syntax

* Nullability is declared by appending `?` to the **identifier** in declarations:

  ```raven
  let x? : int
  field value? : MyStruct
  func f(x? : int, y : string) -> int
  property Name? : string
  ```

* The annotated type (`int`, `MyStruct`, `string`, etc.) is written in its **non-nullable** form for this feature.

* Double-nullability is rejected:

  ```raven
  let x? : int?    // illegal
  func f(x? : int?) // illegal
  ```

* Existing type-level nullability (`T?`) remains valid where needed (especially for generic arguments and nested types):

  ```raven
  let list : List<int?>  // still allowed
  ```

* For *simple* declarations, `x? : T` is considered the canonical form going forward, with `T?` being primarily used inside compound types.

### Symbol model

* Introduce an `IsNullable` flag on declared symbols:

  * `ILocalSymbol.IsNullable`
  * `IParameterSymbol.IsNullable`
  * `IFieldSymbol.IsNullable`
  * `IPropertySymbol.IsNullable`
  * (Later) `IMethodSymbol.ReturnsNullable` or similar for return values.

* Semantics:

  * `IsNullable == true` means the **declaration** allows `null` for that symbol.
  * The **effective type** of the symbol is adjusted:

    * For value types, the compiler wraps the underlying type in a `NullableTypeSymbol` (backed by `System.Nullable<T>`).
    * For reference types, `IsNullable` captures their nullability state; the exact representation (`NullableReferenceTypeSymbol` vs state on symbol) remains an internal design choice.

### Binding rules

When binding a declaration like:

```raven
let x? : int
```

the binder:

1. Binds the type syntax `int` to a **non-nullable** `ITypeSymbol`.

2. Sees the nullable marker on the identifier (`x?`).

3. Sets `IsNullable = true` on the resulting symbol.

4. Computes the symbol’s effective type:

   * If underlying type is a non-nullable value type, wraps with `NullableTypeSymbol`.
   * If underlying type is already nullable (`NullableTypeSymbol`, or `T?`), reports a diagnostic:

     > Nullability cannot be specified both on the variable and the type.

5. For reference types, uses `IsNullable` to represent the nullability of that reference in semantic and flow analysis.

These rules apply uniformly to locals, fields, parameters, and properties (and later, pattern variables, deconstruction, etc.).

### Metadata representation

Identifier-level nullability is largely *syntactic and semantic sugar* over the existing .NET nullability encoding:

* Value types:

  * Effective type is emitted as `System.Nullable<T>` when `IsNullable` is true.
* Reference types:

  * The type remains the same (e.g. `System.String`), but the compiler emits `NullableAttribute` (and/or `NullableContextAttribute`) on the declared symbol to represent its nullability.
* IL and metadata continue to expose standard .NET types; Raven tooling and debugger displays are free to show the nicer identifier-level syntax (`let x? : int`) when mapping back to source.

### Examples

```raven
// Locals
let count? : int    // nullable int
let name  : string  // non-nullable string

// Parameters and returns
func tryFind(key: string, value? : string) -> bool

// Properties and fields
property Title? : string
field payload? : MyStruct

// Interplay with type-level nullability in generics
let cache : Dictionary<string, int?>
let current? : Dictionary<string, int>  // nullable symbol holding a non-nullable dictionary
```

## Implementation notes

* **Parser**: Extend declaration grammars so that identifiers may carry an optional trailing `?` before the `:` type separator. Treat this as a nullable flag on the declarator rather than part of the type syntax.
* **Symbols**: Add `IsNullable` to declared symbol interfaces and implementations. Ensure symbol constructors (for source) accept an `isNullable` flag and that metadata-backed symbols read nullability from `NullableAttribute` where available.
* **Type system**: Reuse `NullableTypeSymbol` (or equivalent) for value types and enforce that nested/nullability-of-nullable combinations are not created.
* **Diagnostics**: Introduce a nullability diagnostic for “double nullability” (identifier **and** type), e.g. `let x? : int?`. Optionally, in the future, warn when using simple `T?` where `x? : T` is preferred.
* **Semantic model**: `GetTypeInfo` should return the effective nullable type for expressions referring to nullable symbols. Tooling (Quick Info, navigation) should include both the type and the symbol’s `IsNullable` state, formatting it in Raven’s identifier-level notation.
* **Flow analysis**: Where Raven tracks null-state, use `IsNullable` as the declaration’s initial state—non-nullable symbols can be checked more strictly against `null` assignments and uses, while nullable ones allow `null` without diagnostics.
