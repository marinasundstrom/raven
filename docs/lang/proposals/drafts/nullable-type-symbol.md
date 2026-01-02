# Proposal: Unified nullable type symbol

> ℹ️ This proposal is under consideration

## Summary

Unify nullable handling around a single `NullableTypeSymbol` that represents both nullable reference types and nullable value types. The compiler should treat `IsNullable = true` as the sole indicator that a type is nullable, while `IsValueType` determines whether it represents a CLR `Nullable<T>` (value type) or a nullable reference annotation.

## Motivation

Nullable types are used across Raven's type system, binding, conversion, and code generation. Today, code frequently branches on `NullableTypeSymbol` or requires explicit casts to identify nullable types. We want a consistent, low-friction model that:

* Uses a single symbol shape for nullable types.
* Allows consumers to check nullability without casting.
* Makes nullable value types and nullable reference types behave consistently in conversions and member lookup.
* Aligns emitted IL with the runtime representation: annotations for references, `Nullable<T>` for values.
* Leaves room for a future compatibility mode where `?` is ignored on reference types.

## Goals

* Represent nullable reference and value types with the same `NullableTypeSymbol`.
* Provide a general `IsNullable` indicator on `ITypeSymbol`.
* Ensure `IsNullable = true` + `IsValueType = true` implies a nullable value type (`Nullable<T>`).
* Ensure `IsNullable = true` + `IsValueType = false` implies a nullable reference type annotation.
* Reduce the need for casting to `NullableTypeSymbol` in binder and codegen.
* Keep current surface syntax (`T?` and `T | null`) intact.

## Non-goals

* Introducing new syntax for nullability.
* Changing the meaning of `T?` in source.
* Changing the shape of `Option<T>` or union types.
* Making `T?` behave as `Nullable<T>` for member lookup without an explicit cast or type annotation.

## Proposed design

### 1. Generalize nullable checks on `ITypeSymbol`

Add a default interface member to `ITypeSymbol`:

```csharp
bool IsNullable => TypeKind == TypeKind.Nullable;
```

Provide a shared helper to obtain the underlying type (or `null` if not nullable), e.g.:

```csharp
ITypeSymbol? GetNullableUnderlyingType();
```

This replaces scattered ad-hoc `NullableTypeSymbol` casts.

### 2. Keep `NullableTypeSymbol` as the canonical nullable wrapper

`NullableTypeSymbol` should wrap both reference and value types:

* `UnderlyingType` points to the non-nullable base.
* `IsNullable` is always `true` for `NullableTypeSymbol`.
* `IsValueType` is derived from `UnderlyingType.IsValueType`.

This yields the key rule:

* `IsNullable = true` and `IsValueType = true` → nullable value type (`Nullable<T>`).
* `IsNullable = true` and `IsValueType = false` → nullable reference annotation (`T?`).

### 3. Member lookup on nullable value types

Nullable value types should **not** expose `System.Nullable<T>` members by default. To access `HasValue`, `Value`, or `GetValueOrDefault`, users must cast or explicitly type the value as `Nullable<T>`. This keeps `T?` and `T | null` semantically unified across reference and value types, and avoids special-casing member lookup:

* `x: int?` does not surface `Nullable<int>` members directly.
* `(x as Nullable<int>).HasValue` or `let n: Nullable<int> = x` does.

The compiler should still be able to lower and emit `Nullable<T>` under the hood, but member lookup remains unified via `NullableTypeSymbol`.

When the user explicitly casts or types to `Nullable<T>`, member lookup should align with `System.Nullable<T>`:

* `HasValue`, `Value`, and `GetValueOrDefault` should be available.
* Base type and interfaces should be resolved from the CLR `Nullable<T>` shape, not from the underlying value type's `System.ValueType` base.

This ensures that nullable value types behave like first-class CLR `Nullable<T>` symbols in the compiler.

### 4. Conversion classification updates

Conversions should consider both nullability and value/reference representation:

* Nullable reference types are *annotation-only*; conversions match the underlying reference conversion without runtime wrappers.
* Nullable value types participate in `Nullable<T>` lifting:
  * `T -> T?` is implicit.
  * `T? -> T` is explicit.
  * `T? -> U?` is allowed when `T -> U` is valid (lifted conversion).

Introduce or reuse conversion flags to distinguish lifted conversions so codegen can emit the correct IL.

### 5. Code generation alignment

Codegen should map:

* Nullable reference types to the underlying CLR reference type (plus nullable metadata annotations).
* Nullable value types to `System.Nullable<T>`.

When emitting conversions to a nullable value type, codegen should consider:

* Wrapping non-nullable values into `Nullable<T>`.
* Lifting conversions when both source and destination are nullable value types.
* Nullable union or `Option<T>` conversions, when the language model provides a well-defined lowering to `T?`.

### 6. Normalization consistency

Type normalization should continue to collapse `T | null` into `T?`, making `NullableTypeSymbol` the canonical representation of “type plus null.” This ensures downstream passes always see a single nullable shape.

### 7. Generic constraints and `T?`

Nullability over type parameters must respect generic constraints. The compiler should classify `T?` based on the constraint set and emit diagnostics for invalid combinations:

* `where T : struct` → `T?` is a **nullable value type** and lowers to `System.Nullable<T>`.
* `where T : class` → `T?` is a **nullable reference annotation** and lowers to `T` plus nullable metadata.
* `where T : notnull` → `T?` is **illegal** and should produce a diagnostic explaining that a `notnull` constraint forbids nullable annotations/wrappers.
* Unconstrained `T` → **allowed**, but treated as a **nullable reference annotation** by default unless the language adds a separate rule for “maybe value type” parameters. This keeps `T?` deterministic without needing runtime discrimination.

Enforcement touchpoints:

* Binder constraint validation: `src/Raven.CodeAnalysis/Binder/*`
* Nullable canonicalization and `T | null` folding: `src/Raven.CodeAnalysis/TypeSymbolNormalization.cs`
* Constraint representation and querying: `src/Raven.CodeAnalysis/Symbols/TypeParameterSymbol.cs`

## Affected components

Likely touch points:

* `src/Raven.CodeAnalysis/Symbols/ISymbol.cs` (add `IsNullable` default member)
* `src/Raven.CodeAnalysis/Symbols/Constructed/NullableTypeSymbol.cs` (value-type member forwarding)
* `src/Raven.CodeAnalysis/Compilation.Conversions.cs` (nullable conversion rules)
* `src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs` (nullable conversion emission)
* `src/Raven.CodeAnalysis/TypeSymbolNormalization.cs` (canonicalization checks)
* `src/Raven.CodeAnalysis/Binder/*` (constraint validation)
* `src/Raven.CodeAnalysis/Symbols/TypeParameterSymbol.cs` (constraint representation)

## Open questions

* How should lifted conversions interact with user-defined conversions and union lowering?
* Do we need a dedicated `Conversion.IsLifted` flag to avoid ambiguity in codegen?
* Do we want a switch to disable reference-type `?` annotations for compatibility with .NET libraries, while keeping value-type nullability intact?

## Migration / compatibility

This proposal keeps syntax unchanged and preserves existing nullable semantics. The primary changes are internal to symbol behavior and conversion/codegen pipelines.

The design also accommodates a future compatibility mode where `T?` on reference types is treated as `T` (annotation disabled), while still using `Nullable<T>` for value types.

## Examples

```raven
let x: int? = 5
let y: string? = "hello"

func f(val: int?) -> () { }
func g(text: string?) -> () { }

f(x)           // nullable value type
f(3)           // implicit wrap to int?

g(y)           // nullable reference annotation
```

### Constraint examples

```raven
func f1<T: struct>(value: T?) -> () { }
// T? → System.Nullable<T>

func f2<T: class>(value: T?) -> () { }
// T? → nullable reference annotation

func f3<T: notnull>(value: T?) -> () { }
// Diagnostic: `T?` not allowed for `notnull` constraint

func f4<T>(value: T?) -> () { }
// T? → nullable reference annotation (unless future rule introduces maybe-value-type parameters)
```

## Rationale

A unified nullability model simplifies binder and codegen logic, reduces casts, and aligns the type system with .NET's dual representation of nullable reference and value types.
