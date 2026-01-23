# Nullable control flow analysis and `GetTypeInfo` API

## Goals
- Implement `GetTypeInfo` with nullable flow awareness, aligning with Raven's unified nullability model for reference and value types.
- Treat unannotated external APIs as nullable by default to avoid unsound assumptions.
- Simplify nullable symbol handling so flow analysis and diagnostics use a consistent representation.

## Current constraints and observations
- Raven enforces null checks uniformly for nullable reference and value types; nullable locals must be proven non-null before member access.
- Some dependencies lack `NullableContext` metadata; Raven needs a conservative default (nullable) when metadata is missing.
- `NullableTypeSymbol` currently wraps types with special-case handling, which makes flow analysis harder to reason about and introduces duplication.

## Proposed API and model changes
### 1) Implement `GetTypeInfo`
`GetTypeInfo` should surface both the declared type and effective nullable type for an expression, accounting for flow state:
- **Declared type**: what the syntax declares or the symbol's underlying type.
- **Flow type**: the declared type decorated with current null-state (nullable vs. non-null) after control-flow analysis.
- **Effective nullable type**: a helper that expresses the "nullable view" of a type when external annotations are missing.

Suggested shape (names illustrative):
```csharp
public readonly struct TypeInfo
{
    public ITypeSymbol DeclaredType { get; }
    public ITypeSymbol FlowType { get; }
    public ITypeSymbol EffectiveNullableType { get; }
}
```

`GetTypeInfo` should:
1. Query the bound node's type (declared).
2. Apply flow-state (nullable vs. non-null) to produce `FlowType`.
3. Replace any missing metadata context with a nullable decoration via `EffectiveNullableType`.

### 2) Treat missing nullable metadata as nullable by default
When a referenced symbol lacks nullable context, assume it is nullable for analysis:
- This should apply to return types, parameters, and fields.
- For `GetTypeInfo`, this means `EffectiveNullableType` is nullable unless explicit non-null metadata exists.
- Diagnostics should recommend explicit checks or annotations when consuming these APIs.

### 3) Introduce `Conversion.IsNullable`
Add a dedicated `Conversion.IsNullable` helper so conversions can query nullability consistently:
- Avoid re-deriving nullability logic in individual conversions.
- Centralize behavior for value vs. reference types.

### 4) Simplify `NullableTypeSymbol` into a pure decorator
Make `NullableTypeSymbol` behave as the wrapped type with `IsNullable` set to `true`:
- Remove special cases in consumers; treat `ITypeSymbol.IsNullable` as authoritative.
- Add `EffectiveNullableType` to represent the "nullable view" of the type.
- Replace use of `UnderlyingType` with a dedicated helper for "plain type" extraction.

### 5) Add a helper to obtain the "plain" type
Introduce a utility method that returns the non-nullable base type:
- For nullable reference types: return the reference type symbol itself.
- For nullable value types: return the `T` inside `Nullable<T>`.

Suggested helper (names illustrative):
```csharp
public static ITypeSymbol GetPlainType(this ITypeSymbol type)
```

Use this helper when:
- Binding member access on nullable values.
- Producing diagnostics about nullable misuse.
- Computing conversions that must ignore nullability.

### 6) Prefer `ITypeSymbol.IsNullable` in flow checks
Refactor flow analysis and diagnostics to use `ITypeSymbol.IsNullable` directly:
- Eliminate type checks against `NullableTypeSymbol` where possible.
- Ensure nullable-aware features (like match exhaustiveness or event null checks) depend on `IsNullable`.

## Diagnostics and flow analysis updates
- Add a diagnostic that explains when nullable metadata is missing and the compiler assumed nullable.
- Ensure flow state can upgrade types from nullable to non-null after a successful check (e.g., `if (x != null)`), but never downgrade missing-metadata types to non-null without an explicit check.

## Migration plan
1. Add `Conversion.IsNullable` and the plain-type helper.
2. Refactor `NullableTypeSymbol` to be a decorator; introduce `EffectiveNullableType`.
3. Implement `GetTypeInfo` using declared type, flow state, and effective-nullable handling.
4. Update flow analysis and diagnostics to query `ITypeSymbol.IsNullable`.
5. Add tests for:
   - `GetTypeInfo` in contexts with explicit nullable annotations.
   - `GetTypeInfo` with missing metadata (should be nullable).
   - Flow checks on nullable references and `Nullable<T>` values.

## Open questions
- Should `EffectiveNullableType` be surfaced publicly, or only used internally by flow analysis and `GetTypeInfo`?
- How should nullable defaulting interact with `dynamic` or error types?
- Do we need a targeted opt-out for known-safe APIs without metadata?
