# Nullable control flow analysis and `GetTypeInfo` API

## Goals
- Implement `GetTypeInfo` with nullable flow awareness, aligning with Raven's unified nullability model for reference and value types.
- Treat unannotated external APIs as nullable by default to avoid unsound assumptions.
- Simplify nullable symbol handling so flow analysis and diagnostics use a consistent representation.

## Current constraints and observations
- Raven enforces null checks uniformly for nullable reference and value types; nullable locals must be proven non-null before member access.
- Some dependencies lack `NullableContext` metadata; Raven needs a conservative default (nullable) when metadata is missing.
- `NullableTypeSymbol` currently wraps types with special-case handling, which makes flow analysis harder to reason about and introduces duplication.
- There are existing helpers like `MakeNullable`/`StripNullable` that influence how types are decorated or normalized today.

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
Make `NullableTypeSymbol` behave as the wrapped type with `IsNullable` set to `true`, without exposing `Nullable<T>` as part of the public surface:
- Remove special cases in consumers; treat `ITypeSymbol.IsNullable` as authoritative.
- Keep `Nullable<T>` as an internal implementation detail for value types.
- Add `EffectiveNullableType` for the compiler/internal pipeline to discover the underlying representation when needed.
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

### 7) Align `MakeNullable`/`StripNullable` with the decorator model
Ensure `MakeNullable` and `StripNullable` treat `NullableTypeSymbol` as a pure decorator and do not leak `Nullable<T>` to the public surface:
- `MakeNullable` should return a type with `IsNullable = true` (using `NullableTypeSymbol` when needed), without exposing `Nullable<T>` externally.
- `StripNullable` should remove the nullable decoration but keep the same base symbol identity (reference types) or underlying `T` for value types.
- Both helpers should defer to `EffectiveNullableType` for internal pipelines that need to know the runtime representation.

## Diagnostics and flow analysis updates
- Add a diagnostic that explains when nullable metadata is missing and the compiler assumed nullable.
- Ensure flow state can upgrade types from nullable to non-null after a successful check (e.g., `if (x != null)`), but never downgrade missing-metadata types to non-null without an explicit check.

## External metadata detection (nullable context)
We need a reliable query path for external assemblies to determine whether nullable annotations are enabled:
- When reading metadata, detect `NullableContextAttribute`/`NullableAttribute` on assemblies, modules, or member scopes.
- Cache a tri-state context (enabled/disabled/unknown) per assembly/module to avoid recomputation.
- When the context is unknown or missing, default to nullable (conservative).
- Feed this into `EffectiveNullableType` and `GetTypeInfo` so external APIs without context flow as nullable.

## Impact assessment on existing code
The changes affect multiple layers of the compiler pipeline and public APIs:
- **Symbol model**: `NullableTypeSymbol` becomes a strict decorator and consumers stop inspecting `Nullable<T>` directly. This impacts any code that currently branches on wrapper types.
- **Binding & lowering**: `MakeNullable`/`StripNullable` should become the single entry points for decorating or removing nullability, with `GetPlainType` used when nullability must be ignored.
- **Type info surface**: `GetTypeInfo` returns declared/flow/effective types, which may require updates in semantic model consumers and tests.
- **Diagnostics & flow analysis**: use `ITypeSymbol.IsNullable` instead of concrete type checks, and introduce diagnostics for missing metadata assumptions.
- **Interop/metadata**: external symbol readers need to understand nullable context attributes to avoid accidental non-null defaults.

## Migration plan
1. Catalog current nullability utilities (`MakeNullable`, `StripNullable`, existing `IsNullable` checks) and align them with the decorator model.
2. Add `Conversion.IsNullable` and the plain-type helper.
3. Refactor `NullableTypeSymbol` to be a decorator; introduce `EffectiveNullableType`.
4. Implement `GetTypeInfo` using declared type, flow state, and effective-nullable handling.
5. Update flow analysis and diagnostics to query `ITypeSymbol.IsNullable`.
6. Add tests for:
   - `GetTypeInfo` in contexts with explicit nullable annotations.
   - `GetTypeInfo` with missing metadata (should be nullable).
   - Flow checks on nullable references and `Nullable<T>` values.
   - `MakeNullable`/`StripNullable` behavior on reference types and `Nullable<T>`.

## Implementation progress
- [x] Add `Conversion.IsNullable` helper and consume it in conversion identity checks.
- [x] Add `GetPlainType` helper for nullable unwrapping.
- [x] Implement `EffectiveNullableType` helper for value-type nullable runtime representation.
- [x] Derive `NullabilityInfo`/`NullableAnnotation` from type decoration (including unions with `null`).
- [ ] Refactor `NullableTypeSymbol` into a pure decorator across all consumers.
- [ ] Implement `GetTypeInfo` declared/flow/effective-nullable shape.
- [ ] Add metadata context detection for external assemblies.

## Implementation gaps to address
- `EffectiveNullableType` and `GetPlainType` exist, but most consumers still branch on `NullableTypeSymbol` and `UnderlyingType`, which keeps value/reference nullability logic split instead of treating nullability as decoration.
- Codegen and binder layers still reach into `UnderlyingType` for reference types instead of relying on `IsNullable` + plain/effective type helpers, so runtime type mapping is inconsistent.
- `GetTypeInfo` currently reports only the bound type (declared/converted) and does not surface flow-state or effective-nullable types, so nullability-aware tooling cannot query consistent data.
- Ensure all public/nullability surfaces consume the type-derived `NullabilityInfo` (instead of ad-hoc nullability inference).

## Delivery plan (phased)
1. **Inventory & refactor helpers**: enumerate current uses of `MakeNullable`, `StripNullable`, and direct `NullableTypeSymbol` checks; replace with the decorator model and `ITypeSymbol.IsNullable`.
2. **Metadata pipeline**: add nullable context discovery (assembly/module/member), plus caching, and plumb it into symbol construction.
3. **Type surfaces**: implement `EffectiveNullableType` and update `GetTypeInfo` to return declared/flow/effective values.
4. **Flow + diagnostics**: update flow analysis and diagnostics to consume `IsNullable` and emit missing-metadata guidance.
5. **Tests & validation**: add targeted tests for nullable flow checks, metadata defaults, and `MakeNullable`/`StripNullable` behavior.

## `GetTypeInfo` test plan
- **Declared vs. converted type**: verify `GetTypeInfo(expr).Type` vs. `ConvertedType` for explicit casts and implicit conversions (e.g., `int` to `int?`, `string` to `object`).
- **Flow state updates**: ensure flow analysis upgrades nullable locals to non-null after `if (x != null)` and that `GetTypeInfo` returns the flow-decorated type inside the guarded block.
- **Nullable value types**: confirm nullable value types surface as decorated `T` in symbol model while `EffectiveNullableType` returns `Nullable<T>` when requested.
- **External metadata defaults**: load a metadata-only assembly with missing nullable context and assert `GetTypeInfo` reports nullable flow/effective types conservatively.
- **Union nullability**: for `T | null`, ensure `GetTypeInfo` reports nullable annotation/flow consistent with the decoration model and does not treat union-null as a distinct type kind.

## Current state vs. proposed implementation checklist
### Likely existing (verify in code)
- `NullableTypeSymbol` exists as a wrapper.
- `MakeNullable`/`StripNullable` helpers exist and are used in binding/conversions.
- Some `IsNullable`/type checks exist across binding/flow diagnostics.

### Needs to be implemented or changed
- `GetTypeInfo` to return declared/flow/effective-nullable types.
- `Conversion.IsNullable` helper to unify conversion-time nullability checks.
- `EffectiveNullableType` to represent internal nullable shape for interop and flow.
- Nullable metadata reader to detect `NullableContextAttribute`/`NullableAttribute`.
- Update all consumers to prefer `ITypeSymbol.IsNullable` and `MakeNullable`/`StripNullable` instead of inspecting `Nullable<T>`.

## Open questions
- Should `EffectiveNullableType` be surfaced publicly, or only used internally by flow analysis and `GetTypeInfo`?
- How should nullable defaulting interact with `dynamic` or error types?
- Do we need a targeted opt-out for known-safe APIs without metadata?
