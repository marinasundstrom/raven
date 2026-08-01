# Nullable control flow analysis and `GetTypeInfo` API

## Goals
- Implement `GetTypeInfo` with nullable flow awareness, aligning with Raven's unified nullability model for reference and value types.
- Treat unannotated external APIs as nullable by default to avoid unsound assumptions.
- Simplify nullable symbol handling so flow analysis and diagnostics use a consistent representation.

## Current constraints and observations
- Raven enforces null checks uniformly for nullable reference and value types; nullable locals must be proven non-null before member access.
- Some dependencies lack `NullableContext` metadata; Raven needs a conservative default (nullable) when metadata is missing.
- `NullableTypeSymbol` currently wraps types with special-case handling, which makes flow analysis harder to reason about and introduces duplication.
- Public nullable transformations must distinguish declared annotation from
  contextual flow and must not expose compiler normalization shortcuts.

## Proposed API and model changes
### 1) Keep declared annotation and flow state separate
`GetTypeInfo` surfaces declared type symbols and reports contextual nullability
separately:

```csharp
typeInfo.Type
typeInfo.Nullability.Annotation
typeInfo.Nullability.FlowState
```

`NullableTypeSymbol` represents a declared nullable type. Flow analysis may
report that an expression is currently not null, but it does not replace or
mutate that declared symbol. The same contract applies to `ConvertedType` and
`ConvertedNullability`.

### 2) Treat missing nullable metadata as nullable by default
When a referenced symbol lacks nullable context, assume it is nullable for analysis:
- This should apply to return types, parameters, and fields.
- Apply this conservative annotation while loading the declared symbol; do not
  manufacture a separate public "effective" type in `GetTypeInfo`.
- Diagnostics should recommend explicit checks or annotations when consuming these APIs.

### 3) Introduce `Conversion.IsNullable`
Add a dedicated `Conversion.IsNullable` helper so conversions can query nullability consistently:
- Avoid re-deriving nullability logic in individual conversions.
- Centralize behavior for value vs. reference types.

### 4) Simplify `NullableTypeSymbol` into a pure decorator
Make `NullableTypeSymbol` behave as the wrapped type with `IsNullable` set to `true`, without exposing `Nullable<T>` as part of the public surface:
- Remove special cases in consumers; treat `ITypeSymbol.IsNullable` as authoritative.
- Keep `Nullable<T>` as an internal implementation detail for value types.
- Preserve metadata provenance internally when needed, without introducing a
  third public type identity.
- Replace direct use of `UnderlyingType` with explicit nullable transformation
  APIs where consumers need them.

### 5) Provide explicit nullable transformations
`GetNonNullableType` is a total operation that returns the non-nullable type:
- For nullable reference types: return the reference type symbol itself.
- For nullable value types: return the `T` inside `Nullable<T>`.

Suggested helper (names illustrative):
```csharp
public static ITypeSymbol GetNonNullableType(this ITypeSymbol type)
```

Use this helper when:
- Binding member access on nullable values.
- Producing diagnostics about nullable misuse.
- Computing conversions that must ignore nullability.

Public consumers can inspect the structure without normalizing optimistically:

```csharp
if (type.TryGetNullableUnderlyingType(out var underlyingType))
{
    // The input carried a nullable decoration.
}
```

`WithNullableAnnotation` is immutable and idempotent. `Annotated` adds the
nullable decoration, while `NotAnnotated` removes it. `None` is rejected for a
concrete Raven type because the unified model requires a definite declared
annotation; absence of a type is represented separately by `TypeInfo`.

### 6) Prefer `ITypeSymbol.IsNullable` in flow checks
Refactor flow analysis and diagnostics to use `ITypeSymbol.IsNullable` directly:
- Eliminate type checks against `NullableTypeSymbol` where possible.
- Ensure nullable-aware features (like match exhaustiveness or event null checks) depend on `IsNullable`.

### 7) Keep representation and flow out of transformation APIs
`WithNullableAnnotation`, `GetNonNullableType`, and
`TryGetNullableUnderlyingType` operate on Raven's declared type model. They do
not expose `System.Nullable<T>` as a separate public type shape and do not
encode `NullableFlowState`. Runtime representation remains an internal codegen
and metadata concern.

## Diagnostics and flow analysis updates
- Add a diagnostic that explains when nullable metadata is missing and the compiler assumed nullable.
- Ensure flow state can upgrade types from nullable to non-null after a successful check (e.g., `if (x != null)`), but never downgrade missing-metadata types to non-null without an explicit check.

## External metadata detection (nullable context)
We need a reliable query path for external assemblies to determine whether nullable annotations are enabled:
- When reading metadata, detect `NullableContextAttribute`/`NullableAttribute` on assemblies, modules, or member scopes.
- Cache a tri-state context (enabled/disabled/unknown) per assembly/module to avoid recomputation.
- When the context is unknown or missing, default to nullable (conservative).
- Apply this during PE symbol construction so `GetTypeInfo` observes the
  conservative declared annotation without synthesizing another public type view.

The same rules apply in the other direction. Raven emission must preserve the
.NET nullable ABI for public Raven signatures, including context/annotation and
flow attributes, nested generic and array positions, and by-reference
parameters and returns. This includes the APIs shipped in Raven.Core and
Raven.Macros; round-trip coverage should inspect metadata and consume it from
both Raven and C#.

## Impact assessment on existing code
The changes affect multiple layers of the compiler pipeline and public APIs:
- **Symbol model**: `NullableTypeSymbol` becomes a strict decorator and consumers stop inspecting `Nullable<T>` directly. This impacts any code that currently branches on wrapper types.
- **Binding & lowering**: `GetNonNullableType` is the total internal
  normalization operation; public structural inspection uses
  `TryGetNullableUnderlyingType`.
- **Type info surface**: `GetTypeInfo` preserves declared symbols and reports
  annotation and flow through `NullabilityInfo`.
- **Diagnostics & flow analysis**: use `ITypeSymbol.IsNullable` instead of concrete type checks, and introduce diagnostics for missing metadata assumptions.
- **Interop/metadata**: external symbol readers need to understand nullable context attributes to avoid accidental non-null defaults.

## Migration plan
1. Catalog current nullability utilities and align them with the decorator model.
2. Add `Conversion.IsNullable` and the plain-type helper.
3. Refactor `NullableTypeSymbol` to be a decorator and apply conservative
   metadata defaults during symbol loading.
4. Implement `GetTypeInfo` using declared type and contextual flow state.
5. Update flow analysis and diagnostics to query `ITypeSymbol.IsNullable`.
6. Add tests for:
   - `GetTypeInfo` in contexts with explicit nullable annotations.
   - `GetTypeInfo` with missing metadata (should be nullable).
   - Flow checks on nullable references and `Nullable<T>` values.
   - nullable transformation behavior on reference and value types.

## Delivery plan (phased)
1. **Inventory & refactor helpers**: replace ambiguous plain/strip helpers with
   explicit annotation and structural APIs.
2. **Metadata pipeline**: add nullable context discovery (assembly/module/member), plus caching, and plumb it into symbol construction.
3. **Type surfaces**: keep `GetTypeInfo` focused on declared annotation and
   contextual flow; keep runtime representation and metadata provenance internal.
4. **Flow + diagnostics**: update flow analysis and diagnostics to consume `IsNullable` and emit missing-metadata guidance.
5. **Tests & validation**: add targeted tests for nullable flow checks, metadata
   defaults, and declared nullable transformation behavior.

## Progress
- ✅ `SemanticModel.GetTypeInfo(ExpressionSyntax)` now surfaces the unconverted expression type as `TypeInfo.Type`, while leaving `ConvertedType` intact.
- ✅ `TypeInfo` preserves the declared nullable annotation while reporting the bound expression's narrowed flow state for strict null-check branches and null guards, independent of whether diagnostics or the semantic query runs first.
- ✅ Added `Conversion.IsNullable` and a `GetNonNullableType` helper to centralize nullability and plain-type access.
- ✅ Added `TryGetNullableUnderlyingType` for structural inspection and
  `WithNullableAnnotation` for immutable, idempotent declared-type transforms.
- ✅ Kept `TypeInfo.Nullability.Annotation` and `FlowState` separate so flow
  narrowing does not mutate the declared symbol.
- ✅ Began routing conversion identity checks through `Conversion.IsNullable` to centralize nullability logic.
- ✅ Updated async return helpers to unwrap nullable decorators via `GetNonNullableType`.
- ✅ Applied `Conversion.IsNullable` in overload scoring to avoid direct nullable wrapper checks.
- ✅ Conditional access lookup now unwraps nullable decorators via `GetNonNullableType`.
- ✅ Null-coalescing binding now unwraps nullable decorators via `GetNonNullableType`.
- ✅ Extension-receiver unification now relies on `Conversion.IsNullable` and `GetNonNullableType`.
- ⏳ Broader `GetTypeInfo` flow coverage remains for joins, loops, richer patterns, nullable unions, and incremental edits.
- ⏳ Metadata defaults and flow diagnostics updates remain outstanding.

## Current state vs. proposed implementation checklist
### Existing
- `NullableTypeSymbol` is the internal declared-type decorator.
- `WithNullableAnnotation`, `GetNonNullableType`, and
  `TryGetNullableUnderlyingType` form the public transformation API.
- `TypeInfo` reports declared annotation and contextual flow independently.

### Needs to be implemented or changed
- Nullable metadata reader to detect `NullableContextAttribute`/`NullableAttribute`.
- Continue replacing direct `NullableTypeSymbol` inspection outside the symbol
  implementation with public structural APIs or purpose-specific internal
  normalization.

## Open questions
- How should nullable defaulting interact with `dynamic` or error types?
- Do we need a targeted opt-out for known-safe APIs without metadata?
