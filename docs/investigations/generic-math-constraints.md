# Generic Math Constraint Investigation

## Goal
Document the current failure when compiling the `generic-math-error.rav` sample and outline hypotheses around the recursive substitution path triggered by `INumber<TSelf>` constraints.

## Reproduction
1. Run the compiler against the sample:
   ```bash
   dotnet run --project src/Raven.Compiler -- samples/generic-math-error.rav
   ```
2. The build never produces diagnostics or an output binary. The process remains active until manually cancelled, indicating a hang during compilation rather than an early parse/bind failure.

## Findings
- `ConstructedNamedTypeSymbol.NormalizeTypeArguments` substitutes every explicit type argument before storing them, even when the argument is already concrete. The method does not guard against re-entrance or repeated normalization of the same argument set.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L205-L232】
- `Substitute` rebuilds generic named types by invoking `Construct` whenever any argument changes, which immediately triggers `NormalizeTypeArguments` for the new `ConstructedNamedTypeSymbol`. With self-referential constraints like `INumber<TSelf>`, this loop can re-enter substitution on the same `(type parameter, argument)` pair indefinitely because there is no memoization or visited tracking.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L43-L103】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L131-L189】
- `SubstituteNamedType` also eagerly constructs nested generic types when substitutions are available, again without a guard to short-circuit previously substituted parameter/argument pairs. This expands the recursion surface when constraints or interface closures contain other generic references.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L234-L284】

## Previous Attempt (for reference)
### Constructed type caching & substitution guards
- Added a `ConstructedNamedTypeSymbol.Create(...)` factory with a concurrent cache to canonicalize constructed named types.
- Introduced a `SubstitutionTrace` (`AsyncLocal`) to track in-progress substitutions, avoid re-entrant loops, and memoize substituted results.
- Added debug flags (`RAVEN_DEBUG_CONSTRUCTED_NAMED_TYPE`, `RAVEN_DEBUG_COMPILATION`) and conditional file output for tracing.

### Type parameter equality adjustments
- `TypeParameterSubstitutionComparer` switched to comparing type parameters by `Ordinal`/`Name` + containing symbol key rather than the full `SymbolEqualityComparer`, to avoid recursion and improve stability.

### Caching policy changes
- Caching was disabled for synthesized mutable types (state machines/iterators) to avoid stale constructed symbols.

### Tests added
- `ConstructedNamedTypeSymbolTests` coverage for:
  - Canonicalization: `ConstructedType_FromMetadata_CanonicalizesByArguments`
  - Comparer fallback behavior
  - State machine refresh after mutation
  - Recursive field substitutions

### Build/workflow changes
- Added `scripts/codex-build.sh` to run generators and build in a stable order.
- Marked the script as preferred in `AGENTS.md`.
- Added investigation docs for samples build/run and generic math constraints.

## Additional Findings From the Attempted Implementation
### Substitution & caching behavior
- Recursion originates from generic constraints (`INumber<TSelf>`) that repeatedly construct and substitute the same named types.
- Guarding re-entrant substitution is necessary, but it must not return partial results (avoid emitting a partially substituted `ConstructedNamedTypeSymbol`).
- Canonicalizing constructed types helps reduce duplication but must avoid caching mutable synthesized types.

### Debug/diagnostics
- File I/O for traces is useful but must be strictly gated (environment variables) and throttled to avoid flooding.

### Build reliability
- Generators and MSBuild tasks can collide or fail in agent environments (file locks, missing generated types).
- A stable sequence is required: generators → `Raven.CodeAnalysis` → `Raven.Compiler` (no core) → emit `Raven.Core` → `Raven.Compiler` (with core).

### Sample build/run status
- With the codex build script, samples partially compile.
- Some failures are known (discriminated unions, pattern binding, overload inference gaps).
- `test-result3.rav` still hits an emission crash (`FieldInfo` null).
- Runtime failures include missing `runtimeconfig.json`, `BadImageFormat` in async/generators, and missing `TestDep.dll`.

## Restart Plan (what to re-implement cleanly)
### A) Core substitution & caching
- Rebuild constructed-type caching around explicit factory usage with a clear cache key.
- Re-implement recursion guards in `Substitute`:
  - Track in-progress substitutions separately from completed memoized results.
  - Ensure re-entrance never returns partial substitutions.

### B) Comparer stability
- Re-implement type parameter comparison:
  - Use `Ordinal`/`Name` + containing-type key (only when the containing type is stable).
  - Avoid recursion into symbol equality.

### C) Debugging
- Keep debug trace files strictly behind env flags.
- Throttle output to avoid flooding.

### D) Build workflow for agents
- Provide a reliable agent-only build sequence:
  - generators → `Raven.CodeAnalysis` → `Raven.Compiler` (no core) → emit `Raven.Core` → `Raven.Compiler` (with core)
- Ensure output directories exist before emitting.

## Suggested Re-implementation Checkpoints
- ✅ Generators run independently without invoking MSBuild targets.
- ✅ `Raven.CodeAnalysis` builds without missing generated syntax types.
- ✅ `Raven.Compiler` builds without `Raven.Core` and can emit `Raven.Core` via `ravc`.
- ✅ Substitution re-entrance is guarded without returning partial results.
- ✅ Constructed type caching excludes mutable synthesized types.
- ✅ Tests for caching/substitution pass.
- ✅ Samples build/run, with remaining failures categorized and documented.

## Next Steps
- Add lightweight instrumentation (e.g., a recursion depth counter and an `ImmutableHashSet` of `(parameter, argument)` pairs) around `NormalizeTypeArguments`/`Substitute` to capture the exact loop triggered by `INumber<TSelf>`.
- Introduce a recursion guard or memoization cache inside `Substitute` (and the equality path that calls it) so re-visiting the same substitution returns the existing symbol instead of reconstructing it.
- Add a regression sample or unit test that exercises an async generic method constrained to `INumber<T>` to confirm the hang is resolved once substitution short-circuits.

## Proposed Investigation: Enumerated Substitution Resolution
- Create a dedicated enumerator/visitor type that drives substitution resolution instead of inline recursion.
  - The enumerator owns the current substitution context and yields resolved symbols in a controlled order.
  - Maintain a `HashSet` cache of `(type parameter, argument)` pairs to short-circuit repeats.
  - Track the active substitution stack so re-entrance can return a cached or placeholder result instead of re-entering `NormalizeTypeArguments`.
- Store substitutions in a memoization map keyed by `(containing symbol, parameter, argument)` so nested generic expansions can reuse already-resolved results.
- Ensure the enumerator records the original source of substitutions (constraint path vs. explicit type arguments) to make diagnostics traceable.
- This refactor should make the recursion boundary explicit and isolate the stateful logic required to avoid the `INumber<TSelf>` loop.

## Investigation Log (Start)
- Identify candidate entry points for the enumerator:
  - `ConstructedNamedTypeSymbol.Substitute`
  - `ConstructedNamedTypeSymbol.NormalizeTypeArguments`
  - `ConstructedNamedTypeSymbol.SubstituteNamedType`
- Define the state container for the enumerator:
  - `HashSet<(ITypeParameterSymbol parameter, ITypeSymbol argument)>` for in-flight pairs.
  - `Dictionary<(ISymbol? container, ITypeParameterSymbol parameter, ITypeSymbol argument), ITypeSymbol>` for memoized results.
  - `Stack<SubstitutionFrame>` to track recursion provenance (constraint vs. explicit type arguments).
- Determine how the enumerator yields results:
  - Yield substituted type arguments in a stable order before materializing constructed symbols.
  - Allow short-circuiting when a pair is already in-flight (emit the cached symbol or a placeholder).
- Record any diagnostics hooks needed to trace cycles:
  - Emit a single diagnostic or trace entry when a cycle is detected, with the originating substitution path.
