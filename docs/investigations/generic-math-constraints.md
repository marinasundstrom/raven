# Generic Math Constraint Investigation

## Goal
Document the current failure when compiling the `generic-math-error.rav` sample and outline hypotheses around the recursive substitution path triggered by `INumber<TSelf>` constraints. Capture the recent work on constructed-type caching and substitution tracing as groundwork for the next implementation pass.

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
- Recent work introduced constructed-type caching (`ConstructedNamedTypeSymbol.Create`) and per-call substitution tracing to short-circuit re-entrant substitutions. The cache helps canonicalize generic constructions, while the trace records in-progress and completed substitutions to prevent infinite recursion. This establishes the scaffolding needed to revisit the `INumber<TSelf>` path without re-triggering the same substitution loop.

## Learnings and challenges
- The recursion was not only about repeated calls to `Construct` but also about re-entering substitutions for the same `(type parameter, argument)` pair through interface closures. Caching is useful but insufficient unless we also guard re-entrance and avoid returning partially substituted results.
- Some constructed types (e.g., synthesized state machines) can mutate after construction. Caching those can return stale members, so the caching policy must be explicit and selective.
- Debug output is helpful for tracing inference/substitution loops, but file I/O can be noisy in tight loops. The current debug flags should remain opt-in and throttled to avoid masking performance issues.

## Step-by-step guide (next implementation pass)
1. **Re-run the repro**: Execute `dotnet run --project src/Raven.Compiler -- samples/generic-math-error.rav` and confirm the hang. Capture any debug output if `RAVEN_DEBUG_CONSTRUCTED_NAMED_TYPE=trace` is enabled.
2. **Inspect constructed-type creation**: Review `ConstructedNamedTypeSymbol.Create(...)` and its cache key to ensure `INumber<TSelf>` constructions are canonicalized without mutating types being cached.
3. **Trace re-entrant substitutions**: Use the substitution trace to identify which `(type parameter, argument)` pairs re-enter substitution. Verify that `InProgressTypes`/`SeenTypes` are preventing re-entrant loops without returning incomplete results.
4. **Focus on constraint expansion**: Examine how constraint interfaces are expanded for type parameters. Confirm that constructed interface chains are not re-constructing the same generic instantiations repeatedly.
5. **Add targeted coverage**: Create a regression test or sample that constrains a generic method to `INumber<T>` and exercises interface traversal. Ensure it completes with diagnostics (or successful compile) rather than hanging.
6. **Iterate on caching policy**: If re-entrance still happens, refine the cache key or opt out for specific constructed types that participate in constraint expansion loops.

## Next Steps
- Confirm the new substitution trace covers the `INumber<TSelf>` recursion path and adjust it if it returns incomplete substitutions.
- Add a regression sample or unit test that exercises an async generic method constrained to `INumber<T>` to confirm the hang is resolved once substitution short-circuits.
- Validate the caching policy against any constructed types that mutate after creation and refine the `ShouldCache` logic if needed.
