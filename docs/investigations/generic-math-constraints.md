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

## Next Steps
- Add lightweight instrumentation (e.g., a recursion depth counter and an `ImmutableHashSet` of `(parameter, argument)` pairs) around `NormalizeTypeArguments`/`Substitute` to capture the exact loop triggered by `INumber<TSelf>`.
- Introduce a recursion guard or memoization cache inside `Substitute` (and the equality path that calls it) so re-visiting the same substitution returns the existing symbol instead of reconstructing it.
- Add a regression sample or unit test that exercises an async generic method constrained to `INumber<T>` to confirm the hang is resolved once substitution short-circuits.
