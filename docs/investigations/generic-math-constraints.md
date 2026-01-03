# Generic Math Constraint Hang Investigation

## Goal

Explain why `samples/generic-math-error.rav` hangs during compilation (no diagnostics/output), and define a concrete fix plan that guarantees termination for self-referential generic math constraints like `where TSelf : INumber<TSelf>`.

## Reproduction

```bash
dotnet run --project src/Raven.Compiler -- samples/generic-math-error.rav
```

Observed: process remains active until cancelled (hang).

---

## Findings

### 1) The hang is driven by uncontrolled recursion in `ConstructedNamedTypeSymbol.Substitute`

`Substitute` is a recursive rewriter that:

* substitutes type parameters using `_substitutionMap` and optional `methodMap`
* recurses into wrappers (`NullableTypeSymbol`, `ByRefTypeSymbol`, address, arrays)
* for generic named types, substitutes each type argument and then constructs a new generic type

Key recursion amplifier:

```csharp
return constructedFrom.Construct(substitutedArgs);
```

This forces a new constructed type and immediately re-triggers type argument normalization and more substitution.

### 2) `NormalizeTypeArguments` re-enters substitution without any cycle guard

`BuildTypeArguments()` normalizes explicit arguments via:

```csharp
foreach (var argument in typeArguments)
    builder.Add(Substitute(argument));
```

This creates a self-referential path:

* construct → build args → normalize → substitute → construct → ...

This is especially problematic with generic math, where constraints and interface graphs commonly reference “self” repeatedly (`INumber<TSelf>` and many operator interfaces).

### 3) `AllInterfaces`/`Interfaces` are computed by substituting the original definition’s closure

Current implementation:

```csharp
_originalDefinition.AllInterfaces.Select(i => (INamedTypeSymbol)Substitute(i))
```

This is dangerous for generic math because:

* `_originalDefinition.AllInterfaces` can be very large
* it may involve self-referential edges via constraints/interface inheritance
* each `Substitute(i)` can reconstruct more generic interfaces and re-enter normalization/substitution

### 4) `Substitute` currently performs synchronous file I/O per call

`File.WriteAllText(...)` inside `Substitute` can make the compiler appear hung due to massive repeated writes during deep recursion/closure walks. This must be removed or strictly gated and throttled.

---

## Fix Plan (Concise)

### A) Remove per-call file writes in `Substitute`

* Delete `File.WriteAllText(...)` from `Substitute`.
* If tracing is needed, gate behind env flags and throttle (e.g., every N calls) using buffered logging.

### B) Introduce a cycle-safe `SubstitutionSession`

Create a session object that owns memoization and in-flight detection for substitution work items.

Key rule: memoize by **(type-expression identity, substitution environment)**, where environment includes:

* the constructed owner instance (`this`) and
* the `methodMap` instance (if present)

Session state:

* `Dictionary<Key, ITypeSymbol> completed`
* `HashSet<Key> inProgress`

Cycle policy:

* if a key is already `inProgress`, return a stable fallback (**return the original `type` unchanged**).
  This avoids partial constructed symbols and guarantees termination.

### C) Route all substitution recursion through the session

Refactor:

* `Substitute(...)` becomes a thin entrypoint that creates a session and calls `session.Substitute(this, type, methodMap)`
* move existing logic into `SubstituteCore(type, methodMap, session)`
* replace all recursive calls `Substitute(x)` with `session.Substitute(this, x, methodMap)`

### D) Stop calling `named.Construct(...)` from inside substitution

In `SubstituteCore`, for generic named types:

* compute substituted type arguments via the session
* if changed, create a constructed symbol **without** triggering deep normalization/closure expansion as a side effect

Practical approach with current design:

* avoid `constructedFrom.Construct(substitutedArgs)` within substitution
* construct via `new ConstructedNamedTypeSymbol(constructedFrom, substitutedArgs.ToImmutableArray(), ...)` or a dedicated shallow factory
* ensure the constructed symbol’s type-argument normalization uses the session (see next step)

### E) Make type argument normalization session-based (no raw re-entry)

Update normalization so it does not call `Substitute` directly.

* `NormalizeTypeArguments(...)` should accept a `SubstitutionSession` (and optional `methodMap`) and use `session.Substitute(...)` for each argument.
* `TypeArguments` computation should build once using a session, store `_typeArguments`, and never trigger an unguarded substitution loop.

### F) Re-implement `AllInterfaces` as an explicit visited-set closure walk

Do not compute `AllInterfaces` as:

* “original definition’s `AllInterfaces` then substitute every element”

Instead:

* seed a worklist with substituted **direct** interfaces
* maintain `visited` set keyed by constructed interface identity
* pop interface, add to result if new, then push its base interfaces (substituted via session)
* cache the resulting immutable array

This mirrors Roslyn’s safety properties:

* terminates even with cycles/pathological graphs
* dedupes constructed interfaces
* avoids repeated closure recomputation

---

## Progress

* ✅ Removed per-call file writes in `Substitute`.
* ✅ Introduced a cycle-safe `SubstitutionSession` with in-flight guards.
* ✅ Routed normalization through the substitution session.
* ✅ Replaced interface closure substitution with a visited worklist.
* ✅ Avoided eager `Construct(...)` calls during substitution and named-type member substitution.
* ✅ Avoid constructing unchanged named-type substitutions without actual replacements.
* ✅ Preserve containing overrides when substitutions are otherwise unchanged.
* ⏳ Validate `samples/generic-math-error.rav` compilation termination post-fix.
* ⏳ Add regression coverage for substitution cycles and interface closure.

---

## Verification Checklist

* ✅ `generic-math-error.rav` compilation terminates (may still emit diagnostics, but must not hang)
* ✅ Substitution of self-referential graphs terminates (unit test)
* ✅ `AllInterfaces` for `System.Int32` terminates and is duplicate-free (unit test)
* ✅ Removing debug I/O eliminates I/O-bound “hang” behavior
* ✅ No substitution path reconstructs types in a way that re-enters normalization without session guards

---

## Minimal Regression Tests

1. **Hang regression:** compile `generic-math-error.rav` with a test harness timeout; must complete.
2. **Substitution cycle:** substitute a type graph that includes a self-referential interface constraint edge; must complete and return stable results.
3. **Interface closure:** `AllInterfaces(int)` completes and contains expected constructed interfaces without duplicates.
