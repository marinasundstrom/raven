# Investigation: Recursion in Symbol Resolution and Symbol Comparison

## Purpose

Investigate and explain non-terminating recursion in Raven’s symbol resolution pipeline, focusing on interactions between **generic substitution**, **symbol equality**, and **interface / constraint traversal**.

## Additional Goal

Based on the findings, **produce a clear, implementable plan** to eliminate recursion, ensure termination, and restore deterministic compilation for affected samples (`generic-math-error.rav`, `tokenizer.rav`).

---

## 1. Symptoms

* Compilation hangs indefinitely.
* No diagnostics are produced.
* No output binary is emitted.
* Process must be manually terminated.

This indicates an infinite loop during **semantic analysis**, not parsing or code generation.

---

## 2. Primary Reproducers

### A. `generic-math-error.rav`

Triggered during **generic constraint validation**, especially with self-referential constraints (`INumber<TSelf>`).

#### Observed Recursion Pattern

```
NormalizeTypeArguments
 → BuildTypeArguments
 → get_TypeArguments
 → SymbolEqualityComparer.GetTypeArgumentsOrParameters
 → SymbolEqualityComparer.EqualsCore
 → TypeParameterSubstitutionComparer.Equals
 → Dictionary.TryGetValue
 → TryGetSubstitution
 → Substitute
 → NormalizeTypeArguments
```

#### Key Observation

Symbol comparison is *not* side-effect free:

* Equality pulls type arguments
* Type arguments trigger normalization
* Normalization re-enters substitution
* Substitution relies on equality again

This creates a closed feedback loop.

---

### B. `tokenizer.rav`

Triggered during **overload resolution** and **extension method classification**, without explicit generic math syntax.

#### Observed Recursion Pattern

```
get_AllInterfaces
 → Substitute (repeated)
 → ImmutableArray.CreateRange
 → TryUnifyExtensionReceiver
 → TryConstructExtensionConversion
 → ClassifyConversion
 → OverloadResolver.TryMatch
```

#### Key Observation

Interface closure traversal eagerly substitutes constructed generic interfaces with no cycle detection or memoization.

---

## 3. Root Causes

### A. Substitution Is Not Cycle-Aware

* No tracking of:

  * In-progress substitutions
  * Completed substitutions
* Identical `(type parameter, argument)` pairs are reconstructed indefinitely.

---

### B. Symbol Equality Triggers Substitution

* `SymbolEqualityComparer`:

  * Accesses `TypeArguments`
  * Triggers normalization
  * Is invoked inside dictionary lookups during substitution

Equality therefore re-enters substitution while substitution is already executing.

---

### C. Interface Closure Traversal Is Unguarded

* `AllInterfaces` eagerly substitutes every interface.
* Recursive generic interfaces expand the substitution graph.
* No visited-set or caching exists at this layer.

---

## 4. Architectural Implication

These failures demonstrate a systemic issue:

> **Symbol resolution, substitution, equality, and traversal are not phase-separated.**

Operations expected to be observational (equality, formatting, traversal) can:

* Mutate resolution state
* Re-enter substitution
* Trigger infinite recursion

---

## 5. Previous Mitigation Attempt (For Reference)

A prior attempt tried to mitigate these issues via partial re-implementation.

### Summary of Attempt

* Canonicalized constructed named types via a cache.
* Added substitution guards using `AsyncLocal`.
* Simplified type parameter equality to avoid recursive symbol comparison.
* Disabled caching for synthesized mutable types.

### Outcome

* Reduced duplication and recursion depth.
* Confirmed recursion sources (constraints and interfaces).
* Did **not** fully eliminate recursion.
* Introduced complexity and risk of partial substitution leakage.

This attempt informs—but does not define—the final fix.

---

## 6. Investigation Conclusion

The hangs in both samples stem from the same fundamental flaw:

> **Recursive symbol graphs are traversed without cycle detection, while equality and traversal paths are allowed to trigger substitution.**

Any fix must:

* Enforce termination
* Prevent re-entrance
* Preserve correctness (no partial symbols)

---

## 7. Derived Constraints for a Fix

From the investigation, any solution must satisfy:

1. Substitution must be **cycle-aware and idempotent**
2. Equality must be **substitution-neutral**
3. Interface traversal must be **memoized or guarded**
4. No partially substituted symbols may escape
5. Mutable / synthesized types must not be cached
6. Fix must cover:

   * Constraint validation
   * Interface closure
   * Overload resolution
   * Formatting / display paths

---

## 8. Output: Plan to Fix the Issue

Based on the findings, the investigation produces the following **plan**.

### A. Separate Substitution From Observation

* Ensure:

  * Equality
  * Formatting
  * Interface enumeration
    **never trigger substitution or normalization**
* Precompute or cache required data outside equality paths.

---

### B. Make Substitution Explicitly Cycle-Aware

* Introduce a substitution context that tracks:

  * In-progress substitutions
  * Completed substitutions
* On re-entry:

  * Reuse the existing canonical symbol
  * Do not reconstruct or normalize again

---

### C. Stabilize Equality

* Replace recursive symbol equality during substitution with:

  * Ordinal-based type parameter identity
  * Stable containing-symbol keys
* Prohibit equality from calling `get_TypeArguments` during substitution.

---

### D. Guard Interface Closure Traversal

* Memoize `AllInterfaces` per constructed type.
* Track visited constructed interfaces during traversal.
* Reuse canonical interface instances.

---

### E. Handle Self-Referential Constraints as Fixed Points

* Treat `INumber<TSelf>` and similar patterns as fixed-point constraints.
* Once constructed, reuse the same interface instance rather than re-substituting.

---

### F. Add Temporary Enforcement & Instrumentation

* Assert when:

  * Equality triggers normalization
  * Substitution re-enters without memoization
* Gate instrumentation behind environment flags.
* Remove once termination is guaranteed.

---

## 9. Success Criteria

The issue is considered resolved when:

* `generic-math-error.rav` terminates deterministically.
* `tokenizer.rav` terminates deterministically.
* No infinite recursion in:

  * Substitution
  * Equality
  * Interface traversal
* Equality is side-effect free.
* All symbols produced are fully valid and stable.