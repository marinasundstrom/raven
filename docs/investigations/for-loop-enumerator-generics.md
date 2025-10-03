# Why `for` loops fall back to non-generic enumerators

When Raven lowers a `for each` loop it first inspects the type of the
collection expression to decide which of the three lowering paths to use:
arrays, generic enumerables, or the legacy non-generic enumerator pattern.
The dispatcher lives in `StatementGenerator.EmitForStatement`, which now
switches on the binder-provided `ForIterationInfo` to choose the iteration
strategy. When the binder identifies an `IEnumerable<T>` implementation the
emitter grabs the cached enumerable/enumerator interfaces and emits the
generic lowering directly, otherwise it falls back to the non-generic path
intentionally.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L236-L272】

In the example from the investigation (`members.Where(...)`) the bound
collection expression ultimately has the non-generic
`System.Collections.IEnumerable` type, so the probe reports no generic support
and the backend emits the legacy lowering. This happens because the binder is
still conservative when inferring the element type for a `for` loop:
`InferForElementType` only keeps a concrete `IEnumerable<T>` when it can see
that interface directly on the bound expression's type. For iterator helpers
produced by LINQ extension methods we used to lose that specific interface
information, so the inferred element type collapsed to `object`. Once the loop
variable was typed as `object`, the generator could not construct a matching
`IEnumerator<T>` and therefore fell back to the non-generic lowering that
produced the `System.Collections.IEnumerator` local observed in the IL dump.
The new iteration metadata keeps hold of the discovered interface so the
emitter can lower to the generic pattern.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L206-L244】【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L236-L272】

Improving the binder so it preserves the exact `IEnumerable<T>` interface for
extension-method iterators would let `TryGetGenericEnumeratorInfo` succeed and
unblock generic enumerator lowering. Until then, loops over helpers like
`Enumerable.Where` continue to use the non-generic pattern even though the
runtime iterator implements `IEnumerator<T>`.

## Strategy to fix the lowering

1. **Teach the binder to classify the iteration pattern.** Instead of
   returning only the loop variable symbol, have `BindForStatement` compute a
   small "iteration info" record that captures the element type plus the best
   matching enumerable/enumerator pair (array, `IEnumerable<T>`, or
   non-generic). This logic can reuse the existing `InferForElementType`
   helper while also surfacing the specific `IEnumerable<T>` interface that it
   discovers.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L206-L244】
2. **Plumb the iteration info through the bound tree.** Extend
   `BoundForStatement` so it stores the chosen iteration kind alongside the
   bound collection expression. That way the lowering step does not have to
   re-run type discovery and risk ending up with a less-precise interface than
   the binder already found.【F:src/Raven.CodeAnalysis/BoundTree/BoundForStatement.cs†L5-L15】
3. **Update code generation to respect the binder's choice.** Replace the
   ad-hoc probe in `StatementGenerator.EmitForStatement` with a switch on the
   new iteration info. When the binder says the loop can enumerate via
   `IEnumerable<T>`/`IEnumerator<T>`, emit the generic lowering directly and
   avoid the non-generic fallback that currently injects the
   `System.Collections.IEnumerable` cast.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L236-L415】
4. **Add regression coverage.** Introduce an end-to-end test that lowers a
   `for` loop over a LINQ helper (e.g. `members.Where(...)`) and verifies that
   the emitted IL declares a generic enumerator local. This guards the new
   plumbing and prevents future regressions that would reintroduce the
   `IEnumerator` fallback.

## Step 1 – binder iteration metadata

`BindForStatement` now asks a new `ClassifyForIteration` helper to describe the
best iteration strategy before it creates the loop variable. The helper wraps
the existing `InferForElementType` logic so it can forward the precise
`IEnumerable<T>` interface symbol the binder already discovered instead of
forcing the emitter to re-run reflection-based probing. The classification is
captured in a new `ForIterationInfo` record that distinguishes array, generic,
and non-generic patterns and remembers the matching enumerable/enumerator pair
when present.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L206-L244】【F:src/Raven.CodeAnalysis/ForIterationInfo.cs†L5-L32】

## Step 2 – bound tree iteration plumbing

`BoundForStatement` now stores the `ForIterationInfo` produced by the binder so
lowering can rely on the binder's classification instead of rediscovering the
enumeration pattern. This ensures the chosen `IEnumerable<T>` interface flows
through semantic analysis unchanged and keeps the upcoming emitter updates
focused on code generation rather than repeated type inspection.【F:src/Raven.CodeAnalysis/BoundTree/BoundForStatement.cs†L1-L21】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L206-L218】

## Step 3 – emitter honors iteration metadata

`StatementGenerator.EmitForStatement` now switches over
`BoundForStatement.Iteration` to decide which lowering to emit. Array loops
continue to use the optimized indexed form, while generic enumerable loops use
the cached `IEnumerable<T>`/`IEnumerator<T>` symbols provided by the binder and
avoid re-probing the collection type. Only loops that the binder explicitly
marks as non-generic flow through the legacy cast-heavy lowering, eliminating
the accidental `System.Collections.IEnumerator` locals for LINQ helpers.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L236-L415】
