# Lowering

Lowering converts high-level Raven syntax into simpler, semantically explicit constructs that the binder and emitter can analyze uniformly. This document captures the goals of the lowering pipeline, when it should be applied, and the options we have for implementing new lowerings.

## Goals

* Normalize the surface syntax so downstream stages see a consistent shape (for example, desugaring `if let`, pattern matching sugar, or implicit conversions).
* Preserve source spans and diagnostic context whenever possible to keep error reporting precise.
* Avoid losing semantic information that later phases depend on (e.g., definite assignment or flow analysis facts).
* Keep the transformation composable so that feature-specific rewriters can be combined without observing each other's intermediate state.

## When to add a lowering step

Add a lowering pass when:

* A feature is easier to specify in terms of existing language primitives.
* Semantic analysis would otherwise have to special-case the surface syntax.
* The emitter requires a canonical construct (e.g., loops rewritten into `while` forms).

Prefer leaving syntax untouched when:

* The feature introduces genuinely new semantics that require binder support.
* The syntax tree must be preserved for tooling scenarios (source generators, analyzers) and no normalized form exists yet.

## Approaches

### Syntax-tree rewriting

The simplest approach rewrites the bound tree using visitors. Each rewriter focuses on one feature—such as converting pattern-based `switch` statements into cascaded `if`/`else` checks—and produces a new tree that reuses existing bound nodes. This approach works well for:

* One-to-one desugarings where the lowered structure is still expressible with current bound node types.
* Maintaining direct mappings back to the original syntax nodes for diagnostics.

### Intermediate representations

For complex control-flow features, consider lowering into a dedicated intermediate representation (IR) before code generation. An IR can model constructs like exception regions, temporaries, or SSA form explicitly. In Raven, we reserve this approach for features that require:

* Non-structured control flow (e.g., `goto`, iterator rewriting).
* Aggressive optimization or analysis that is difficult on syntax-shaped trees.

### Hybrid lowering

Some features benefit from a staged pipeline. For example, we can first rewrite syntax into a minimal bound tree, then emit an IR for code generation. This keeps the binder simple while allowing the backend to perform targeted transformations.

When building hybrid pipelines:

1. Keep each stage pure—no mutation in place—to align with the compiler's immutability goals.
2. Clearly document the invariants each stage guarantees (e.g., all `foreach` nodes eliminated).
3. Make the transitions explicit in the diagnostics so future contributors know where to hook in.

## Where Raven already relies on lowering

Even without an IR in place today, several implemented features assume that a lowering pass will normalize their syntax or semantic model before emission. When extending the compiler, audit these areas first so new constructs continue to compose cleanly with the existing pipeline.

### Pattern-based control flow

`match` expressions and `is` patterns both require the scrutinee to be evaluated once, then compared against each arm in order. The [language specification](../../lang/spec/language-specification.md#pattern-matching) also permits guards that observe pattern-bound variables before falling through to later arms. A lowering pass can model this by:

* Capturing the scrutinee in a temporary so subsequent checks reuse the cached value.
* Rewriting each arm into cascaded `if`/`else` statements (or a switch) that preserves the source ordering and fall-through semantics.
* Ensuring that bindings introduced by the pattern remain scoped to the guard and arm expression, matching the specification's flow rules.

This transformation lets the binder and emitter operate on the simpler `if`/`else` and assignment constructs they already understand while keeping exhaustiveness diagnostics intact.

### Null-conditional access

The `?.` operator evaluates its receiver exactly once and either produces `null` or forwards the call to the underlying member when the receiver is non-null. Lowering should synthesize a temporary for the receiver, check it for `null`, and only invoke the member when the value is present. For nullable value types, the lowered form unwraps the `System.Nullable<T>` storage, performs the call, and then re-wraps the result, as described in the specification.

### Extension method invocation

Raven consumes .NET extension methods by rewriting an apparent instance call into a static method invocation on the declaring type. Overload resolution runs on the surface form first; lowering then rewrites `receiver.Extension(args)` into `DeclaringType.Extension(receiver, args)` so IL emission reuses the existing call machinery.

### Union member dispatch

When a union type invokes a member shared by all branches, the specification requires the call to proceed as if the receiver were cast to the common ancestor that defines the member. Lowering performs that cast explicitly, letting the emitter reuse the normal virtual or interface dispatch paths while maintaining the nominal intersection rules for unions.

### Bridging `unit` with `void`

Raven's `unit` type maps to `void` in metadata, but user code can still observe a `unit` value. To bridge the gap, lowering inserts a synthetic `Unit.Value` load after any call to a `void` method when the surrounding context expects `unit`. Likewise, a bare `return` in a `unit`-returning function lowers to emit `Unit.Value` before `ret`. This keeps the surface language consistent with .NET interop requirements without complicating the emitter.

### `for` statements and collection specialization

`for` loops currently survive lowering as `BoundForStatement` nodes so the emitter can generate two specialized code paths: direct indexing for arrays and `IEnumerable`-based enumeration for other collections.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L39-L145】 Lowering them into a common `while` shape would still need to distinguish these cases to avoid regressing array performance and boxing behavior, meaning the lowering phase would end up duplicating the emitter's specialization logic.

There are still benefits to consider. A lowering pass could centralize flow constructs (e.g., `break`/`continue` label synthesis) and make future optimizations reason about loops uniformly before emission. If we pursue this, the lowering should produce two lowered forms—one for arrays and another for general enumerables—so the emitter only handles primitive constructs while retaining the existing optimizations.【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L90-L145】 Until we need those broader transformations, keeping `for` statements out of lowering avoids duplicating iterator plumbing across phases.

## Implementation tips

* Place lowering visitors under `Raven.CodeAnalysis.Lowering` and favor small, composable classes over monolithic rewriters.
* Cover new lowerings with unit tests that inspect both the lowered tree and emitted IL when relevant.
* Update the language specification when lowering changes observable behavior (e.g., order of evaluation).
* Keep diagnostic IDs stable—if a lowering introduces new error conditions, add them to `DiagnosticDescriptors.xml` instead of reusing existing IDs.
