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

## Implementation tips

* Place lowering visitors under `Raven.CodeAnalysis.Lowering` and favor small, composable classes over monolithic rewriters.
* Cover new lowerings with unit tests that inspect both the lowered tree and emitted IL when relevant.
* Update the language specification when lowering changes observable behavior (e.g., order of evaluation).
* Keep diagnostic IDs stable—if a lowering introduces new error conditions, add them to `DiagnosticDescriptors.xml` instead of reusing existing IDs.
