# Ref safety analysis

Ref safety is a family of related rules for ref-like values, `scoped`
declarations, references, `stackalloc`, closures, iterators, and async
suspension. These rules require more than name and type resolution, but they
should not accumulate in Raven's binder classes.

This document defines the compiler architecture for implementing those rules.
It is contributor guidance; the language-facing behavior belongs in the
specialized systems-programming specification.

## Design boundary

The binder constructs the semantic input to ref-safety analysis. It may:

- recognize syntax and resolve types
- construct symbols and bound nodes
- assign declaration facts such as `ScopedKind`
- reject a construct when validity is known at the point it is bound
- invoke an analysis after the relevant body or signature is complete

The binder should not implement a whole-body ref-safety traversal or maintain a
parallel set of escape, capture, or suspension facts. Those responsibilities
belong to standalone components:

- syntax walkers for rules that depend only on syntax shape
- bound-tree walkers for semantic rules spanning a completed bound body
- data-flow analyses for provenance, escape scopes, and control-flow joins
- contract analyzers for relationships between complete signatures
- diagnostic reporters that translate analysis results into diagnostics

A check belongs in binding only when it is intrinsic to constructing the
current symbol or bound node. For example, resolving the element type and count
of `stackalloc` is binding work. Determining whether its resulting value can
escape through later assignments and returns is analysis work.

## Current components

`RefSafetyAnalysis` walks a completed bound body. It tracks ref-safety
provenance through locals, assignments, fields, calls, and returns, then
produces a `RefSafetyAnalysisResult`.

`RefSafetyAnalysisResult` is a diagnostic-independent description of
violations. Analyses should return facts rather than report diagnostics while
walking. This keeps traversal reusable and makes it possible to test semantic
behavior without coupling tests to binder implementation details.

`RefSafetyDiagnosticReporter` coordinates body-level checks and maps their
findings to Raven diagnostics. Capture, iterator, async-suspension, and escape
reporting live here rather than as convenience methods on the base binder.

`RefSafetyContractAnalyzer` compares completed signatures. It owns rules such
as scoped-contract compatibility across overrides, interface implementations,
indexers, and partial declarations.

Syntax-local declaration validation remains in focused syntax utilities or the
code that creates the declaration. It should move to a dedicated syntax walker
when several declaration kinds need the same traversal.

## Adding a rule

Choose the smallest semantic layer that owns all required information:

1. Use a syntax utility or walker when tokens and syntax shape are sufficient.
2. Use the binding site when the rule is inseparable from constructing one
   symbol or bound node.
3. Use `RefSafetyContractAnalyzer` when complete signatures are compared.
4. Extend a bound-tree or data-flow analysis when the rule follows values
   through a body.
5. Add diagnostic mapping after the analysis can express the violation without
   referring to a binder.

Do not add binder fields to simulate flow state. Do not put a recursive
bound-tree traversal in a binder. Do not expose analysis caches through public
semantic APIs.

## Analysis lifetime and incremental compilation

Ref-safety results are derived from the completed bound body and compilation
context. They may eventually be cached with the binder-owned semantic state,
but cache identity and invalidation must remain internal to the compiler.
`SemanticModel` callers should receive the same answer whether the analysis
result was cached or recomputed.

An analysis must be deterministic and must not mutate bound nodes or symbols.
If a future rule requires control-flow joins, represent its state explicitly
and merge it at graph edges instead of depending on traversal order.

The escape model is expected to grow into an explicit lattice of scopes and
provenance. New rules should strengthen that shared model rather than add
one-off booleans to binding code.

## Testing

Prefer tests that assert:

- diagnostics and their source locations
- symbol facts such as `ScopedKind` and ref-like metadata
- semantic operation or data-flow results when those APIs are exposed
- observable runtime behavior for valid programs
- compatibility of generic and metadata-imported ref-like types

Avoid tests tied to the private walker order or exact lowered instruction
sequence. Every moved rule should retain focused regression coverage, and new
control-flow behavior should cover branches and joins rather than only linear
bodies.
