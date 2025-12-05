# Performance Investigation Notes

## Goal
This document summarizes near-term options for improving the Raven compiler's performance and outlines potential risks associated with each approach.

## Observations and Opportunities

### 1. Reduce upfront metadata loading and symbol construction
*Observation.* `Compilation.Setup` eagerly builds a `MetadataLoadContext`, registers every reference, and constructs `DeclarationTable`/binder state for all syntax trees as soon as `EnsureSetup` runs, even if the caller only needs partial services (for example, semantic analysis without emit).【F:src/Raven.CodeAnalysis/Compilation.cs†L136-L185】

*Potential benefit.* Delaying metadata loads or caching `MetadataLoadContext` instances across derived compilations (created via `AddSyntaxTrees`/`AddReferences`) would reduce repeated I/O and reflection-heavy symbol creation when running iterative builds or language services.

*Risks.* Sharing metadata contexts introduces disposal and lifetime concerns, and lazily loading symbols can surface new failure points (missing files or version mismatches) later in the pipeline, making errors harder to attribute. Any caching layer must also stay coherent when reference lists change.

### 2. Make top-level program discovery lazy
*Observation.* When targeting console output, `InitializeTopLevelPrograms` walks every syntax tree, calls `DescendantNodes` to find global statements, and synthesizes program symbols before binding has a chance to demand them.【F:src/Raven.CodeAnalysis/Compilation.cs†L189-L279】

*Potential benefit.* Moving global-statement scanning behind a lazy provider or caching per syntax tree would avoid repeated full-tree traversals in multi-file projects and cut work for compilations that ultimately skip top-level entry-point generation.

*Risks.* Deferring discovery changes when diagnostics are produced; consumers that expect eager synthesis might observe different ordering. Introducing laziness also requires thread-safe caches because `GetOrCreateTopLevelProgram` uses shared dictionaries and spin-waits for partially constructed entries.【F:src/Raven.CodeAnalysis/Compilation.cs†L221-L265】

### 3. Improve incremental compilation reuse
*Observation.* Creating a new compilation via `AddSyntaxTrees`, `AddReferences`, or `WithAssemblyName` clones arrays but re-triggers `Setup`, which rebuilds binders and declaration tables for all syntax trees from scratch.【F:src/Raven.CodeAnalysis/Compilation.cs†L119-L185】

*Potential benefit.* Persisting immutable structures (syntax trees, declaration tables, and bound node factories) across derived compilations would align the pipeline closer to Roslyn's incremental model, substantially reducing per-edit latency in IDE scenarios.

*Risks.* Sharing state across compilations complicates ownership of caches like `_topLevelProgramMembers` and `_metadataReferenceSymbols`; without careful isolation, mutations or delayed initialization could bleed across compilations and lead to hard-to-reproduce correctness bugs.

## Recommendation
Prototype a profiler-backed spike focused on incremental reuse (items 1 and 3) to measure the payoff in editing workloads, and gate the lazy top-level program discovery behind an option to validate diagnostic stability before adopting it broadly.
