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

### 4. Reduce string allocation pressure
*Observation.* Symbol and diagnostic construction eagerly allocates numerous short-lived strings (for metadata names, diagnostic messages, and fully qualified identifiers), increasing GC pressure during binding and emit.

*Potential benefit.* Interning commonly repeated names (metadata type names, well-known identifiers) and avoiding redundant `string.Format` operations can lower allocation volume and improve throughput, especially in large solutions with many repeated symbol lookups.

*Risks.* Aggressive interning can raise working-set size and introduce contention if done without batching. Refactoring diagnostic creation must preserve localization/formatting semantics and avoid hiding missing-argument errors.

## Recommendation
Follow a phased, measurable plan to de-risk the improvements:

1. **Establish a profiling baseline.** Capture representative traces for IDE-like edit/build loops to quantify time in metadata setup, binder construction, and top-level discovery.
2. **Audit allocation hotspots.** Add tracing to identify high-volume string allocations during binding/emit and catalog the dominant call sites (e.g., symbol name construction, diagnostics formatting).
3. **Spike metadata/context reuse.** Prototype caching of `MetadataLoadContext` and related symbol state across derived compilations; measure effect on repeated `AddSyntaxTrees`/`AddReferences` flows and validate reference-change invalidation paths.
4. **Prototype incremental binder/declaration reuse.** Persist immutable declaration tables and binder caches between compilations created via `With*` methods; compare per-edit latency and ensure isolation of mutable caches (e.g., top-level program dictionaries).
5. **Introduce optional lazy top-level discovery.** Guard the lazy path behind a feature flag, exercise diagnostics to confirm stability, and assess threading behavior around shared caches.
6. **Reduce string allocation churn.** Apply targeted mitigations (interning frequently repeated names, caching formatted diagnostic strings, and eliminating redundant concatenations) and re-profile to confirm allocation and throughput gains.
7. **Decide on rollout.** Based on measured wins and diagnostic parity, promote the most effective options behind defaults, documenting remaining risks and required safeguards.
