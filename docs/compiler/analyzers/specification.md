# Analyzer Specification

This document specifies Raven analyzer behavior. It applies to built-in analyzers
and to external analyzers loaded through workspace analyzer references.

## Definition

An analyzer is a `DiagnosticAnalyzer` that observes Raven syntax and semantic
state and reports `Diagnostic` instances. Analyzer diagnostics are non-emission diagnostics:
they can block a build when configured as errors, but they do not define language validity or
runtime behavior.

Compiler diagnostics remain the authority for syntactic and semantic validity. Analyzer
diagnostics are policy, correctness guidance, maintainability guidance, or editor assistance.

## Execution Model

Analyzers run after a project has syntax trees, references, compilation options, and analyzer
references. The workspace may request diagnostics for:

- a whole project;
- a single document;
- syntax-only paths that intentionally exclude analyzers.

Analyzer callbacks are registered through `AnalysisContext` and invoked by the analyzer
runner. Runners must isolate analyzer failures from normal compiler diagnostics. Analyzer
authors must still treat unexpected exceptions as bugs and must not use exceptions for
ordinary source conditions.

The runner owns traversal and invalidation. A cold document analysis may perform one full
syntax walk and dispatch every registered syntax-node action from that walk. Incremental
document analysis should reuse previous analyzer diagnostics when the relevant project and
document snapshot is unchanged, and should rerun only actions whose registered syntax or
symbol scope intersects the changed region when that information is available. Analyzers must
not assume that every callback runs on every edit.

Registered actions define the invalidation unit:

- document-scoped syntax-node actions are invalidated by changed nodes of the registered
  kinds, changed ancestors, and any document-level semantic state they depend on;
- node-scoped syntax-node actions are invalidated by changed nodes of the registered kinds
  and by changed stable semantic context for those nodes;
- symbol actions are invalidated by changes to declarations of the registered symbol kinds,
  by declaration-sensitive semantic context, and by reference or operation facts the action
  explicitly queries;
- syntax-tree actions are invalidated by any change in that syntax tree;
- compilation actions are invalidated by project-wide inputs such as references, options,
  analyzer configuration, document set changes, or source declarations they inspect;
- future operation actions should be invalidated by changed operation roots and by semantic
  context for those operations.

The default syntax-node action scope is document-scoped. An analyzer should opt into
node-scoped invalidation only when rerunning the action for the changed node is enough to
preserve correctness. This prevents the workspace cache from treating analyzers such as
unused-symbol checks as local when their diagnostics depend on references elsewhere in the
document or project.

Analyzer actions are independent from each other. They share immutable compilation and semantic
model snapshots, report diagnostics into the driver, and must not communicate through mutable
analyzer state. The driver may therefore run actions sequentially or concurrently, merge their
diagnostics afterward, and drop stale action results when a newer snapshot supersedes the run.

Cancellation is cooperative. `OperationCanceledException` must propagate and must not be
converted into a diagnostic.

## Diagnostic Identity

Each analyzer diagnostic has one stable ID and one stable `DiagnosticDescriptor`.

- Built-in Raven analyzer IDs use the reserved `RAV` prefix.
- External analyzers must not report `RAV` IDs.
- Built-in analyzer IDs must not duplicate compiler diagnostic IDs.
- A diagnostic's message may use descriptor placeholders for source-specific data.
- A diagnostic's message must not change just because its severity changes.

## Severity And Suppression

Diagnostic severity is determined in this order:

1. source suppression, when a pragma suppresses the diagnostic;
2. specific diagnostic options, including `.editorconfig`;
3. the descriptor's `DefaultSeverity`.

`.editorconfig` controls severity and suppression by diagnostic ID. It must not decide
whether a whole opt-in analyzer mode runs.

Source suppression uses the same pragma warning mechanism as compiler diagnostics.

## Analyzer Participation

Analyzer participation is controlled by compilation and project options:

- `RunAnalyzers=false` disables analyzer diagnostics for a project.
- `DisabledAnalyzers` / `RavenDisabledAnalyzers` disables individual built-in analyzers for
  a project by analyzer type name or fully qualified analyzer type name.
- Built-in analyzers may implement `ICompilationOptionsAwareAnalyzer` to skip a whole
  analyzer mode before callbacks run.
- Analyzer mode should be modeled as a named `CompilationOptions` value, not as diagnostic
  severity.

An opt-in analyzer must stay off when only `.editorconfig` severity is present. Enabling the
analyzer and choosing diagnostic severity are separate actions.

Analyzer instances are registration/execution objects, not durable cache containers. They
should not use mutable static state or mutable instance state for semantic results. Any
per-run state must be scoped to a callback or to an explicit driver-provided start context
when such APIs exist.

## Semantic Contract

Analyzers consume semantic information through public compiler APIs:

- `SemanticModel.GetDeclaredSymbol`
- `SemanticModel.GetSymbolInfo`
- `SemanticModel.GetTypeInfo`
- `SemanticModel.GetOperation`
- `SymbolAnalysisContext.Symbol`
- public symbol and operation interfaces

Analyzers must not depend on binder internals, incremental-transfer tables, language-server
caches, or other implementation-specific state. If a public API cannot answer the analyzer's
question accurately or cheaply enough, improve the compiler API.

Semantic APIs may bind lazily. The compiler owns the binding state and cache policy.
They are also non-reporting query APIs: asking for a symbol, type, or operation must not make
the analyzer depend on the compiler diagnostic collection path or publish partial diagnostics
as a side effect.

Analyzer implementations must compare symbols with `SymbolEqualityComparer.Default`.
Reference identity is not semantic identity. The same source declaration or metadata member
can be represented by equivalent symbol instances across lazy binding paths, diagnostic
binding, operation creation, or future incremental snapshots. Analyzer state such as
candidate sets, used-symbol sets, and cross-callback maps must therefore use comparer-backed
collections.

## Reporting Contract

Analyzer diagnostics should be:

- deterministic for the same compilation and analyzer options;
- located on the smallest useful source span;
- concise enough for editor diagnostic lists;
- actionable when possible;
- robust in incomplete or erroneous source.

When required semantic information is unavailable because source is incomplete or already has
blocking errors, analyzers should normally skip reporting rather than guessing.

## Language Server Contract

The language server may schedule syntax diagnostics and analyzer diagnostics on separate
lanes. Analyzer diagnostics are allowed to be slower than syntax diagnostics, but analyzers
must be written with repeated typing updates and cancellation in mind.

Skipped, canceled, or pending analyzer lanes are not empty diagnostic results.
The language server keeps the last successful analyzer diagnostics visible while
newer analyzer work is pending when their source ranges can be translated across
the edit. If an edit intersects an analyzer diagnostic's previous span, that
diagnostic is dropped until a fresh analyzer result reports it again.

`.editorconfig` severity changes are watched by the language server and reapplied to open
projects without reloading the project. Project-file analyzer mode changes follow project
reload behavior.

## Testing Requirements

Built-in analyzers require focused tests for:

- positive reports;
- no-report cases;
- source locations and message arguments;
- severity remapping;
- source suppression when relevant;
- project-mode behavior for opt-in analyzers;
- document-scoped diagnostics when language-server behavior depends on it.

Use [Analyzer verifier](../development/testing/analyzer-verifier.md) for workspace-backed
analyzer tests.
