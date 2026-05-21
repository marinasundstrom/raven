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
- Built-in analyzers may implement `ICompilationOptionsAwareAnalyzer` to skip a whole
  analyzer mode before callbacks run.
- Analyzer mode should be modeled as a named `CompilationOptions` value, not as diagnostic
  severity.

An opt-in analyzer must stay off when only `.editorconfig` severity is present. Enabling the
analyzer and choosing diagnostic severity are separate actions.

## Semantic Contract

Analyzers consume semantic information through public compiler APIs:

- `SemanticModel.GetDeclaredSymbol`
- `SemanticModel.GetSymbolInfo`
- `SemanticModel.GetTypeInfo`
- `SemanticModel.GetOperation`
- public symbol and operation interfaces

Analyzers must not depend on binder internals, incremental-transfer tables, language-server
caches, or other implementation-specific state. If a public API cannot answer the analyzer's
question accurately or cheaply enough, improve the compiler API.

Semantic APIs may bind lazily. The compiler owns the binding state and cache policy.

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
