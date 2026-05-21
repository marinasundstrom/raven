# Analyzer API Surface

This document lists the Raven APIs analyzer authors are expected to use. The API is intended
to stay Roslyn-like where possible, with Raven-specific syntax and symbol types where the
language requires them.

## Core Types

`DiagnosticAnalyzer`
: Base class for source analyzers. Implement `Initialize` and register syntax-tree or
syntax-node callbacks.

`AnalysisContext`
: Registration surface passed to `DiagnosticAnalyzer.Initialize`.

`SyntaxTreeAnalysisContext`
: Context for file-wide analyzer callbacks. Provides `SyntaxTree`, `Compilation`,
`ReportDiagnostic`, and `CancellationToken`.

`SyntaxNodeAnalysisContext`
: Context for syntax-node callbacks. Provides `Node`, `SemanticModel`, `Compilation`,
`ReportDiagnostic`, and `CancellationToken`.

`DiagnosticDescriptor`
: Stable descriptor for a diagnostic ID, title, message format, category, and default
severity.

`Diagnostic`
: A reported diagnostic with descriptor, location, severity, suppression state, message
arguments, and optional properties.

`ICompilationOptionsAwareAnalyzer`
: Optional interface for built-in analyzers whose entire analyzer mode is controlled by
`CompilationOptions`.

## Registration APIs

`RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext>)`
: Runs once per syntax tree. Use for checks that need file-wide context.

`RegisterSyntaxNodeAction(Action<SyntaxNodeAnalysisContext>, params SyntaxKind[])`
: Runs for matching syntax nodes. Use for most analyzers.

Analyzer runners call registered actions for each compilation or document scope requested by
the workspace. Analyzer implementations should assume callbacks may run often during typing.

## Syntax APIs

Analyzers use the Raven syntax model from `Raven.CodeAnalysis.Syntax`:

- `SyntaxTree.ParseText` parses source.
- `SyntaxNode.Kind` identifies a node's `SyntaxKind`.
- `ChildNodes`, typed child properties, tokens, trivia, spans, and locations expose the
  parsed source shape.
- `GetLocation` returns a reportable source location.

Use syntax when the rule is about source shape. Use semantic APIs when the rule depends on
symbols, types, overload resolution, conversions, or member identity.

See [Syntax Tree API](../api/syntax-tree.md) and
[Syntax tree architecture](../architecture/syntax-tree.md).

## Semantic APIs

`SemanticModel` is the analyzer entry point for compiler-owned semantic facts:

- `GetDeclaredSymbol` for declarations.
- `GetSymbolInfo` for references, calls, member access, and overload resolution.
- `GetTypeInfo` for expression type and converted type.
- `GetOperation` for operation-tree analysis.
- `GetDocumentDiagnostics` when an analyzer intentionally skips semantic analysis for files
  with errors.

Semantic queries may trigger binding. The compiler owns the caches and incremental state;
analyzers should not reach into binder internals or language-server caches.

See [Semantic analysis API](../api/semantic-analysis.md),
[Symbol resolution](../api/symbol-resolution.md), and
[Compiler API performance guidance](../api/performance.md).

## Operations APIs

The operations API provides a semantic tree for expressions and statements. It is useful when
an analyzer needs to understand what code does without depending on exact syntax spelling.

Common operation interfaces include:

- `IExpressionStatementOperation`
- `IInvocationOperation`
- `IMemberReferenceOperation`
- `IPropertyReferenceOperation`
- `IFieldReferenceOperation`
- `IConditionalAccessOperation`
- `IConversionOperation`
- `IAwaitOperation`

Use `SemanticModel.GetOperation(node)` to enter the operations model. If an analyzer requires
a missing operation node or property, add that API and its tests rather than duplicating
operation logic in analyzer syntax walkers.

See [Operations API](../api/operations.md) and
[Operations implementation status](../api/operations-implementation-status.md).

## Workspace Integration

`RavenWorkspace` attaches analyzer references to projects and applies diagnostics through the
same options pipeline as compiler diagnostics:

- `RunAnalyzers=false` disables analyzer diagnostics for the project.
- `SpecificDiagnosticOptions` maps diagnostic severities and suppression.
- `ICompilationOptionsAwareAnalyzer.ShouldAnalyze` can skip a whole analyzer before it runs.
- Analyzer diagnostics are validated so external analyzers cannot use the reserved `RAV`
  diagnostic prefix.

The language server publishes syntax diagnostics quickly and schedules project-with-analyzers
diagnostics separately. Analyzers should be responsive under repeated document changes and
should honor cancellation.

## Testing APIs

Use `AnalyzerVerifier<TAnalyzer>` through `AnalyzerTestBase.CreateAnalyzerVerifier` for
workspace-backed analyzer tests. The verifier supports expected diagnostics, disabled
diagnostics, suggestion enablement, specific diagnostic options, and analyzer-specific
compilation modes such as returned-value handling.

See [Analyzer verifier](../development/testing/analyzer-verifier.md) and
[Verifying diagnostics](../development/testing/verifying-diagnostics.md).
