# Analyzer API Surface

This document lists the Raven APIs analyzer authors are expected to use. The API is intended
to stay Roslyn-like where possible, with Raven-specific syntax and symbol types where the
language requires them.

## Core Types

`DiagnosticAnalyzer`
: Base class for source analyzers. Implement `Initialize` and register syntax-tree,
syntax-node, symbol, operation, or compilation callbacks.

`AnalysisContext`
: Registration surface passed to `DiagnosticAnalyzer.Initialize`.

`SyntaxTreeAnalysisContext`
: Context for file-wide analyzer callbacks. Provides `SyntaxTree`, `Compilation`,
`ReportDiagnostic`, and `CancellationToken`.

`SyntaxNodeAnalysisContext`
: Context for syntax-node callbacks. Provides `Node`, `SemanticModel`, `Compilation`,
`ReportDiagnostic`, and `CancellationToken`.

`SymbolAnalysisContext`
: Context for declared-symbol callbacks. Provides `Symbol`, `Compilation`,
`ReportDiagnostic`, and `CancellationToken`.

`OperationAnalysisContext`
: Context for semantic-operation callbacks. Provides `Operation`, `SemanticModel`,
`SyntaxTree`, `Compilation`, `ReportDiagnostic`, and `CancellationToken`.

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

`EnableConcurrentExecution()`
: Allows this analyzer to run concurrently with other analyzers that also opted in. Use this
  only when analyzer callbacks are stateless for each run and honor cancellation.

`RegisterSyntaxTreeAction(Action<SyntaxTreeAnalysisContext>)`
: Runs once per syntax tree. Use for checks that need file-wide context.

`RegisterSyntaxNodeAction(Action<SyntaxNodeAnalysisContext>, params SyntaxKind[])`
: Runs for matching syntax nodes. Use for most analyzers. The action is document-scoped by
default for safe invalidation.

`RegisterSyntaxNodeAction(Action<SyntaxNodeAnalysisContext>, SyntaxNodeAnalysisScope, params SyntaxKind[])`
: Runs for matching syntax nodes with an explicit invalidation scope. Use
`SyntaxNodeAnalysisScope.Node` only for diagnostics that are local to the analyzed node and
its stable semantic context.

`RegisterSymbolAction(Action<SymbolAnalysisContext>, params SymbolKind[])`
: Runs for declared symbols whose `SymbolKind` matches one of the requested kinds. Use this
  for declaration-owned checks such as parameters, methods, properties, fields, and types.
  Symbol actions let the workspace use declaration identity as the scheduling and
  invalidation unit instead of asking each analyzer to rediscover declarations from syntax.

`RegisterOperationAction(Action<OperationAnalysisContext>, params OperationKind[])`
: Runs for semantic operations whose `OperationKind` matches one of the requested kinds. Use
  this for executable-behavior checks such as ignored invocation results, property
  references, assignments, awaits, conditional accesses, and conversions. Operation actions
  let the workspace build and traverse operation trees once for a document and dispatch all
  matching analyzer callbacks from that shared pass.

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

Semantic queries may trigger binding. The compiler owns the caches and incremental state;
analyzers should not reach into binder internals or language-server caches.

Semantic queries are intentionally non-reporting: `GetSymbolInfo`, `GetTypeInfo`,
`GetDeclaredSymbol`, and `GetOperation` may bind narrowly to answer the requested question,
but they should not publish or cache compiler diagnostics as a side effect. Diagnostic
collection is a separate compiler pipeline. This separation lets hover, completion, analyzers,
and code fixes ask semantic questions without poisoning later diagnostics or depending on
diagnostic scheduling order.

Symbols returned from different semantic queries should be treated as equivalent semantic
values, not as durable object identities. Lazy binding, operation creation, diagnostic
binding, and future incremental snapshots may return different `ISymbol` instances for the
same declaration or metadata member. Use `SymbolEqualityComparer.Default` for symbol
comparison and for `HashSet<ISymbol>`/`Dictionary<ISymbol, ...>` keys unless a specific API
documents a narrower comparer.

Avoid diagnostic-producing APIs from analyzer callbacks. `Compilation.GetDiagnostics`,
`Compilation.GetDocumentDiagnostics`, `SemanticModel.GetDiagnostics`,
`SemanticModel.GetDocumentDiagnostics`, and workspace diagnostic APIs can force broad
semantic diagnostic binding or analyzer execution. Use `SyntaxTree.GetDiagnostics()` only
as a syntax-error guard, and use targeted semantic APIs for the facts the analyzer actually
needs.

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

Use `RegisterOperationAction` to enter the operations model when the analyzer is naturally
about operation shape. Use `SemanticModel.GetOperation(node)` inside syntax or symbol actions
when those actions are still the correct scheduling unit and only need one targeted
operation. If an analyzer requires a missing operation node or property, add that API and its
tests rather than duplicating operation logic in analyzer syntax walkers.

See [Operations API](../api/operations.md) and
[Operations implementation status](../api/operations-implementation-status.md).

## Workspace Integration

`RavenWorkspace` attaches analyzer references to projects and applies diagnostics through the
same options pipeline as compiler diagnostics:

- `RunAnalyzers=false` disables analyzer diagnostics for the project.
- `DisabledAnalyzers` / `RavenDisabledAnalyzers` disables individual built-in analyzers by
  analyzer type name or fully qualified type name.
- `SpecificDiagnosticOptions` maps diagnostic severities and suppression.
- `ICompilationOptionsAwareAnalyzer.ShouldAnalyze` can skip a whole analyzer before it runs.
- Analyzer diagnostics are validated so external analyzers cannot use the reserved `RAV`
  diagnostic prefix.

The language server publishes syntax diagnostics quickly and schedules project-with-analyzers
diagnostics separately. Pending, skipped, or canceled analyzer work is not an empty analyzer
result: previous analyzer diagnostics remain visible for unchanged ranges until a newer
successful analyzer lane replaces them. Analyzers should be responsive under repeated document
changes and should honor cancellation.

## Testing APIs

Use `AnalyzerVerifier<TAnalyzer>` through `AnalyzerTestBase.CreateAnalyzerVerifier` for
workspace-backed analyzer tests. The verifier supports expected diagnostics, disabled
diagnostics, suggestion enablement, specific diagnostic options, and analyzer-specific
compilation modes such as returned-value handling.

See [Analyzer verifier](../development/testing/analyzer-verifier.md) and
[Verifying diagnostics](../development/testing/verifying-diagnostics.md).
