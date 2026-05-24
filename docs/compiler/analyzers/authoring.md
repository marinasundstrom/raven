# Authoring Analyzers

An analyzer derives from `Raven.CodeAnalysis.Diagnostics.DiagnosticAnalyzer`, defines one or
more `DiagnosticDescriptor` instances, registers analysis callbacks in `Initialize`, and
reports diagnostics through the supplied analysis context.

## Minimal Analyzer Shape

```csharp
using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;

public sealed class ExampleAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "AN0001";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Example diagnostic",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Example diagnostic for '{0}'.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeIdentifier, SyntaxKind.IdentifierName);

    private static void AnalyzeIdentifier(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not IdentifierNameSyntax identifier)
            return;

        if (identifier.Identifier.ValueText == "example")
        {
            context.ReportDiagnostic(Diagnostic.Create(
                Descriptor,
                identifier.GetLocation(),
                identifier.Identifier.ValueText));
        }
    }
}
```

## Diagnostic Descriptors

Each reported diagnostic must be backed by a `DiagnosticDescriptor`.

- Use a stable `id`.
- Use a short `title`.
- Keep `messageFormat` concise and action-oriented.
- Put user-adjustable severity in `defaultSeverity`, then let `.editorconfig` remap it.
- Use placeholders in `messageFormat` for values that vary per report.
- Do not vary the message text based on severity.

Built-in Raven analyzers use `RAV` IDs, normally in the `RAV9000` range. External analyzers
must not report diagnostics with the reserved `RAV` prefix; workspace validation rejects
external analyzer diagnostics that do so. Built-in analyzer IDs must not duplicate compiler
diagnostics.

## Registering Work

Use the narrowest registration that can answer the question:

- `RegisterSyntaxNodeAction` for checks rooted at specific syntax kinds.
- `RegisterSyntaxTreeAction` for file-wide checks.

Prefer syntax-node actions when the analyzer can naturally start from a declaration,
statement, expression, or pattern. Avoid scanning the full syntax tree from every registered
node.

## Semantic Analysis

Use `SyntaxNodeAnalysisContext.SemanticModel` for semantic facts:

- `GetDeclaredSymbol` for declarations.
- `GetSymbolInfo` for references and invocations.
- `GetTypeInfo` for expression and conversion types.
- `GetOperation` when statement or expression shape is easier to reason about through the
  operations API.

Analyzer code should not depend on binder internals, incremental caches, or language-server
state. If a public semantic API is too expensive or incomplete, fix the compiler API rather
than bypassing it in the analyzer.

## Diagnostic Queries From Analyzers

Analyzers should not normally ask the compiler or workspace for diagnostics while they are
running. Diagnostic production is its own compiler pipeline, and broad diagnostic queries can
force binding, re-enter analyzer execution, or make analyzer results depend on scheduling
order.

- Do not call `Compilation.GetDiagnostics`, `Compilation.GetDocumentDiagnostics`,
  `SemanticModel.GetDiagnostics`, `SemanticModel.GetDocumentDiagnostics`, workspace
  diagnostic APIs, or diagnostic-with-analyzers APIs from ordinary analyzer callbacks.
- If an analyzer only needs to avoid malformed syntax, use `SyntaxTree.GetDiagnostics()` for
  parser diagnostics and return early on syntax errors.
- If an analyzer needs semantic facts, use targeted semantic APIs such as `GetSymbolInfo`,
  `GetTypeInfo`, `GetDeclaredSymbol`, or `GetOperation` for the node being analyzed.
- If an analyzer needs to skip files with compiler errors, that should be modeled as
  analyzer-driver or context behavior, not as each analyzer recursively querying semantic
  diagnostics.
- Code fixes should use the diagnostics supplied by `CodeFixContext`. A fix provider must
  not recompute compiler or analyzer diagnostics to decide which fixes to offer.

This keeps analyzer execution deterministic, cancellation-friendly, and compatible with lazy
binder-owned semantic state.

## Operations API

Prefer operations when the analyzer is about semantic expression shape rather than raw syntax.
For example, returned-value checks should reason about `IExpressionStatementOperation`,
`IInvocationOperation`, `IPropertyReferenceOperation`, and related operation interfaces rather
than parsing member-access source text manually.

Operations are intentionally Roslyn-like. If an analyzer needs an operation shape that does
not exist yet, add the missing operation API and test it before building analyzer logic around
syntax workarounds.

## Configuration-Aware Analyzers

Analyzer participation and diagnostic severity are separate concerns.

- Project files and compilation options decide whether a whole analyzer mode runs.
- `.editorconfig` decides the severity of diagnostics that are reported.

If a built-in analyzer needs a project-file mode, implement
`ICompilationOptionsAwareAnalyzer`:

```csharp
public sealed class ExampleAnalyzer : DiagnosticAnalyzer, ICompilationOptionsAwareAnalyzer
{
    public bool ShouldAnalyze(CompilationOptions options)
        => options.SomeAnalyzerMode == SomeAnalyzerMode.Full;
}
```

Also keep a cheap guard inside expensive callbacks when practical. This makes direct
`Analyze` calls behave the same way as workspace-scheduled analyzer runs.

Do not use `DiagnosticDescriptor` disabled-by-default state or
`SpecificDiagnosticOptions` to decide whether an analyzer runs. Severity configuration must
not implicitly enable an opt-in analyzer.

## Error Handling And Cancellation

Analyzers should tolerate incomplete, erroneous, or partially typed source.

- Return without reporting when required semantic facts are unavailable.
- Let `OperationCanceledException` propagate.
- Avoid throwing for normal source shapes. Workspace and analyzer runners isolate analyzer
  failures, but swallowed failures produce missing diagnostics and poor editor feedback.
- Check `context.CancellationToken` in long loops.

## Testing

Use [Analyzer verifier](../development/testing/analyzer-verifier.md) for normal analyzer
coverage. Add tests for:

- the positive diagnostic case, including span and message arguments;
- no-report cases for handled, valid, or incomplete source;
- severity remapping through `specificDiagnosticOptions`;
- project-mode behavior for opt-in analyzers;
- document-scoped behavior when the analyzer is expected to run in the language server.

For diagnostic-only tests that do not need analyzer execution, use
[Verifying diagnostics](../development/testing/verifying-diagnostics.md).

## Documentation Checklist

When adding a built-in analyzer:

- Add or update a row in the [built-in analyzer inventory](../development/analyzers.md).
- Add configuration notes when the analyzer has project-file modes.
- Add changelog coverage for user-visible behavior.
- Update code-fix or refactoring docs if the diagnostic has a fix.
