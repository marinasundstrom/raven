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
    {
        context.EnableConcurrentExecution();

        context.RegisterSyntaxNodeAction(AnalyzeIdentifier, SyntaxKind.IdentifierName);
    }

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
- `RegisterSymbolAction` for checks rooted at declared symbol identity.
- `RegisterSyntaxTreeAction` for file-wide checks.

Prefer syntax-node actions when the analyzer can naturally start from a declaration,
statement, expression, or pattern. Avoid scanning the full syntax tree from every registered
node.

Prefer symbol actions when the diagnostic belongs to a declaration rather than to the exact
syntax spelling of the declaration. Examples include unused parameters, unused members,
visibility suggestions, and declaration-shape rules that should be scheduled by method,
property, field, type, or other symbol identity. The analyzer callback should inspect the
provided symbol, ask for its declaring syntax only when needed, and use targeted semantic
queries for references or operations.

Syntax-node actions are document-scoped by default. This is intentionally conservative:
many checks start from one node but depend on references, declarations, or semantic facts
elsewhere in the document. Use the explicit `SyntaxNodeAnalysisScope.Node` overload only when
the diagnostic is truly local to the analyzed node and its stable semantic context.

The workspace analyzer driver is intentionally Roslyn-like: analyzers register callbacks,
and the workspace decides when and how to invoke those callbacks. A cold document analysis may
walk the whole syntax tree once and dispatch all registered node actions from that pass. After
edits, the driver is expected to reuse cached analyzer results and rerun only the actions whose
registered syntax or symbol scopes were invalidated by the change. Analyzer authors should
therefore describe their work through narrow actions instead of building their own whole-file
or whole-project traversal loops.

## Concurrent Execution

Call `context.EnableConcurrentExecution()` from `Initialize` when the analyzer can safely run
at the same time as other analyzers. This mirrors Roslyn's analyzer contract and is expected
for analyzers that do meaningful semantic work in the language server.

An analyzer that enables concurrent execution must be stateless for each run:

- Do not store mutable analysis results on the analyzer instance.
- Do not store mutable analysis results in static fields.
- Use local variables inside callbacks for single-action state.
- Use local thread-safe collections when future compilation-start or compilation-end style
  APIs need to aggregate facts across multiple callbacks.
- Assume callbacks from different analyzers may run concurrently against the same
  compilation snapshot.
- Always honor `context.CancellationToken` in long loops.

Raven currently runs concurrent analyzers independently and merges their diagnostics in a
deterministic order before publishing. Enabling concurrency does not change semantic truth:
symbols, types, operations, and compiler diagnostics still come from `Raven.CodeAnalysis`.

## State And Incrementality

Treat analyzer instances as stateless executors. `Initialize` should register actions and
descriptor metadata; it should not capture mutable semantic state that must survive across
projects, compilations, documents, or edits.

- Do not store mutable analysis results in static fields.
- Do not store mutable per-compilation or per-document state on the analyzer instance.
- Use local variables inside an analysis callback for single-action state.
- If a future Raven API provides compilation-start, symbol-start, or document-start analysis
  contexts, use those contexts for explicitly scoped per-run state.
- Assume callbacks may be invoked independently, in a different order, concurrently, or only
  for syntax/symbol scopes affected by an edit.

This mirrors the compiler's binder-owned cache direction at the workspace level: the workspace
owns analyzer scheduling, cache reuse, and invalidation; analyzer callbacks describe what they
need to inspect and report deterministic diagnostics for the supplied scope.

For invalidation, think of each registered action as an independent unit of work:

- a document-scoped syntax-node action is tied to the registered syntax kinds and the
  containing document;
- a node-scoped syntax-node action is tied to the registered syntax kinds and may be reused
  for unchanged nodes;
- a symbol action is tied to declarations of the registered symbol kinds and the semantic
  context needed to analyze those declarations;
- a syntax-tree action is tied to the whole document;
- a compilation action is tied to project-wide inputs unless the analyzer only uses the
  document supplied by the context.

The driver may cache diagnostics per action and scope, rerun only invalidated actions after an
edit, and merge diagnostics from many actions. Analyzer callbacks should therefore be pure with
respect to the supplied context: they may read syntax, symbols, options, and semantic facts, and
they may report diagnostics, but they should not rely on another analyzer or another action
running before them.

## Semantic Analysis

Use `SyntaxNodeAnalysisContext.SemanticModel` for semantic facts:

- `GetDeclaredSymbol` for declarations.
- `GetSymbolInfo` for references and invocations.
- `GetTypeInfo` for expression and conversion types.
- `GetOperation` when statement or expression shape is easier to reason about through the
  operations API.

Use `SymbolAnalysisContext.Symbol` when the analyzer was registered with
`RegisterSymbolAction`. If the analyzer needs syntax for that declaration, use the symbol's
declaring syntax references and then obtain the semantic model for that syntax tree from the
compilation. Keep this path narrow: a symbol action should not turn into a full-document scan.

Analyzer code should not depend on binder internals, incremental caches, or language-server
state. If a public semantic API is too expensive or incomplete, fix the compiler API rather
than bypassing it in the analyzer.

Semantic symbols must be compared with `SymbolEqualityComparer.Default`, not by reference
identity and not by display text. Raven binds lazily and may answer semantic queries from
different compiler-owned paths: declaration binding, diagnostic binding, operation creation,
or a later focused semantic query. Those paths can produce different symbol object instances
that still represent the same source declaration or metadata member.

Use comparer-backed collections whenever an analyzer records candidates and later matches
references:

```csharp
var candidates = new Dictionary<ISymbol, Location>(SymbolEqualityComparer.Default);
var used = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

var declared = context.SemanticModel.GetDeclaredSymbol(declaration);
if (declared is not null)
    candidates[declared] = declaration.GetLocation();

var referenced = context.SemanticModel.GetSymbolInfo(identifier).Symbol;
if (referenced is not null)
    used.Add(referenced);
```

Do not write analyzer logic that depends on `object.ReferenceEquals`, default
`HashSet<ISymbol>`/`Dictionary<ISymbol, ...>` equality, `ToDisplayString()`, or metadata-name
strings for identity. Strings are useful for presentation and cheap syntax prefilters; they
are not the semantic identity of a symbol.

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
- `RavenDisabledAnalyzers` can disable individual built-in analyzers for a project by
  analyzer type name. Use this for coarse participation control or performance isolation,
  not for ordinary diagnostic severity policy.

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
