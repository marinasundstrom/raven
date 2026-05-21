# Compiler API performance guidance

Raven's compiler API is Roslyn-like: `Compilation`, `SyntaxTree`, `SemanticModel`,
symbols, diagnostics, and operations are the public boundary for analyzers, source
generators, refactorings, and language-server features. These APIs are safe to
use, but some calls intentionally widen the amount of compiler work. Use the
narrowest API that answers the question.

This is especially important for code that runs repeatedly while the user types,
or for generators and analyzers that visit many nodes.

## General rules

- Start with syntax. Use `SyntaxKind`, node shape, names, modifiers, and trivia
  to reject most candidates before asking semantic questions.
- Reuse the `SemanticModel` supplied by the current analyzer, document, or
  generator context. Raven caches per-tree semantic models, but asking for a
  model is still a semantic boundary and may force compilation setup.
- Query semantics at the smallest useful node. Prefer one semantic query on the
  declaration, invocation, member access, or type syntax that matters over many
  queries on all descendants.
- Cache results for the duration of the current pass. Metadata symbols,
  frequently used special types, and expensive per-declaration answers should
  usually be computed once.
- Keep generated syntax deterministic and incremental-friendly. Do not rebuild a
  compilation or reparse unchanged generated text just to inspect data that is
  already available from symbols or syntax.
- Thread through `CancellationToken` where the API accepts it. Live tooling
  should be able to abandon stale work quickly.

## Expensive operations and alternatives

| Instead of | Prefer | Why |
| --- | --- | --- |
| Calling `Compilation.Create`, `AddSyntaxTrees`, `AddReferences`, or `WithAssemblyName` repeatedly inside an analyzer or generator loop. | Build or update the compilation once per project snapshot, then reuse that instance for all queries in the pass. | Compilations are immutable. A new compilation can reuse some state, but it still creates a new semantic universe and can bypass work already done for the current snapshot. |
| Calling `compilation.GetSemanticModel(tree)` from every visited node. | Capture the model once per syntax tree, or use the model passed by the analyzer/refactoring/document context. | Raven caches semantic models by tree, unlike Roslyn's `GetSemanticModel` wording of returning a new model, but the call still crosses into compilation setup and obscures where semantic work is paid. |
| Calling `Compilation.GetDiagnostics()` to decide whether a node is interesting. | Use syntax filters first; if diagnostics are truly needed, ask for diagnostics at the current analysis boundary once. | Full compilation diagnostics walks syntax trees, binds semantic diagnostics, consults macro and entry-point diagnostics, applies suppression, and sorts results. |
| Calling `Compilation.GetDiagnostics(tree)` repeatedly for the same tree. | Compute it once per tree and reuse the immutable result, or use `SyntaxTree.GetDiagnostics()` when parser diagnostics are all you need. | Tree diagnostics still bind that tree's semantic model and include compilation-level diagnostics that belong to the tree. |
| Calling `SemanticModel.GetDiagnostics()` from a per-node callback. | Use targeted `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, or `GetOperation` calls for the node being analyzed. | Semantic diagnostics intentionally complete diagnostic binding for the tree; that is broader than most node-level checks need. |
| Calling `Compilation.Emit(...)` or using emit-only behavior as an analysis shortcut. | Query symbols, operations, diagnostics, or metadata shape directly. | Emit first ensures setup and diagnostics, then runs code generation. It is a build step, not a cheap semantic query. |
| Calling `GetEntryPoint()` or entry-point diagnostics from general analyzers. | Inspect top-level declarations or method symbols only if your rule is actually about entry-point shape. | Entry-point resolution can require cross-tree program-shape checks that are unrelated to ordinary node analysis. |
| Calling `SemanticModel.GetOperation(node)` for every statement or expression in a file. | Use syntax to select likely candidates, then call `GetOperation` on the topmost relevant statement or expression. | `GetOperation` may complete diagnostic binding before creating and caching the operation view. It is useful, but it is heavier than syntax and many simple symbol/type queries. |
| Calling `GetSymbolInfo` or `GetTypeInfo` on every identifier while searching for one pattern. | Filter by syntax kind and token text first; query the containing invocation, member access, assignment, pattern, or declaration once. | Symbol and type queries can bind expressions, run overload resolution, infer target types, and populate caches. |
| Calling `GetDeclaredSymbol` for every declaration in every tree before applying a rule. | Apply syntax filters first, then get the declared symbol only for matching declarations. | Declaration symbols are cheaper than full expression binding, but they still require declaration tables and member signatures to be available. |
| Walking `compilation.SyntaxTrees.SelectMany(t => t.GetRoot().DescendantNodes())` for each generated file or analyzer rule. | Use targeted syntax registration/provider-style filtering, or maintain incremental candidate sets keyed by syntax tree. | Whole-compilation syntax walks multiply quickly across rules and edits. Syntax is cheap relative to binding, but not free at solution scale. |
| Repeatedly calling `GetTypeByMetadataName` or resolving the same special type inside a per-node loop. | Resolve well-known symbols once per pass and compare with `SymbolEqualityComparer.Default`. | Metadata lookup is cached, but repeated string-based lookup still adds noise and makes the hot path harder to reason about. |
| Comparing symbols by display string, metadata-name string, or reference identity. | Use `SymbolEqualityComparer.Default` unless the API documents a different comparer. | String formatting can allocate and can hide generic/nullability distinctions. Reference equality can fail across equivalent constructed or reused symbols. |
| Generating source by inspecting formatted source text from semantic nodes. | Generate from syntax facts and symbol facts, then format the generated output once. | Converting semantic answers back into source text usually forces extra formatting, display-string, or traversal work. |

## Common scenarios

### Find declarations with a specific name

Do a syntax pass first:

```csharp
foreach (var candidate in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
{
    if (candidate.Identifier.Text != "Configure")
        continue;

    var symbol = semanticModel.GetDeclaredSymbol(candidate);
    // Inspect symbol only after the cheap name test passes.
}
```

Avoid resolving every method symbol and then filtering by `symbol.Name`.

### Find calls to a specific method

Filter by invocation shape and textual name first, then ask for the invoked
symbol:

```csharp
if (node is InvocationExpressionSyntax invocation &&
    invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
    memberAccess.Name.Identifier.Text == "Add")
{
    var method = semanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
    // Compare containing type and method identity here.
}
```

Avoid calling `GetSymbolInfo` for every identifier named `Add`; the invocation is
the semantic unit that owns overload resolution.

### Check a type relationship

Resolve the well-known target type once:

```csharp
var disposableType = compilation.GetTypeByMetadataName("System.IDisposable");

// Later, inside the filtered analysis path:
if (disposableType is not null &&
    SemanticFacts.ImplementsInterface(type, disposableType, SymbolEqualityComparer.Default))
{
    // Report or generate.
}
```

Avoid repeated metadata lookups and string comparisons against
`type.ToDisplayString()`.

### Generate code from source symbols

Collect a compact model from syntax and symbols, then generate from that model:

```csharp
var declaration = new GeneratedMember(
    ContainingType: typeSymbol,
    Name: methodSymbol.Name,
    ReturnType: methodSymbol.ReturnType);
```

Avoid keeping syntax nodes, semantic models, or compilations in long-lived
generator state. Keep only stable, comparable data needed for the generated
output.

## API owner expectations

Consumers should avoid broad compiler work in hot paths, but Raven API owners
should still keep the public compiler API responsible for correctness and cache
selection. Do not expose cache-specific or incremental-state helper APIs to
analyzers, source generators, refactorings, completion, hover, or LSP code.

If a public API such as `GetTypeInfo`, `GetSymbolInfo`, `GetDeclaredSymbol`, or
`GetOperation` is too expensive for an important live-tooling scenario, improve
that API's implementation or add a Roslyn-like public abstraction that preserves
the semantic boundary.
