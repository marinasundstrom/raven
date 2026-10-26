# Compiler API performance guidance

Raven's compiler API is Roslyn-like: `Compilation`, `SyntaxTree`, `SemanticModel`,
symbols, diagnostics, and operations are the public boundary for analyzers, source
generators, refactorings, and language-server features. These APIs are safe to
use, but some calls intentionally widen the amount of compiler work. Use the
narrowest API that answers the question.

The compiler's live-editing architecture connects compiler-owned semantic
state, workspace analyzer scheduling, and language-server request lanes.

This is especially important for code that runs repeatedly while the user types,
or for generators and analyzers that visit many nodes.

## General rules

- Start with syntax. Use `SyntaxKind`, node shape, names, modifiers, and trivia
  to reject most candidates before asking semantic questions.
- Register analyzer actions narrowly and let the workspace/analyzer driver own
  traversal, caching, and invalidation. A cold pass may walk a document once;
  after edits the driver should be free to rerun only invalidated syntax or
  symbol scopes.
- Reuse the `SemanticModel` supplied by the current analyzer, document, or
  generator context. Raven caches per-tree semantic models, but asking for a
  model is still a semantic boundary and may force compilation setup.
- Query semantics at the smallest useful node. Prefer one semantic query on the
  declaration, invocation, member access, or type syntax that matters over many
  queries on all descendants.
- Do not query broad diagnostics from inside analyzers, code fixes, refactorings,
  or other live-tooling callbacks unless that callback was explicitly created to
  compute diagnostics. Syntax diagnostics are acceptable as a malformed-source
  guard; semantic diagnostics can force binding and analyzer execution.
- Cache results for the duration of the current pass. Metadata symbols,
  frequently used special types, and expensive per-declaration answers should
  usually be computed once.
- Keep generated syntax deterministic and incremental-friendly. Do not rebuild a
  compilation or reparse unchanged generated text just to inspect data that is
  already available from symbols or syntax.
- Thread through `CancellationToken` where the API accepts it. Live tooling
  should be able to abandon stale work quickly.
- Keep analyzers stateless. Do not store mutable semantic results in analyzer
  instance fields or static fields; keep per-run state local to callbacks or
  future explicit start-analysis contexts.
- Compare symbols with `SymbolEqualityComparer.Default`. Equivalent symbols can
  be returned as different object instances from lazy binding, diagnostic
  binding, operations, metadata loading, or later incremental snapshots.

## Expensive operations and alternatives

| Instead of | Prefer | Why |
| --- | --- | --- |
| Calling `Compilation.Create`, `AddSyntaxTrees`, `AddReferences`, or `WithAssemblyName` repeatedly inside an analyzer or generator loop. | Build or update the compilation once per project snapshot, then reuse that instance for all queries in the pass. | Compilations are immutable. A new compilation can reuse some state, but it still creates a new semantic universe and can bypass work already done for the current snapshot. |
| Calling `compilation.GetSemanticModel(tree)` from every visited node. | Capture the model once per syntax tree, or use the model passed by the analyzer/refactoring/document context. | Raven caches semantic models by tree, unlike Roslyn's `GetSemanticModel` wording of returning a new model, but the call still crosses into compilation setup and obscures where semantic work is paid. |
| Calling `Compilation.GetDiagnostics()` to decide whether a node is interesting. | Use syntax filters first; if diagnostics are truly needed, ask for diagnostics at the current analysis boundary once. | Full compilation diagnostics walks syntax trees, binds semantic diagnostics, consults macro and entry-point diagnostics, applies suppression, and sorts results. |
| Calling `Compilation.GetDiagnostics(tree)` repeatedly for the same tree. | Compute it once per tree and reuse the immutable result, or use `SyntaxTree.GetDiagnostics()` when parser diagnostics are all you need. | Tree diagnostics still bind that tree's semantic model and include compilation-level diagnostics that belong to the tree. |
| Calling `SemanticModel.GetDiagnostics()` from a per-node callback. | Use targeted `GetSymbolInfo`, `GetTypeInfo`, `GetDeclaredSymbol`, or `GetOperation` calls for the node being analyzed. | Semantic diagnostics intentionally complete diagnostic binding for the tree; that is broader than most node-level checks need. |
| Calling workspace diagnostic APIs from a code-fix provider to discover other diagnostics. | Use the diagnostics supplied by the code-fix context, or let the LSP/workspace diagnostic lane compute diagnostics separately. | Code actions run interactively and should not trigger analyzer or compiler diagnostic recomputation. |
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

```raven
for candidate in root.DescendantNodes().OfType<MethodDeclarationSyntax>() {
    if (candidate.Identifier.Text != "Configure")
        continue

    let symbol = semanticModel.GetDeclaredSymbol(candidate)
    // Inspect symbol only after the cheap name test passes.
}
```

Avoid resolving every method symbol and then filtering by `symbol.Name`.

### Find calls to a specific method

Filter by invocation shape and textual name first, then ask for the invoked
symbol:

```raven
if let invocation: InvocationExpressionSyntax = node {
    if let memberAccess: MemberAccessExpressionSyntax = invocation.Expression {
        if memberAccess.Name.Identifier.Text == "Add" {
            let method = semanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol
            // Compare containing type and method identity here.
        }
    }
}
```

Avoid calling `GetSymbolInfo` for every identifier named `Add`; the invocation is
the semantic unit that owns overload resolution.

### Check a type relationship

Resolve the well-known target type once:

```raven
let disposableType = compilation.GetTypeByMetadataName("System.IDisposable")

// Later, inside the filtered analysis path:
if disposableType is not null &&
    SemanticFacts.ImplementsInterface(type, disposableType, SymbolEqualityComparer.Default) {
    // Report or generate.
}
```

Avoid repeated metadata lookups and string comparisons against
`type.ToDisplayString()`.

### Generate code from source symbols

Collect a compact model from syntax and symbols, then generate from that model:

```raven
let declaration = GeneratedMember(
    ContainingType: typeSymbol,
    Name: methodSymbol.Name,
    ReturnType: methodSymbol.ReturnType)
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

## Text processing allocations

Source edits concatenate spans of the immutable source into the new snapshot's
string, avoiding intermediate prefix and suffix strings. Multiline literal
indentation trimming appends spans to the value builder without materializing
each line. Identifier and keyword scanning materializes and interns token text
once, then reuses it for classification and token construction.

These are compiler-host optimizations shared by main and `codex/neoclr`.
Public API signatures, snapshot ownership, token values, newline handling,
Runtime Contract configuration, and emitted metadata are unchanged. No source
buffer slices are retained in syntax tokens.

The allocation probe in `tools/CompilerTextAllocationProbe` measures complete
`SourceText.WithChange` and `SyntaxTree.ParseText` calls. It preconstructs inputs,
warms each workload for 100 operations, then reports thread-local allocated bytes
per operation for three batches of 500 operations. Repeated inputs also warm
string interning; the figures do not describe cold unique-input retention.
Build the compiler first, then run the probe against that configuration:

```sh
dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj -f net11.0 -c Debug --property WarningLevel=0
dotnet run --project tools/CompilerTextAllocationProbe -c Release -p:CompilerConfiguration=Debug
```

Use the same SDK, compiler configuration, probe and inputs before and after a
change. Debug parser measurements include debug/instrumentation overhead; do
not extrapolate their percentages to Release compilation or execution on a
target runtime. This probe measures allocations, not elapsed-time speedup.

### Measurement on 2026-10-26

Measured with SDK `11.0.100-rc.1.26425.128`, a Release probe and Debug/net11.0
compiler assemblies. Values below are median bytes allocated per operation
across the three batches (integer bytes/op). Baselines were main `f8a27dd82`
and `codex/neoclr` `aa7406718`; after measurements include the changes described
above. The edit workload replaces one character in a 55,296-character source.

| Workload | main before | main after | neoCLR branch before | neoCLR branch after | Reduction |
| --- | ---: | ---: | ---: | ---: | ---: |
| Mid-file source edit | 254,424 | 143,784 | 254,424 | 143,784 | 43.49% |
| Parse 256 identifier declarations | 2,925,109 | 2,826,176 | 2,925,203 | 2,826,267 | 3.38% |
| Parse 256 indented string lines | 692,578 | 653,095 | 692,601 | 653,117 | 5.70% |

The first post-change identifier batch varied slightly (up to 2,828,175 bytes/op);
subsequent batches stabilized at the table values. Inputs and measured API calls
were identical before and after.

Validation on each branch: 120 focused tests passed before the changes and 131
passed afterward, covering source text, lexing, string literals and interpolation.
The 11 added cases check edit boundaries/snapshot preservation and six newline
forms with matching and nonmatching indentation. Compiler builds passed for
net10.0 and net11.0; tests and allocation measurements ran on .NET 11. These
results do not establish execution on neoCLR, .NET Framework or NanoFramework.
