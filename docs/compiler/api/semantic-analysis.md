# Semantic Analysis API

Semantic analysis is orchestrated by the `Compilation` and its per-tree
`SemanticModel`. Together they resolve symbols, infer types, track diagnostics,
and expose higher-level views such as operations.

---

## Building a compilation

Create a `Compilation` by supplying syntax trees (and optional references):

```csharp
var tree = SyntaxTree.ParseText(source);
var compilation = Compilation.Create("App", [tree]);
```

Compilations are immutable—`AddSyntaxTrees`, `AddReferences`, and
`WithAssemblyName` return new instances that share existing state. Each
compilation manages a binder factory, declaration table, and synthesized symbols
for features such as delegates and the entry point.【F:src/Raven.CodeAnalysis/Compilation.cs†L1-L130】

## Accessing semantic models

`GetSemanticModel` materializes a `SemanticModel` for a specific tree. The
compilation caches models so repeated calls return the same instance. Models are
created lazily when diagnostics, symbol queries, type queries, operations, or
language-service features require them.【F:src/Raven.CodeAnalysis/Compilation.cs†L82-L123】

Each semantic model wires up the binder chain for the tree's root during
construction. Binders are the execution units that own derived semantic state for
their syntax and scope: parameters, locals, labels, pattern variables, bound
expressions, and binder-produced diagnostics. Public semantic queries may create
or reuse the responsible binder state for the requested node; diagnostic
collection uses the same binders but remains a separate reporting pipeline.

## Diagnostics

`SemanticModel.GetDiagnostics` consolidates binder diagnostics, deduplicates
entries, and caches the resulting immutable array. It triggers binding for the
current tree if diagnostics have not yet been collected.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L23-L45】

At the tree level, `SyntaxTree.GetDiagnostics` provides parser diagnostics. A
compilation's `Emit` and `GetEntryPoint` flows also surface aggregated
information when generating IL or determining the program entry point.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L46-L67】【F:src/Raven.CodeAnalysis/Compilation.cs†L124-L206】

## Symbol and type queries

`GetSymbolInfo` retrieves the symbol associated with a syntax node by binding the
node (or using cached results). For declarations, `GetDeclaredSymbol` resolves
keys from the declaration table and uses the symbol factory to return the
canonical symbol. `GetTypeInfo` has overloads for expressions and type syntax,
returning both the inferred and converted types when available.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L47-L143】

These query APIs are non-reporting. They may bind narrowly to answer the
requested question, and the result may be reused by later queries, but they should
not publish compiler diagnostics or make analyzer results depend on diagnostic
collection order. If a query needs semantic meaning, prefer the public semantic
API that owns that meaning instead of reading binder caches directly.

Do not compare returned symbols by object reference. Lazy binding, diagnostic
binding, operation creation, metadata loading, and future incremental snapshots
can return different `ISymbol` instances that represent the same declaration or
metadata member. Use `SymbolEqualityComparer.Default` for equality, hash sets,
dictionaries, distinctness, and comparisons with well-known symbols unless a
specific API documents otherwise.

Closure-aware tooling can query capture data directly from `SemanticModel`:

- `GetCapturedVariables(ISymbol)` and `GetCapturedVariables(SyntaxNode)` return
  the symbols captured by local functions and lambdas.
- `IsCapturedVariable(ISymbol)` reports whether a local/parameter is captured by
  any closure in the current syntax tree.

These APIs are used by the language server hover experience to annotate closure
information for function declarations and variable references.

These APIs cooperate with the binder hierarchy described in the semantic binding
architecture notes. Binders chain by scope so lookups naturally fall back to
parent contexts while preserving local information.【F:docs/compiler/architecture/semantic-binding.md†L1-L64】

### Symbol facts helpers

The compiler exposes relationship helpers in `SemanticFacts` for advanced
analysis scenarios. `IsDerivedFrom` walks base types (including type-parameter
constraints) while `ImplementsInterface` traverses the interface closure (and
considers interface identity), and both accept optional
`SymbolEqualityComparer` instances so tooling can align with Roslyn's equality
semantics when desired. Array types mirror Roslyn's behavior by surfacing their
single-dimensional `IList<T>`/`IReadOnlyList<T>` contracts directly on the array
symbol, letting the helpers observe Roslyn-style constructed interfaces via the
array's element type.【F:src/Raven.CodeAnalysis/SemanticFacts.cs†L1-L120】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ArrayTypeSymbol.cs†L1-L137】

## Operations

For syntax-agnostic analysis, call `SemanticModel.GetOperation`. The semantic
model caches realized operations and uses `OperationFactory` to translate bound
nodes into `IOperation` instances that mirror Roslyn's shape. The dedicated
[Operations API](operations.md) document covers the available kinds, caching
behavior, and current limitations.【F:src/Raven.CodeAnalysis/SemanticModel.Operations.cs†L1-L45】【F:docs/compiler/api/operations.md†L1-L55】
