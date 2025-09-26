# Syntax Tree API

The syntax tree API exposes the immutable structure produced by the parser. It
follows the familiar Roslyn split between a compact "green" tree and a richer
"red" tree so callers can efficiently inspect and transform source code without
sacrificing reuse.

---

## Creating syntax trees

The entry point is `SyntaxTree.ParseText`, which accepts raw text or a
`SourceText` instance. Parsing returns a `SyntaxTree` whose root is a
`CompilationUnitSyntax` node:

```csharp
var tree = SyntaxTree.ParseText(source);
var root = tree.GetRoot();
```

`SyntaxTree.Create` performs the inverse: it wraps an existing red root (for
example, one produced by a rewriter) back into a tree so it can participate in a
`Compilation`.

Each tree retains its original `SourceText`. Calls to `GetText` and
`TryGetText` either return the parsed text or lazily reconstruct it from the
current root when the source was produced from a red tree.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L8-L104】

## Navigating nodes and tokens

Red `SyntaxNode` and `SyntaxToken` instances expose parents, spans, and helpers
such as `DescendantNodes`/`DescendantTokens`. The `SyntaxTree` surface adds
convenience methods for locating nodes: `GetNodesInSpan` returns the innermost
nodes that cover a `TextSpan`, `GetNodeForSpan` gives the first match, and
`GetNodeToReplace` normalizes zero-width insertions to the containing node.
`GetLocation` turns a span into a `Location` tied to the tree for diagnostics.
Tree-level `GetDiagnostics` overloads aggregate parser diagnostics for the whole
file, a particular node/token, or a span.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L46-L139】

For inspection and tooling, syntax trees can be printed using the `PrintSyntaxTree`
extension showcased in the API README. That routine walks the red tree and emits
kind names, spans, and trivia so callers can visualize structure.

## Visitors and rewriters

The compiler ships source-generated visitor bases that align with Roslyn's
patterns. `SyntaxVisitor` and `SyntaxVisitor<T>` offer traversal hooks for every
node type, while `SyntaxRewriter` enables immutable transformations by producing
updated nodes. The generator maintains these APIs so consumers can override only
the members they care about and rely on default traversal for the rest.【F:docs/compiler/architecture/visitors-and-rewriters.md†L1-L40】

## Incremental updates

`WithChangedText` rebuilds a tree from edited source and shares unchanged green
nodes where possible. The API exposes text changes via `GetChanges`, allowing
callers to diff old and new trees or integrate with workspace features.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L68-L93】【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L141-L178】

When used with `Compilation.With...` helpers, new syntax trees can be swapped in
without mutating existing compilations. This workflow underpins the compiler's
incremental model and keeps tree operations predictable for IDE scenarios.
