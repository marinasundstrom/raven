# Operations API

The operations API exposes a tree of semantic nodes that sit between the
binder's `BoundNode` graph and the syntax tree. It mirrors the `IOperation`
surface from Roslyn so tooling and analyzers can reason about Raven programs
using a stable, syntax-agnostic shape. Every concrete operation now has a
dedicated class (for example, `BinaryOperation`, `InvocationOperation`, and
`ReturnOperation`) so analyzers can inspect well-named properties rather than
working with anonymous nodes.

---

## Concepts

- **Operation** – An immutable semantic node backed by an `IOperation`
  implementation. Each operation knows the `SemanticModel` that created it, its
  originating syntax, and the value `Type` (if any).
- **OperationKind** – An enum that categorizes each concrete operation. Kinds
  follow Roslyn's naming where possible so existing analyzers can be ported.
- **Implicit operations** – Operations synthesized by the compiler rather than
  appearing directly in source. The `IsImplicit` flag distinguishes them from
  syntax-backed operations.

Operations form a tree rooted at the syntax node that produced them. The tree is
constructed lazily—child operations are only realized when they are accessed.
Each operation caches the typed symbol information from the corresponding bound
node (such as referenced locals, methods, or properties) and reuses
`SemanticModel.GetOperation` to populate children on demand.

### Walking and rewriting

The `tools/OperationGenerator` utility emits strongly typed visitors for the
operation tree:

```bash
(cd src/Raven.CodeAnalysis && dotnet run --project ../../tools/OperationGenerator)
```

- `OperationVisitor` / `OperationVisitor<TResult>` expose a `Visit{Kind}` method
  for every operation kind so analyzers can override only the shapes they care
  about.
- `OperationWalker` dispatches to those methods while defaulting to walking
  child operations.
- `OperationRewriter` mirrors the walker but returns the visited operation so
  transforms can be implemented incrementally.

## Retrieving operations

Use `SemanticModel.GetOperation` to obtain the operation that corresponds to a
syntax node:

```csharp
var model = compilation.GetSemanticModel(tree);
var ifStatement = tree.GetRoot().DescendantNodes()
    .OfType<IfStatementSyntax>()
    .First();

var operation = model.GetOperation(ifStatement);
```

`GetOperation` returns `null` when no operation exists for the supplied node.
Bound nodes are pulled from the binder cache, so the call does not trigger a new
binding pass.

### Caching

`SemanticModel` caches every realized operation. Repeated calls for the same
syntax node return the exact instance, ensuring parent/child links remain stable
and visitors can rely on reference equality.

## Navigating the operation tree

Operations expose their children through the `Children` property and can be
traversed with the provided visitor base types:

```csharp
public sealed class ControlFlowCollector : OperationVisitor
{
    public override void DefaultVisit(IOperation operation)
    {
        if (operation.Kind is OperationKind.Conditional
            or OperationKind.WhileLoop
            or OperationKind.ForLoop
            or OperationKind.Try)
        {
            // Record the construct for later analysis.
        }

        base.DefaultVisit(operation);
    }
}
```

Operations delegate child discovery to `OperationFactory`, which walks syntax
children and converts them into operations. As more `BoundNode` shapes are
mapped, the factory recognizes additional control-flow and reference patterns.

## Supported operation kinds

Beyond the foundational expression and statement nodes, the factory surfaces
semantic kinds for:

- Control flow: `Break`, `Continue`, `Goto`, `Labeled`, and `Try` statements as
  well as individual `CatchClause` nodes.
- Exception expressions: `TryExpression` for expression-based exception
  handling forms.
- Assignments: `Assignment` operations appear for both assignment expressions
  and statements, including pattern and discard assignments. Statement syntax
  nodes expose `AssignmentStatementSyntax.IsDiscard` so analyzers can spot `_ =
  expression` patterns.
- Indirection and access: `AddressOf`, `ArrayElement`, and `IndexerElement`
  expressions.
- Contextual references: `NamespaceExpression` and `SelfReference` to model
  namespace qualifiers and `self`/`this` usages.

## Current limitations

The initial implementation focuses on establishing the public surface. While
more control-flow and reference constructs now map to dedicated operation
kinds, several concepts—such as control flow regions and data flow analysis—are
not yet represented. Future work will flesh out specialized operation types,
improve child synthesis, and surface additional semantic annotations.
