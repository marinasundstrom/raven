# Operations API

The operations API exposes a tree of semantic nodes that sit between the binder's
`BoundNode` graph and the syntax tree. It mirrors the `IOperation` surface from
Roslyn so tooling and analyzers can reason about Raven programs using a stable,
syntax-agnostic shape.

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
        if (operation.Kind is OperationKind.IfStatement or OperationKind.Loop)
        {
            // Record the construct for later analysis.
        }

        base.DefaultVisit(operation);
    }
}
```

Operations delegate child discovery to `OperationFactory`, which currently walks
syntax children and converts them into operations. This behavior will evolve as
more operation kinds are implemented.

## Current limitations

The initial implementation focuses on establishing the public surface. Only a
subset of `OperationKind` values produce meaningful trees today, and several
concepts—such as control flow regions and data flow analysis—are not yet
represented. Future work will flesh out specialized operation types, improve
child synthesis, and surface additional semantic annotations.
