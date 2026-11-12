# Operations API

The operations API exposes a tree of semantic nodes between the binder's
`BoundNode` graph and the syntax tree. Its contracts follow Roslyn's `IOperation`
model, with Raven's unprefixed interface names: `Operation`, `ArgumentOperation`,
`BinaryOperation`, and so on. Concrete classes use an `Implementation` suffix
(for example, `ArgumentOperationImplementation`); abstract bases use `Base`
(`OperationBase`, `SymbolReferenceOperationBase<TSymbol>`, `LoopOperationBase`).
Unmapped semantic shapes fall back to `SimpleOperationImplementation` with
`OperationKind.None`.

This is a breaking API rename: replace `IOperation` with `Operation` and each
`I…Operation` interface with its unprefixed name. Consumers should use the public
interfaces; concrete implementations remain internal. Code deriving from the
public abstract base must now derive from `OperationBase`. Operation properties,
including analyzer context `Operation`, keep their names. Behavior and target
selection are unchanged.

---

## Concepts

- **Operation** – An immutable semantic node backed by an `OperationBase`
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

### Await, try, and propagation

Standalone postfix `?` wraps the complete `await` or `try` expression in the
syntax and operation trees. `await Foo()?` exposes an `PropagationOperation`
whose operand is an `AwaitOperation`; `try Foo()?` exposes an
`PropagationOperation` whose operand is an `TryExpressionOperation`.
The prefix nodes retain their own types: an awaited carrier for `await`, and
`Result<T, Exception>` for `try`. The propagation node has the success payload
type. `SemanticModel.GetTypeInfo` and `GetOperation` expose these distinctions
without requiring diagnostics to be requested first. Explicit parentheses can
instead keep propagation inside the operand, as in `await (Foo()?)`.

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

```raven
import System.Linq.*
import Raven.CodeAnalysis.Syntax.*

let model = compilation.GetSemanticModel(tree)
let ifStatement = tree.GetRoot().DescendantNodes()
    .OfType<IfStatementSyntax>()
    .First()

let operation = model.GetOperation(ifStatement)
```

`GetOperation` returns `null` when no operation exists for the supplied node.
Bound nodes are pulled from the binder cache, so the call does not trigger a new
binding pass.

### Caching

`SemanticModel` caches every realized operation. Repeated calls for the same
syntax node return the exact instance, ensuring parent/child links remain stable
and visitors can rely on reference equality.

## Navigating the operation tree

Operations expose their children through the `ChildOperations` property and can be
traversed with the provided visitor base types:

```raven
import Raven.CodeAnalysis.Operations.*

class ControlFlowCollector : OperationVisitor {
    override func DefaultVisit(operation: Operation) {
        if operation.Kind == OperationKind.Conditional ||
            operation.Kind == OperationKind.WhileLoop ||
            operation.Kind == OperationKind.ForLoop ||
            operation.Kind == OperationKind.Try {
            // Record the construct for later analysis.
        }

        base.DefaultVisit(operation)
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
- Local declarations surface as `VariableDeclarationOperation` nodes that
  expose each `VariableDeclaratorOperation`, including explicit initializers
  where present, so analyzers can understand declared locals without
  inspecting syntax.

## Current limitations

The initial implementation focuses on establishing the public surface. While
more control-flow and reference constructs now map to dedicated operation
kinds, several concepts—such as control flow regions and data flow analysis—are
not yet represented. Future work will flesh out specialized operation types,
improve child synthesis, and surface additional semantic annotations.

## Resolved binary operators and invocation receivers (2026-09-30)

`BinaryOperation` now exposes `OperatorKind`, `IsLifted`, `IsChecked`, and
`OperatorMethod`. `Raven.CodeAnalysis.Operations.BinaryOperatorKind` describes the
resolved base operation: Add, Subtract, Multiply, Divide, Remainder, comparisons,
bitwise/logical operations, shifts, or Concatenate. None denotes no ordinary binary
operator. It is not a flags enum; nullable lifting and checked arithmetic are separate
facts copied from binding. `OperatorMethod` is the selected method where that bound
binary representation carries one, otherwise null. These members expose existing
semantic decisions rather than reinterpreting source tokens or changing evaluation.

`CoalesceOperation` retains its existing inheritance from `BinaryOperation`, but
returns None, false/false and null for these ordinary binary facts. Consumers must use
its coalescing operation kind. User-defined operators may already bind as
`InvocationOperation`; consume their TargetMethod instead of requiring every operator
syntax to produce a BinaryOperation.

`InvocationOperation.Instance` now describes the bound runtime receiver. Static
calls return null. Instance calls return a cached receiver operation that participates
in child traversal and parent links. Previously it projected the invocation's callee
syntax and could return a method group, including for static calls. This correction
is useful for analyzers and alternate backends and does not alter binding or emitted IL.
Arguments retain their existing surface; this change does not claim complete normalized
argument/extension-receiver support.

The reference behavior is Roslyn's
[IInvocationOperation](https://learn.microsoft.com/en-us/dotnet/api/microsoft.codeanalysis.operations.iinvocationoperation?view=roslyn-dotnet-5.0.0)
(receiver versus static calls; documentation checked 2026-09-30). Raven keeps its own
operation shapes and does not claim exact Roslyn source compatibility. External
implementations of BinaryOperation must implement the new members. Ordinary consumers
can use the semantic properties without depending on internal bound-node types.

### Conditional access chains

`ConditionalAccessOperation.WhenNotNull` represents the complete access and
invocation chain before a parenthesis or non-access postfix boundary. Within a
nullable conditional branch, `ConditionalAccessInstanceOperation` represents
the already-evaluated, non-null receiver. It is implicit, has the unwrapped
receiver type, and has no children. It does not represent another evaluation of
`ConditionalAccessOperation.Receiver`. Nested conditional accesses have their
own instance and short-circuit boundary.

`MemberReferenceOperation.Instance` and its child operations retain the receiver
of an instance member, including the conditional instance and intervening calls
in an access chain. Static members have no instance. This lets operation walkers
see the complete branch without re-binding source syntax as an independent
expression.
