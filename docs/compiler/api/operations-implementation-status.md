# Operations API implementation status

This document records which bound nodes currently map to specialized operations, which ones only map to generic `SimpleOperation`, and which ones lack any operations kind mapping today. It is intended as a checklist for expanding the Operations API.

Sources of truth:
- `OperationKind` (enumeration of operation kinds)
- `OperationFactory` (mapping from bound nodes to operation kinds and operation implementations)
- Bound node definitions in `src/Raven.CodeAnalysis/BoundTree`

## Implemented operation kinds (specialized operation nodes)

| Operation kind | Bound node(s) | Specialized operation node |
| --- | --- | --- |
| `Block` | `BoundBlockStatement` | `BlockOperation` |
| `BlockExpression` | `BoundBlockExpression` | `BlockOperation` |
| `ExpressionStatement` | `BoundExpressionStatement` | `ExpressionStatementOperation` |
| `LocalDeclaration` | `BoundLocalDeclarationStatement` | `VariableDeclarationOperation` |
| `Function` | `BoundFunctionStatement` | `FunctionOperation` |
| `VariableDeclarator` | `BoundVariableDeclarator` | `VariableDeclaratorOperation` |
| `Return` | `BoundReturnStatement` | `ReturnOperation` |
| `YieldReturn` | `BoundYieldReturnStatement` | `YieldReturnOperation` |
| `YieldBreak` | `BoundYieldBreakStatement` | `YieldBreakOperation` |
| `Throw` | `BoundThrowStatement` | `ThrowOperation` |
| `Break` | `BoundBreakStatement` | `BreakOperation` |
| `Continue` | `BoundContinueStatement` | `ContinueOperation` |
| `Goto` | `BoundGotoStatement` | `GotoOperation` |
| `ConditionalGoto` | `BoundConditionalGotoStatement` | `ConditionalGotoOperation` |
| `Labeled` | `BoundLabeledStatement` | `LabeledOperation` |
| `Literal` | `BoundLiteralExpression` | `LiteralOperation` |
| `DefaultValue` | `BoundDefaultValueExpression` | `DefaultValueOperation` |
| `LocalReference` | `BoundLocalAccess` | `LocalReferenceOperation` |
| `VariableReference` | `BoundVariableExpression` | `VariableReferenceOperation` |
| `ParameterReference` | `BoundParameterAccess` | `ParameterReferenceOperation` |
| `FieldReference` | `BoundFieldAccess` | `FieldReferenceOperation` |
| `PropertyReference` | `BoundPropertyAccess` | `PropertyReferenceOperation` |
| `MethodReference` | `BoundMethodGroupExpression` | `MethodReferenceOperation` |
| `Unary` | `BoundUnaryExpression` | `UnaryOperation` |
| `Binary` | `BoundBinaryExpression` | `BinaryOperation` |
| `Parenthesized` | `BoundParenthesizedExpression` | `ParenthesizedOperation` |
| `Conversion` | `BoundConversionExpression`, `BoundAsExpression` | `ConversionOperation` |
| `ConditionalAccess` | `BoundConditionalAccessExpression` | `ConditionalAccessOperation` |
| `Conditional` | `BoundIfStatement`, `BoundIfExpression` | `ConditionalOperation` |
| `TryExpression` | `BoundTryExpression` | `TryExpressionOperation` |
| `Await` | `BoundAwaitExpression` | `AwaitOperation` |
| `WhileLoop` | `BoundWhileStatement` | `WhileOperation` |
| `ForLoop` | `BoundForStatement` | `ForOperation` |
| `Invocation` | `BoundInvocationExpression` | `InvocationOperation` |
| `ObjectCreation` | `BoundObjectCreationExpression` | `ObjectCreationOperation` |
| `Assignment` | `BoundAssignmentExpression`, `BoundAssignmentStatement` | `AssignmentOperation` |
| `DelegateCreation` | `BoundDelegateCreationExpression` | `DelegateCreationOperation` |
| `Tuple` | `BoundTupleExpression` | `TupleOperation` |
| `Lambda` | `BoundLambdaExpression` | `LambdaOperation` |
| `AddressOf` | `BoundAddressOfExpression` | `AddressOfOperation` |
| `ArrayElement` | `BoundArrayAccessExpression` | `ElementAccessOperation` |
| `IndexerElement` | `BoundIndexerAccessExpression` | `ElementAccessOperation` |
| `Index` | `BoundIndexExpression` | `IndexOperation` |
| `Range` | `BoundRangeExpression` | `RangeOperation` |
| `TypeOf` | `BoundTypeOfExpression` | `TypeOfOperation` |
| `Switch` | `BoundMatchExpression` | `SwitchOperation` |
| `IsPattern` | `BoundIsPatternExpression` | `IsPatternOperation` |
| `CasePattern` | `BoundCasePattern` | `CasePatternOperation` |
| `DeclarationPattern` | `BoundDeclarationPattern` | `DeclarationPatternOperation` |
| `ConstantPattern` | `BoundConstantPattern` | `ConstantPatternOperation` |
| `PositionalPattern` | `BoundPositionalPattern` | `PositionalPatternOperation` |
| `DiscardPattern` | `BoundDiscardPattern` | `DiscardPatternOperation` |
| `NotPattern` | `BoundNotPattern` | `NotPatternOperation` |
| `AndPattern` | `BoundAndPattern` | `AndPatternOperation` |
| `OrPattern` | `BoundOrPattern` | `OrPatternOperation` |
| `SingleVariableDesignator` | `BoundSingleVariableDesignator` | `SingleVariableDesignatorOperation` |
| `DiscardDesignator` | `BoundDiscardDesignator` | `DiscardDesignatorOperation` |
| `Collection` | `BoundCollectionExpression` | `CollectionOperation` |
| `EmptyCollection` | `BoundEmptyCollectionExpression` | `EmptyCollectionOperation` |
| `SpreadElement` | `BoundSpreadElement` | `SpreadElementOperation` |
| `TypeExpression` | `BoundTypeExpression` | `TypeOperation` |
| `NamespaceExpression` | `BoundNamespaceExpression` | `NamespaceOperation` |
| `SelfReference` | `BoundSelfExpression` | `SelfOperation` |
| `Try` | `BoundTryStatement` | `TryOperation` |
| `CatchClause` | `BoundCatchClause` | `CatchClauseOperation` |
| `Unit` | `BoundUnitExpression` | `UnitOperation` |
| `Invalid` | `BoundErrorExpression` | `InvalidOperation` |
| `FieldReference`/`PropertyReference`/`MethodReference` | `BoundMemberAccessExpression` | `MemberReferenceOperation` |

## Missing operation kinds or specialized nodes (no mapping yet)

These bound nodes are not mapped to `OperationKind`/specialized operations in `OperationFactory` and currently default to `SimpleOperation` (or have no kind at all). These are the primary candidates for new operation kinds or new specialized operation nodes.

| Bound node(s) | Suggested operation shape | Notes |
| --- | --- | --- |
None currently tracked.

## Notes

- `OperationKind.None` is used as a fallback when no semantic shape is available.
- The status above is based on `OperationFactory` and `OperationKind` as of this snapshot.
