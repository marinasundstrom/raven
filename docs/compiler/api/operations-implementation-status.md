# Operations API implementation status

This document records which bound nodes currently map to specialized operations, which ones only map to generic `SimpleOperationImplementation`, and which ones lack any operations kind mapping today. It is intended as a checklist for expanding the Operations API.

Sources of truth:
- `OperationKind` (enumeration of operation kinds)
- `OperationFactory` (mapping from bound nodes to operation kinds and operation implementations)
- Bound node definitions in `src/Raven.CodeAnalysis/BoundTree`

## Implemented operation kinds (specialized operation nodes)

| OperationBase kind | Bound node(s) | Specialized operation node |
| --- | --- | --- |
| `Block` | `BoundBlockStatement` | `BlockOperationImplementation` |
| `BlockExpression` | `BoundBlockExpression` | `BlockOperationImplementation` |
| `ExpressionStatement` | `BoundExpressionStatement` | `ExpressionStatementOperationImplementation` |
| `LocalDeclaration` | `BoundLocalDeclarationStatement` | `VariableDeclarationOperationImplementation` |
| `Function` | `BoundFunctionStatement` | `FunctionOperationImplementation` |
| `VariableDeclarator` | `BoundVariableDeclarator` | `VariableDeclaratorOperationImplementation` |
| `Return` | `BoundReturnStatement` | `ReturnOperationImplementation` |
| `ReturnExpression` | `BoundReturnExpression` | `ReturnExpressionOperationImplementation` |
| `BreakExpression` | `BoundBreakExpression` | `BreakExpressionOperationImplementation` |
| `ContinueExpression` | `BoundContinueExpression` | `ContinueExpressionOperationImplementation` |
| `YieldExpression` | `BoundYieldExpression` | `YieldExpressionOperationImplementation` |
| `Yield` | `BoundYieldStatement` | `YieldOperationImplementation` |
| `Throw` | `BoundThrowStatement` | `ThrowOperationImplementation` |
| `ThrowExpression` | `BoundThrowExpression` | `ThrowExpressionOperationImplementation` |
| `Break` | `BoundBreakStatement` | `BreakOperationImplementation` |
| `Continue` | `BoundContinueStatement` | `ContinueOperationImplementation` |
| `Goto` | `BoundGotoStatement` | `GotoOperationImplementation` |
| `ConditionalGoto` | `BoundConditionalGotoStatement` | `ConditionalGotoOperationImplementation` |
| `Labeled` | `BoundLabeledStatement` | `LabeledOperationImplementation` |
| `Literal` | `BoundLiteralExpression` | `LiteralOperationImplementation` |
| `DefaultValue` | `BoundDefaultValueExpression` | `DefaultValueOperationImplementation` |
| `LocalReference` | `BoundLocalAccess` | `LocalReferenceOperationImplementation` |
| `VariableReference` | `BoundVariableExpression` | `VariableReferenceOperationImplementation` |
| `ParameterReference` | `BoundParameterAccess` | `ParameterReferenceOperationImplementation` |
| `FieldReference` | `BoundFieldAccess` | `FieldReferenceOperationImplementation` |
| `PropertyReference` | `BoundPropertyAccess` | `PropertyReferenceOperationImplementation` |
| `MethodReference` | `BoundMethodGroupExpression` | `MethodReferenceOperationImplementation` |
| `Unary` | `BoundUnaryExpression` | `UnaryOperationImplementation` |
| `Binary` | `BoundBinaryExpression` | `BinaryOperationImplementation` |
| `Coalesce` | `BoundNullCoalesceExpression` | `CoalesceOperationImplementation` |
| `NameOf` | `BoundNameOfExpression` | `NameOfOperationImplementation` |
| `Parenthesized` | `BoundParenthesizedExpression` | `ParenthesizedOperationImplementation` |
| `Conversion` | `BoundConversionExpression`, `BoundAsExpression` | `ConversionOperationImplementation` |
| `Propagate` | `BoundPropagateExpression` | `PropagationOperationImplementation` |
| `Dereference` | `BoundDereferenceExpression` | `DereferenceOperationImplementation` |
| `ConditionalAccess` | `BoundConditionalAccessExpression`, `BoundCarrierConditionalAccessExpression` | `ConditionalAccessOperationImplementation` |
| `Conditional` | `BoundIfStatement`, `BoundIfExpression` | `ConditionalOperationImplementation` |
| `TryExpression` | `BoundTryExpression` | `TryExpressionOperationImplementation` |
| `Await` | `BoundAwaitExpression` | `AwaitOperationImplementation` |
| `WhileLoop` | `BoundWhileStatement` | `WhileLoopOperationImplementation` |
| `ForLoop` | `BoundForStatement` | `ForLoopOperationImplementation` |
| `Invocation` | `BoundInvocationExpression` | `InvocationOperationImplementation` |
| `ObjectCreation` | `BoundObjectCreationExpression` | `ObjectCreationOperationImplementation` |
| `ObjectInitializer` | `BoundObjectInitializer` | `ObjectInitializerOperationImplementation` |
| `ObjectInitializerAssignment` | `BoundObjectInitializerAssignmentEntry` | `ObjectInitializerAssignmentOperationImplementation` |
| `ObjectInitializerExpressionEntry` | `BoundObjectInitializerExpressionEntry` | `ObjectInitializerExpressionEntryOperationImplementation` |
| `With` | `BoundWithExpression` | `WithOperationImplementation` |
| `Assignment` | `BoundAssignmentExpression`, `BoundAssignmentStatement` | `AssignmentOperationImplementation` |
| `DelegateCreation` | `BoundDelegateCreationExpression` | `DelegateCreationOperationImplementation` |
| `Tuple` | `BoundTupleExpression` | `TupleOperationImplementation` |
| `Lambda` | `BoundFunctionExpression` | `LambdaOperationImplementation` |
| `UnionCase` | `BoundUnionCaseExpression` | `UnionCaseOperationImplementation` |
| `AddressOf` | `BoundAddressOfExpression` | `AddressOfOperationImplementation` |
| `ArrayElement` | `BoundArrayAccessExpression` | `ElementAccessOperationImplementation` |
| `IndexerElement` | `BoundIndexerAccessExpression` | `ElementAccessOperationImplementation` |
| `Index` | `BoundIndexExpression` | `IndexOperationImplementation` |
| `Range` | `BoundRangeExpression` | `RangeOperationImplementation` |
| `TypeOf` | `BoundTypeOfExpression` | `TypeOfOperationImplementation` |
| `Switch` | `BoundMatchExpression`, `BoundMatchStatement` | `MatchOperationImplementation` |
| `IsPattern` | `BoundIsPatternExpression` | `IsPatternOperationImplementation` |
| `CasePattern` | `BoundCasePattern` | `CasePatternOperationImplementation` |
| `DeclarationPattern` | `BoundDeclarationPattern` | `DeclarationPatternOperationImplementation` |
| `ConstantPattern` | `BoundConstantPattern` | `ConstantPatternOperationImplementation` |
| `ComparisonPattern` | `BoundComparisonPattern` | `ComparisonPatternOperationImplementation` |
| `PositionalPattern` | `BoundPositionalPattern` | `PositionalPatternOperationImplementation` |
| `RecursivePattern` | `BoundDeconstructPattern` | `RecursivePatternOperationImplementation` |
| `RangePattern` | `BoundRangePattern` | `RangePatternOperationImplementation` |
| `PropertyPattern` | `BoundPropertyPattern` | `PropertyPatternOperationImplementation` |
| `DiscardPattern` | `BoundDiscardPattern` | `DiscardPatternOperationImplementation` |
| `NotPattern` | `BoundNotPattern` | `NotPatternOperationImplementation` |
| `AndPattern` | `BoundAndPattern` | `AndPatternOperationImplementation` |
| `OrPattern` | `BoundOrPattern` | `OrPatternOperationImplementation` |
| `SingleVariableDesignator` | `BoundSingleVariableDesignator` | `SingleVariableDesignatorOperationImplementation` |
| `DiscardDesignator` | `BoundDiscardDesignator` | `DiscardDesignatorOperationImplementation` |
| `Collection` | `BoundCollectionExpression` | `CollectionOperationImplementation` |
| `CollectionComprehension` | `BoundCollectionComprehensionExpression` | `CollectionComprehensionOperationImplementation` |
| `EmptyCollection` | `BoundEmptyCollectionExpression` | `EmptyCollectionOperationImplementation` |
| `SpreadElement` | `BoundSpreadElement` | `SpreadElementOperationImplementation` |
| `TypeExpression` | `BoundTypeExpression` | `TypeOperationImplementation` |
| `NamespaceExpression` | `BoundNamespaceExpression` | `NamespaceOperationImplementation` |
| `SelfReference` | `BoundSelfExpression` | `SelfOperationImplementation` |
| `Try` | `BoundTryStatement` | `TryOperationImplementation` |
| `CatchClause` | `BoundCatchClause` | `CatchClauseOperationImplementation` |
| `Unit` | `BoundUnitExpression` | `UnitOperationImplementation` |
| `Invalid` | `BoundErrorExpression` | `InvalidOperationImplementation` |
| `FieldReference`/`PropertyReference`/`MethodReference` | `BoundMemberAccessExpression` | `MemberReferenceOperationImplementation` |
| `FieldReference`/`PropertyReference`/`MethodReference` | `BoundPointerMemberAccessExpression` | `MemberReferenceOperationImplementation` |

## Internal wrappers and fallback operations

Internal lowering wrappers do not require dedicated public operation kinds.
The wrappers below are intentionally unwrapped by `OperationFactory`; they are
not missing API mappings. Other fallback operations should be assessed against
a concrete consumer scenario before extending the public API.

Intentional internal wrappers (not exposed as dedicated operations):

- `BoundRequiredResultExpression` is a codegen-only wrapper and is unwrapped to its operand in the Operations API.
- `BoundNullableValueExpression` is an internal lowering/codegen helper and is unwrapped to its operand in the Operations API.

## Notes

- `OperationKind.None` is used as a fallback when no semantic shape is available.
- The status above is based on `OperationFactory` and `OperationKind` as of this snapshot.

## Recent API surface additions

The following operation interfaces were expanded with typed accessors in the
latest slices:

- `ConditionalOperation`: `Condition`, `WhenTrue`, `WhenFalse`
- `ConditionalAccessOperation`: `Receiver`, `WhenNotNull`
- `LocalReferenceOperation`: `Local`
- `VariableReferenceOperation`: `Variable`
- `ParameterReferenceOperation`: `Parameter`
- `FieldReferenceOperation`: `Field`
- `PropertyReferenceOperation`: `Property`
- `MethodReferenceOperation`: `Method`
- `SwitchOperation`: `Value`, `Patterns`, `Guards`, `ArmValues`
- `CasePatternOperation`: `Arguments`
- `ConstantPatternOperation`: `Value`
- `PositionalPatternOperation`: `Subpatterns`
- `ReceiverPatternOperation`: `ReceiverType`, `NarrowedType`
- `RecursivePatternOperation`: `DeconstructMethod`, `Arguments`
- `RangePatternOperation`: `LowerBound`, `UpperBound`, `IsUpperExclusive`
- `ComparisonPatternOperation`: `OperatorKind`, `Value`
- `PropertyPatternOperation`: `Designator`, `Members`, `Subpatterns`
- `PropagationOperation`: `Operand`, `OkType`, `ErrorType`, `EnclosingResultType`, `EnclosingErrorConstructor`
- `DereferenceOperation`: `Operand`
- `CollectionComprehensionOperation`: `Source`, `Condition`, `Selector`, `IterationLocal`, `ElementType`
- `ObjectCreationOperation`: `Initializer`
- `ObjectInitializerOperation`: `Entries`
- `ObjectInitializerAssignmentOperation`: `Member`, `OperatorTokenKind`, `Value`
- `ObjectInitializerExpressionEntryOperation`: `Expression`
- `NotPatternOperation`: `Pattern`
- `AndPatternOperation`: `Left`, `Right`
- `OrPatternOperation`: `Left`, `Right`
- `TryOperation`: `Body`, `Catches`, `Finally`
- `CatchClauseOperation`: `Body`
- `TryExpressionOperation`: `Operation`, `ExceptionType`, `OkConstructor`, `ErrorConstructor`
- `ElementAccessOperation`: `Instance`, `Arguments`, `Indexer`
- `TupleOperation`: `Elements`
- `LambdaOperation`: `Parameters`, `ReturnType`, `Body`, `CandidateDelegates`, `CapturedVariables`

### Binary and invocation facts (2026-09-30)

BinaryOperation now exposes its resolved public operator kind, IsLifted, IsChecked and
OperatorMethod. CoalesceOperation reports None for the ordinary binary-kind contract.
InvocationOperation uses its bound receiver instead of the callee method-group syntax,
and returns null for static calls. This is a semantic API correction, not a new lowering
or runtime implementation. User operators already represented as invocations retain
that shape. See [operations API](operations.md#resolved-binary-operators-and-invocation-receivers-2026-09-30).
