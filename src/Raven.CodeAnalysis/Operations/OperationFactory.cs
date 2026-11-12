using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

internal static class OperationFactory
{
    public static OperationBase? Create(SemanticModel semanticModel, SyntaxNode syntax, BoundNode bound)
    {
        if (syntax is ParenthesizedPatternSyntax groupedPattern)
            return Create(semanticModel, groupedPattern.Pattern, bound);
        if (bound is BoundRequiredResultExpression requiredResult)
            return Create(semanticModel, syntax, requiredResult.Operand);
        if (bound is BoundNullableValueExpression nullableValue)
            return Create(semanticModel, syntax, nullableValue.Operand);
        if (bound is BoundSpreadElement { Expression: BoundCollectionComprehensionExpression collectionComprehension } &&
            syntax is CollectionComprehensionElementSyntax)
        {
            return Create(semanticModel, syntax, collectionComprehension);
        }

        var kind = GetOperationKind(bound);
        var type = bound switch
        {
            BoundExpression expr => expr.Type,
            BoundVariableDeclarator declarator => declarator.Type,
            _ => null
        };

        var isImplicit = bound is BoundExpression implicitExpr && implicitExpr.Reason != BoundExpressionReason.None;

        if (syntax is InterpolatedStringExpressionSyntax interpolatedString && bound is BoundExpression boundExpression)
            return new InterpolatedStringOperationImplementation(semanticModel, boundExpression, interpolatedString, isImplicit);

        if (syntax is InterpolatedStringTextSyntax interpolatedText)
            return new InterpolatedStringTextOperationImplementation(semanticModel, interpolatedText, isImplicit);

        if (syntax is InterpolationSyntax interpolation)
            return new InterpolationOperationImplementation(semanticModel, interpolation, isImplicit);

        if (syntax is ArgumentSyntax argumentSyntax && bound is BoundExpression boundArgument)
            return new ArgumentOperationImplementation(semanticModel, boundArgument, argumentSyntax, isImplicit);

        return bound switch
        {
            BoundBlockStatement block => new BlockOperationImplementation(semanticModel, block, kind, syntax, block.LocalsToDispose, type, isImplicit),
            BoundBlockExpression blockExpression => new BlockOperationImplementation(semanticModel, blockExpression, kind, syntax, blockExpression.LocalsToDispose, type, isImplicit),
            BoundExpressionStatement statement when syntax is AssignmentStatementSyntax assignmentStatement =>
                new AssignmentOperationImplementation(semanticModel, statement.Expression, assignmentStatement, statement.Expression.Type, isImplicit),
            BoundExpressionStatement statement => new ExpressionStatementOperationImplementation(semanticModel, statement, syntax, isImplicit),
            BoundFunctionStatement function => new FunctionOperationImplementation(semanticModel, function, syntax, isImplicit),
            BoundLocalDeclarationStatement declaration => new VariableDeclarationOperationImplementation(semanticModel, declaration, syntax, isImplicit),
            BoundVariableDeclarator declarator => new VariableDeclaratorOperationImplementation(semanticModel, declarator, syntax, isImplicit),
            BoundReturnStatement @return => new ReturnOperationImplementation(semanticModel, @return, syntax, isImplicit),
            BoundReturnExpression returnExpression => new ReturnExpressionOperationImplementation(semanticModel, returnExpression, syntax, isImplicit),
            BoundBreakExpression breakExpression => new BreakExpressionOperationImplementation(semanticModel, breakExpression, syntax, isImplicit),
            BoundContinueExpression continueExpression => new ContinueExpressionOperationImplementation(semanticModel, continueExpression, syntax, isImplicit),
            BoundYieldExpression yieldExpression => new YieldExpressionOperationImplementation(semanticModel, yieldExpression, syntax, isImplicit),
            BoundYieldStatement yieldStatement => new YieldOperationImplementation(semanticModel, yieldStatement, syntax, isImplicit),
            BoundThrowStatement @throw => new ThrowOperationImplementation(semanticModel, @throw, syntax, isImplicit),
            BoundThrowExpression throwExpression => new ThrowExpressionOperationImplementation(semanticModel, throwExpression, syntax, isImplicit),
            BoundBreakStatement @break => new BreakOperationImplementation(semanticModel, @break, syntax, isImplicit),
            BoundContinueStatement @continue => new ContinueOperationImplementation(semanticModel, @continue, syntax, isImplicit),
            BoundGotoStatement @goto => new GotoOperationImplementation(semanticModel, @goto, syntax, isImplicit),
            BoundConditionalGotoStatement conditionalGoto => new ConditionalGotoOperationImplementation(semanticModel, conditionalGoto, syntax, isImplicit),
            BoundLabeledStatement labeled => new LabeledOperationImplementation(semanticModel, labeled, syntax, isImplicit),
            BoundLiteralExpression literal => new LiteralOperationImplementation(semanticModel, literal, syntax, isImplicit),
            BoundDefaultValueExpression defaultValue => new DefaultValueOperationImplementation(semanticModel, defaultValue, syntax, isImplicit),
            BoundLocalAccess local => new LocalReferenceOperationImplementation(semanticModel, local, syntax, isImplicit),
            BoundVariableExpression variable => new VariableReferenceOperationImplementation(semanticModel, variable, syntax, isImplicit),
            BoundParameterAccess parameter => new ParameterReferenceOperationImplementation(semanticModel, parameter, syntax, isImplicit),
            BoundFieldAccess field => new FieldReferenceOperationImplementation(semanticModel, field, syntax, isImplicit),
            BoundPropertyAccess property => new PropertyReferenceOperationImplementation(semanticModel, property, syntax, isImplicit),
            BoundMethodGroupExpression methodGroup => new MethodReferenceOperationImplementation(semanticModel, methodGroup, syntax, isImplicit),
            BoundUnaryExpression unary => new UnaryOperationImplementation(semanticModel, unary, syntax, isImplicit),
            BoundBinaryExpression binary => new BinaryOperationImplementation(semanticModel, binary, syntax, isImplicit),
            BoundNullCoalesceExpression coalesce => new CoalesceOperationImplementation(semanticModel, coalesce, syntax, isImplicit),
            BoundNameOfExpression nameOf => new NameOfOperationImplementation(semanticModel, nameOf, syntax, isImplicit),
            BoundParenthesizedExpression parenthesized => new ParenthesizedOperationImplementation(semanticModel, parenthesized, syntax, isImplicit),
            BoundConversionExpression cast => new ConversionOperationImplementation(semanticModel, cast, cast.Conversion, syntax, cast.Type, isImplicit),
            BoundAsExpression asExpression => new ConversionOperationImplementation(semanticModel, asExpression, asExpression.Conversion, syntax, asExpression.Type, isImplicit),
            BoundPropagateExpression propagateExpression => new PropagationOperationImplementation(semanticModel, propagateExpression, syntax, isImplicit),
            BoundDereferenceExpression dereferenceExpression => new DereferenceOperationImplementation(semanticModel, dereferenceExpression, syntax, isImplicit),
            BoundConditionalReceiverExpression receiver => new ConditionalAccessInstanceOperationImplementation(semanticModel, receiver, syntax),
            BoundConditionalAccessExpression conditionalAccess => new ConditionalAccessOperationImplementation(semanticModel, conditionalAccess, syntax, isImplicit),
            BoundCarrierConditionalAccessExpression carrierConditionalAccess => new ConditionalAccessOperationImplementation(semanticModel, carrierConditionalAccess, syntax, isImplicit),
            BoundIfStatement conditionalStatement => new ConditionalOperationImplementation(semanticModel, conditionalStatement, syntax, null, isImplicit),
            BoundIfExpression conditionalExpression => new ConditionalOperationImplementation(semanticModel, conditionalExpression, syntax, conditionalExpression.Type, isImplicit),
            BoundTryExpression tryExpression => new TryExpressionOperationImplementation(semanticModel, tryExpression, syntax, isImplicit),
            BoundAwaitExpression awaitExpression => new AwaitOperationImplementation(semanticModel, awaitExpression, syntax, isImplicit),
            BoundWhileStatement whileStatement => new WhileLoopOperationImplementation(semanticModel, whileStatement, syntax, isImplicit),
            BoundLoopStatement loopStatement => new LoopStatementOperationImplementation(semanticModel, loopStatement, syntax, isImplicit),
            BoundLockStatement lockStatement => new LockOperationImplementation(semanticModel, lockStatement, syntax, isImplicit),
            BoundForStatement forStatement => new ForLoopOperationImplementation(semanticModel, forStatement, syntax, isImplicit),
            BoundInvocationExpression invocation => new InvocationOperationImplementation(semanticModel, invocation, syntax, isImplicit),
            BoundObjectCreationExpression creation => new ObjectCreationOperationImplementation(semanticModel, creation, syntax, isImplicit),
            BoundObjectInitializer initializer => new ObjectInitializerOperationImplementation(semanticModel, initializer, syntax, isImplicit),
            BoundObjectInitializerAssignmentEntry assignmentEntry => new ObjectInitializerAssignmentOperationImplementation(semanticModel, assignmentEntry, syntax, isImplicit),
            BoundObjectInitializerExpressionEntry expressionEntry => new ObjectInitializerExpressionEntryOperationImplementation(semanticModel, expressionEntry, syntax, isImplicit),
            BoundWithExpression withExpression => new WithOperationImplementation(semanticModel, withExpression, syntax, isImplicit),
            BoundAssignmentExpression assignmentExpression => new AssignmentOperationImplementation(semanticModel, assignmentExpression, syntax, assignmentExpression.Type, isImplicit),
            BoundAssignmentStatement assignmentStatement => new AssignmentOperationImplementation(semanticModel, assignmentStatement.Expression, syntax, assignmentStatement.Expression.Type, isImplicit),
            BoundDelegateCreationExpression delegateCreation => new DelegateCreationOperationImplementation(semanticModel, delegateCreation, syntax, isImplicit),
            BoundTupleExpression tuple => new TupleOperationImplementation(semanticModel, tuple, syntax, isImplicit),
            BoundFunctionExpression lambda => new LambdaOperationImplementation(semanticModel, lambda, syntax, isImplicit),
            BoundUnionCaseExpression unionCase => new UnionCaseOperationImplementation(semanticModel, unionCase, syntax, isImplicit),
            BoundAddressOfExpression addressOf => new AddressOfOperationImplementation(semanticModel, addressOf, syntax, isImplicit),
            BoundStackAllocExpression stackAlloc => new StackAllocOperationImplementation(semanticModel, stackAlloc, (StackAllocExpressionSyntax)syntax, isImplicit),
            BoundArrayAccessExpression arrayAccess => new ElementAccessOperationImplementation(semanticModel, arrayAccess, OperationKind.ArrayElement, syntax, isImplicit),
            BoundIndexerAccessExpression indexerAccess => new ElementAccessOperationImplementation(semanticModel, indexerAccess, OperationKind.IndexerElement, syntax, isImplicit),
            BoundIndexExpression indexExpression => new IndexOperationImplementation(semanticModel, indexExpression, syntax, isImplicit),
            BoundRangeExpression rangeExpression => new RangeOperationImplementation(semanticModel, rangeExpression, syntax, isImplicit),
            BoundTypeOfExpression typeOf => new TypeOfOperationImplementation(semanticModel, typeOf, syntax, isImplicit),
            BoundMatchExpression match => new MatchOperationImplementation(semanticModel, match, syntax, isImplicit),
            BoundMatchStatement matchStatement => new MatchOperationImplementation(semanticModel, matchStatement, syntax, isImplicit),
            BoundIsPatternExpression isPattern => new IsPatternOperationImplementation(semanticModel, isPattern, syntax, isImplicit),
            BoundCasePattern casePattern => new CasePatternOperationImplementation(semanticModel, casePattern, syntax, isImplicit),
            BoundDeclarationPattern declarationPattern => new DeclarationPatternOperationImplementation(semanticModel, declarationPattern, syntax, isImplicit),
            BoundConstantPattern constantPattern => new ConstantPatternOperationImplementation(semanticModel, constantPattern, syntax, isImplicit),
            BoundComparisonPattern relationalPattern => new ComparisonPatternOperationImplementation(semanticModel, relationalPattern, syntax, isImplicit),
            BoundPositionalPattern tuplePattern => new PositionalPatternOperationImplementation(semanticModel, tuplePattern, syntax, isImplicit),
            BoundDeconstructPattern deconstructPattern => new RecursivePatternOperationImplementation(semanticModel, deconstructPattern, syntax, isImplicit),
            BoundRangePattern rangePattern => new RangePatternOperationImplementation(semanticModel, rangePattern, syntax, isImplicit),
            BoundPropertyPattern propertyPattern => new PropertyPatternOperationImplementation(semanticModel, propertyPattern, syntax, isImplicit),
            BoundDictionaryPattern dictionaryPattern => new DictionaryPatternOperationImplementation(semanticModel, dictionaryPattern, syntax, isImplicit),
            BoundDiscardPattern discardPattern => new DiscardPatternOperationImplementation(semanticModel, discardPattern, syntax, isImplicit),
            BoundNotPattern notPattern => new NotPatternOperationImplementation(semanticModel, notPattern, syntax, isImplicit),
            BoundAndPattern andPattern => new AndPatternOperationImplementation(semanticModel, andPattern, syntax, isImplicit),
            BoundOrPattern orPattern => new OrPatternOperationImplementation(semanticModel, orPattern, syntax, isImplicit),
            BoundSingleVariableDesignator singleVariableDesignator => new SingleVariableDesignatorOperationImplementation(semanticModel, singleVariableDesignator, syntax, isImplicit),
            BoundDiscardDesignator discardDesignator => new DiscardDesignatorOperationImplementation(semanticModel, discardDesignator, syntax, isImplicit),
            BoundArrayCreationExpression array when syntax is InvocationExpressionSyntax invocation => new ArrayCreationOperationImplementation(semanticModel, array, invocation, isImplicit),
            BoundCollectionExpression collection => new CollectionOperationImplementation(semanticModel, collection, syntax, isImplicit),
            BoundDictionaryExpression dictionary => new DictionaryOperationImplementation(semanticModel, dictionary, syntax, isImplicit),
            BoundCollectionComprehensionExpression comprehension => new CollectionComprehensionOperationImplementation(semanticModel, comprehension, syntax, isImplicit),
            BoundEmptyCollectionExpression emptyCollection => new EmptyCollectionOperationImplementation(semanticModel, emptyCollection, syntax, isImplicit),
            BoundSpreadElement spreadElement => new SpreadElementOperationImplementation(semanticModel, spreadElement, syntax, isImplicit),
            BoundTypeExpression typeExpression => new TypeOperationImplementation(semanticModel, typeExpression, syntax, isImplicit),
            BoundNamespaceExpression namespaceExpression => new NamespaceOperationImplementation(semanticModel, namespaceExpression, syntax, isImplicit),
            BoundSelfExpression self => new SelfOperationImplementation(semanticModel, self, syntax, isImplicit),
            BoundTryStatement tryStatement => new TryOperationImplementation(semanticModel, tryStatement, syntax, isImplicit),
            BoundCatchClause catchClause => new CatchClauseOperationImplementation(semanticModel, catchClause, syntax, isImplicit),
            BoundUnitExpression unit => new UnitOperationImplementation(semanticModel, unit, syntax, isImplicit),
            BoundMemberAccessExpression memberAccess => kind == OperationKind.None
                ? new SimpleOperationImplementation(semanticModel, kind, syntax, type, isImplicit)
                : new MemberReferenceOperationImplementation(semanticModel, memberAccess, kind, syntax, isImplicit),
            BoundPointerMemberAccessExpression pointerMemberAccess => kind == OperationKind.None
                ? new SimpleOperationImplementation(semanticModel, kind, syntax, type, isImplicit)
                : new MemberReferenceOperationImplementation(semanticModel, pointerMemberAccess, kind, syntax, isImplicit),
            BoundErrorExpression error => new InvalidOperationImplementation(semanticModel, error, syntax, isImplicit),
            _ => new SimpleOperationImplementation(semanticModel, kind, syntax, type, isImplicit)
        };
    }

    private static OperationKind GetOperationKind(BoundNode bound)
    {
        return bound switch
        {
            BoundBlockStatement => OperationKind.Block,
            BoundBlockExpression => OperationKind.BlockExpression,
            BoundExpressionStatement => OperationKind.ExpressionStatement,
            BoundLocalDeclarationStatement => OperationKind.LocalDeclaration,
            BoundFunctionStatement => OperationKind.Function,
            BoundVariableDeclarator => OperationKind.VariableDeclarator,
            BoundReturnStatement => OperationKind.Return,
            BoundReturnExpression => OperationKind.ReturnExpression,
            BoundBreakExpression => OperationKind.BreakExpression,
            BoundContinueExpression => OperationKind.ContinueExpression,
            BoundYieldExpression => OperationKind.YieldExpression,
            BoundYieldStatement => OperationKind.Yield,
            BoundThrowStatement => OperationKind.Throw,
            BoundThrowExpression => OperationKind.ThrowExpression,
            BoundBreakStatement => OperationKind.Break,
            BoundContinueStatement => OperationKind.Continue,
            BoundGotoStatement => OperationKind.Goto,
            BoundConditionalGotoStatement => OperationKind.ConditionalGoto,
            BoundLabeledStatement => OperationKind.Labeled,
            BoundLiteralExpression => OperationKind.Literal,
            BoundDefaultValueExpression => OperationKind.DefaultValue,
            BoundLocalAccess => OperationKind.LocalReference,
            BoundVariableExpression => OperationKind.VariableReference,
            BoundParameterAccess => OperationKind.ParameterReference,
            BoundFieldAccess => OperationKind.FieldReference,
            BoundPropertyAccess => OperationKind.PropertyReference,
            BoundMethodGroupExpression => OperationKind.MethodReference,
            BoundMemberAccessExpression member => member.Member switch
            {
                FieldSymbol => OperationKind.FieldReference,
                PropertySymbol => OperationKind.PropertyReference,
                MethodSymbol => OperationKind.MethodReference,
                _ => OperationKind.None
            },
            BoundPointerMemberAccessExpression pointerMember => pointerMember.Member switch
            {
                FieldSymbol => OperationKind.FieldReference,
                PropertySymbol => OperationKind.PropertyReference,
                MethodSymbol => OperationKind.MethodReference,
                _ => OperationKind.None
            },
            BoundUnaryExpression => OperationKind.Unary,
            BoundBinaryExpression => OperationKind.Binary,
            BoundNullCoalesceExpression => OperationKind.Coalesce,
            BoundNameOfExpression => OperationKind.NameOf,
            BoundParenthesizedExpression => OperationKind.Parenthesized,
            BoundConversionExpression => OperationKind.Conversion,
            BoundAsExpression => OperationKind.Conversion,
            BoundPropagateExpression => OperationKind.Propagate,
            BoundDereferenceExpression => OperationKind.Dereference,
            BoundConditionalReceiverExpression => OperationKind.ConditionalAccessInstance,
            BoundConditionalAccessExpression => OperationKind.ConditionalAccess,
            BoundCarrierConditionalAccessExpression => OperationKind.ConditionalAccess,
            BoundTryExpression => OperationKind.TryExpression,
            BoundAwaitExpression => OperationKind.Await,
            BoundIfStatement => OperationKind.Conditional,
            BoundIfExpression => OperationKind.Conditional,
            BoundWhileStatement => OperationKind.WhileLoop,
            BoundLoopStatement => OperationKind.Loop,
            BoundLockStatement => OperationKind.Lock,
            BoundForStatement => OperationKind.ForLoop,
            BoundInvocationExpression => OperationKind.Invocation,
            BoundObjectCreationExpression => OperationKind.ObjectCreation,
            BoundObjectInitializer => OperationKind.ObjectInitializer,
            BoundObjectInitializerAssignmentEntry => OperationKind.ObjectInitializerAssignment,
            BoundObjectInitializerExpressionEntry => OperationKind.ObjectInitializerExpressionEntry,
            BoundWithExpression => OperationKind.With,
            BoundAssignmentExpression => OperationKind.Assignment,
            BoundAssignmentStatement => OperationKind.Assignment,
            BoundDelegateCreationExpression => OperationKind.DelegateCreation,
            BoundTupleExpression => OperationKind.Tuple,
            BoundAddressOfExpression => OperationKind.AddressOf,
            BoundStackAllocExpression => OperationKind.StackAlloc,
            BoundArrayAccessExpression => OperationKind.ArrayElement,
            BoundIndexerAccessExpression => OperationKind.IndexerElement,
            BoundIndexExpression => OperationKind.Index,
            BoundRangeExpression => OperationKind.Range,
            BoundTypeOfExpression => OperationKind.TypeOf,
            BoundFunctionExpression => OperationKind.Lambda,
            BoundUnionCaseExpression => OperationKind.UnionCase,
            BoundMatchExpression => OperationKind.Switch,
            BoundMatchStatement => OperationKind.Switch,
            BoundIsPatternExpression => OperationKind.IsPattern,
            BoundCasePattern => OperationKind.CasePattern,
            BoundDeclarationPattern => OperationKind.DeclarationPattern,
            BoundConstantPattern => OperationKind.ConstantPattern,
            BoundComparisonPattern => OperationKind.ComparisonPattern,
            BoundPositionalPattern => OperationKind.PositionalPattern,
            BoundDeconstructPattern => OperationKind.RecursivePattern,
            BoundRangePattern => OperationKind.RangePattern,
            BoundPropertyPattern => OperationKind.PropertyPattern,
            BoundDictionaryPattern => OperationKind.DictionaryPattern,
            BoundDiscardPattern => OperationKind.DiscardPattern,
            BoundNotPattern => OperationKind.NotPattern,
            BoundAndPattern => OperationKind.AndPattern,
            BoundOrPattern => OperationKind.OrPattern,
            BoundSingleVariableDesignator => OperationKind.SingleVariableDesignator,
            BoundDiscardDesignator => OperationKind.DiscardDesignator,
            BoundArrayCreationExpression => OperationKind.ArrayCreation,
            BoundCollectionExpression => OperationKind.Collection,
            BoundDictionaryExpression => OperationKind.Dictionary,
            BoundCollectionComprehensionExpression => OperationKind.CollectionComprehension,
            BoundEmptyCollectionExpression => OperationKind.EmptyCollection,
            BoundSpreadElement => OperationKind.SpreadElement,
            BoundTypeExpression => OperationKind.TypeExpression,
            BoundNamespaceExpression => OperationKind.NamespaceExpression,
            BoundSelfExpression => OperationKind.SelfReference,
            BoundTryStatement => OperationKind.Try,
            BoundCatchClause => OperationKind.CatchClause,
            BoundUnitExpression => OperationKind.Unit,
            BoundErrorExpression => OperationKind.Invalid,
            _ => OperationKind.None
        };
    }
}
