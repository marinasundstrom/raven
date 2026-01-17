using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Operations;

internal static class OperationFactory
{
    public static Operation? Create(SemanticModel semanticModel, SyntaxNode syntax, BoundNode bound)
    {
        var kind = GetOperationKind(bound);
        var type = bound switch
        {
            BoundExpression expr => expr.Type,
            BoundVariableDeclarator declarator => declarator.Type,
            _ => null
        };

        var isImplicit = bound is BoundExpression implicitExpr && implicitExpr.Reason != BoundExpressionReason.None;

        if (syntax is InterpolatedStringExpressionSyntax interpolatedString && bound is BoundExpression boundExpression)
            return new InterpolatedStringOperation(semanticModel, boundExpression, interpolatedString, isImplicit);

        if (syntax is InterpolatedStringTextSyntax interpolatedText)
            return new InterpolatedStringTextOperation(semanticModel, interpolatedText, isImplicit);

        if (syntax is InterpolationSyntax interpolation)
            return new InterpolationOperation(semanticModel, interpolation, isImplicit);

        if (syntax is ArgumentSyntax argumentSyntax && bound is BoundExpression boundArgument)
            return new ArgumentOperation(semanticModel, boundArgument, argumentSyntax, isImplicit);

        return bound switch
        {
            BoundBlockStatement block => new BlockOperation(semanticModel, block, kind, syntax, block.LocalsToDispose, type, isImplicit),
            BoundBlockExpression blockExpression => new BlockOperation(semanticModel, blockExpression, kind, syntax, blockExpression.LocalsToDispose, type, isImplicit),
            BoundExpressionStatement statement => new ExpressionStatementOperation(semanticModel, statement, syntax, isImplicit),
            BoundFunctionStatement function => new FunctionOperation(semanticModel, function, syntax, isImplicit),
            BoundLocalDeclarationStatement declaration => new VariableDeclarationOperation(semanticModel, declaration, syntax, isImplicit),
            BoundVariableDeclarator declarator => new VariableDeclaratorOperation(semanticModel, declarator, syntax, isImplicit),
            BoundReturnStatement @return => new ReturnOperation(semanticModel, @return, syntax, isImplicit),
            BoundYieldReturnStatement yieldReturn => new YieldReturnOperation(semanticModel, yieldReturn, syntax, isImplicit),
            BoundYieldBreakStatement yieldBreak => new YieldBreakOperation(semanticModel, yieldBreak, syntax, isImplicit),
            BoundThrowStatement @throw => new ThrowOperation(semanticModel, @throw, syntax, isImplicit),
            BoundBreakStatement @break => new BreakOperation(semanticModel, @break, syntax, isImplicit),
            BoundContinueStatement @continue => new ContinueOperation(semanticModel, @continue, syntax, isImplicit),
            BoundGotoStatement @goto => new GotoOperation(semanticModel, @goto, syntax, isImplicit),
            BoundConditionalGotoStatement conditionalGoto => new ConditionalGotoOperation(semanticModel, conditionalGoto, syntax, isImplicit),
            BoundLabeledStatement labeled => new LabeledOperation(semanticModel, labeled, syntax, isImplicit),
            BoundLiteralExpression literal => new LiteralOperation(semanticModel, literal, syntax, isImplicit),
            BoundDefaultValueExpression defaultValue => new DefaultValueOperation(semanticModel, defaultValue, syntax, isImplicit),
            BoundLocalAccess local => new LocalReferenceOperation(semanticModel, local, syntax, isImplicit),
            BoundVariableExpression variable => new VariableReferenceOperation(semanticModel, variable, syntax, isImplicit),
            BoundParameterAccess parameter => new ParameterReferenceOperation(semanticModel, parameter, syntax, isImplicit),
            BoundFieldAccess field => new FieldReferenceOperation(semanticModel, field, syntax, isImplicit),
            BoundPropertyAccess property => new PropertyReferenceOperation(semanticModel, property, syntax, isImplicit),
            BoundMethodGroupExpression methodGroup => new MethodReferenceOperation(semanticModel, methodGroup, syntax, isImplicit),
            BoundUnaryExpression unary => new UnaryOperation(semanticModel, unary, syntax, isImplicit),
            BoundBinaryExpression binary => new BinaryOperation(semanticModel, binary, syntax, isImplicit),
            BoundParenthesizedExpression parenthesized => new ParenthesizedOperation(semanticModel, parenthesized, syntax, isImplicit),
            BoundCastExpression cast => new ConversionOperation(semanticModel, cast, cast.Conversion, syntax, cast.Type, isImplicit),
            BoundAsExpression asExpression => new ConversionOperation(semanticModel, asExpression, asExpression.Conversion, syntax, asExpression.Type, isImplicit),
            BoundConditionalAccessExpression conditionalAccess => new ConditionalAccessOperation(semanticModel, conditionalAccess, syntax, isImplicit),
            BoundIfStatement conditionalStatement => new ConditionalOperation(semanticModel, conditionalStatement, syntax, null, isImplicit),
            BoundIfExpression conditionalExpression => new ConditionalOperation(semanticModel, conditionalExpression, syntax, conditionalExpression.Type, isImplicit),
            BoundTryExpression tryExpression => new TryExpressionOperation(semanticModel, tryExpression, syntax, isImplicit),
            BoundAwaitExpression awaitExpression => new AwaitOperation(semanticModel, awaitExpression, syntax, isImplicit),
            BoundWhileStatement whileStatement => new WhileLoopOperation(semanticModel, whileStatement, syntax, isImplicit),
            BoundForStatement forStatement => new ForLoopOperation(semanticModel, forStatement, syntax, isImplicit),
            BoundInvocationExpression invocation => new InvocationOperation(semanticModel, invocation, syntax, isImplicit),
            BoundObjectCreationExpression creation => new ObjectCreationOperation(semanticModel, creation, syntax, isImplicit),
            BoundAssignmentExpression assignmentExpression => new AssignmentOperation(semanticModel, assignmentExpression, syntax, assignmentExpression.Type, isImplicit),
            BoundAssignmentStatement assignmentStatement => new AssignmentOperation(semanticModel, assignmentStatement.Expression, syntax, assignmentStatement.Expression.Type, isImplicit),
            BoundDelegateCreationExpression delegateCreation => new DelegateCreationOperation(semanticModel, delegateCreation, syntax, isImplicit),
            BoundTupleExpression tuple => new TupleOperation(semanticModel, tuple, syntax, isImplicit),
            BoundLambdaExpression lambda => new LambdaOperation(semanticModel, lambda, syntax, isImplicit),
            BoundAddressOfExpression addressOf => new AddressOfOperation(semanticModel, addressOf, syntax, isImplicit),
            BoundArrayAccessExpression arrayAccess => new ElementAccessOperation(semanticModel, arrayAccess, OperationKind.ArrayElement, syntax, isImplicit),
            BoundIndexerAccessExpression indexerAccess => new ElementAccessOperation(semanticModel, indexerAccess, OperationKind.IndexerElement, syntax, isImplicit),
            BoundIndexExpression indexExpression => new IndexOperation(semanticModel, indexExpression, syntax, isImplicit),
            BoundRangeExpression rangeExpression => new RangeOperation(semanticModel, rangeExpression, syntax, isImplicit),
            BoundTypeOfExpression typeOf => new TypeOfOperation(semanticModel, typeOf, syntax, isImplicit),
            BoundMatchExpression match => new SwitchOperation(semanticModel, match, syntax, isImplicit),
            BoundIsPatternExpression isPattern => new IsPatternOperation(semanticModel, isPattern, syntax, isImplicit),
            BoundCasePattern casePattern => new CasePatternOperation(semanticModel, casePattern, syntax, isImplicit),
            BoundDeclarationPattern declarationPattern => new DeclarationPatternOperation(semanticModel, declarationPattern, syntax, isImplicit),
            BoundConstantPattern constantPattern => new ConstantPatternOperation(semanticModel, constantPattern, syntax, isImplicit),
            BoundPositionalPattern tuplePattern => new PositionalPatternOperation(semanticModel, tuplePattern, syntax, isImplicit),
            BoundDiscardPattern discardPattern => new DiscardPatternOperation(semanticModel, discardPattern, syntax, isImplicit),
            BoundNotPattern notPattern => new NotPatternOperation(semanticModel, notPattern, syntax, isImplicit),
            BoundAndPattern andPattern => new AndPatternOperation(semanticModel, andPattern, syntax, isImplicit),
            BoundOrPattern orPattern => new OrPatternOperation(semanticModel, orPattern, syntax, isImplicit),
            BoundSingleVariableDesignator singleVariableDesignator => new SingleVariableDesignatorOperation(semanticModel, singleVariableDesignator, syntax, isImplicit),
            BoundDiscardDesignator discardDesignator => new DiscardDesignatorOperation(semanticModel, discardDesignator, syntax, isImplicit),
            BoundCollectionExpression collection => new CollectionOperation(semanticModel, collection, syntax, isImplicit),
            BoundEmptyCollectionExpression emptyCollection => new EmptyCollectionOperation(semanticModel, emptyCollection, syntax, isImplicit),
            BoundSpreadElement spreadElement => new SpreadElementOperation(semanticModel, spreadElement, syntax, isImplicit),
            BoundTypeExpression typeExpression => new TypeOperation(semanticModel, typeExpression, syntax, isImplicit),
            BoundNamespaceExpression namespaceExpression => new NamespaceOperation(semanticModel, namespaceExpression, syntax, isImplicit),
            BoundSelfExpression self => new SelfOperation(semanticModel, self, syntax, isImplicit),
            BoundTryStatement tryStatement => new TryOperation(semanticModel, tryStatement, syntax, isImplicit),
            BoundCatchClause catchClause => new CatchClauseOperation(semanticModel, catchClause, syntax, isImplicit),
            BoundUnitExpression unit => new UnitOperation(semanticModel, unit, syntax, isImplicit),
            BoundMemberAccessExpression memberAccess => kind == OperationKind.None
                ? new SimpleOperation(semanticModel, kind, syntax, type, isImplicit)
                : new MemberReferenceOperation(semanticModel, memberAccess, kind, syntax, isImplicit),
            BoundErrorExpression error => new InvalidOperation(semanticModel, error, syntax, isImplicit),
            _ => new SimpleOperation(semanticModel, kind, syntax, type, isImplicit)
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
            BoundYieldReturnStatement => OperationKind.YieldReturn,
            BoundYieldBreakStatement => OperationKind.YieldBreak,
            BoundThrowStatement => OperationKind.Throw,
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
                IFieldSymbol => OperationKind.FieldReference,
                IPropertySymbol => OperationKind.PropertyReference,
                IMethodSymbol => OperationKind.MethodReference,
                _ => OperationKind.None
            },
            BoundUnaryExpression => OperationKind.Unary,
            BoundBinaryExpression => OperationKind.Binary,
            BoundParenthesizedExpression => OperationKind.Parenthesized,
            BoundCastExpression => OperationKind.Conversion,
            BoundAsExpression => OperationKind.Conversion,
            BoundConditionalAccessExpression => OperationKind.ConditionalAccess,
            BoundTryExpression => OperationKind.TryExpression,
            BoundAwaitExpression => OperationKind.Await,
            BoundIfStatement => OperationKind.Conditional,
            BoundIfExpression => OperationKind.Conditional,
            BoundWhileStatement => OperationKind.WhileLoop,
            BoundForStatement => OperationKind.ForLoop,
            BoundInvocationExpression => OperationKind.Invocation,
            BoundObjectCreationExpression => OperationKind.ObjectCreation,
            BoundAssignmentExpression => OperationKind.Assignment,
            BoundAssignmentStatement => OperationKind.Assignment,
            BoundDelegateCreationExpression => OperationKind.DelegateCreation,
            BoundTupleExpression => OperationKind.Tuple,
            BoundAddressOfExpression => OperationKind.AddressOf,
            BoundArrayAccessExpression => OperationKind.ArrayElement,
            BoundIndexerAccessExpression => OperationKind.IndexerElement,
            BoundIndexExpression => OperationKind.Index,
            BoundRangeExpression => OperationKind.Range,
            BoundTypeOfExpression => OperationKind.TypeOf,
            BoundLambdaExpression => OperationKind.Lambda,
            BoundMatchExpression => OperationKind.Switch,
            BoundIsPatternExpression => OperationKind.IsPattern,
            BoundCasePattern => OperationKind.CasePattern,
            BoundDeclarationPattern => OperationKind.DeclarationPattern,
            BoundConstantPattern => OperationKind.ConstantPattern,
            BoundPositionalPattern => OperationKind.PositionalPattern,
            BoundDiscardPattern => OperationKind.DiscardPattern,
            BoundNotPattern => OperationKind.NotPattern,
            BoundAndPattern => OperationKind.AndPattern,
            BoundOrPattern => OperationKind.OrPattern,
            BoundSingleVariableDesignator => OperationKind.SingleVariableDesignator,
            BoundDiscardDesignator => OperationKind.DiscardDesignator,
            BoundCollectionExpression => OperationKind.Collection,
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
