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

        return new SimpleOperation(semanticModel, kind, syntax, type, isImplicit);
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
            BoundThrowStatement => OperationKind.Throw,
            BoundBreakStatement => OperationKind.Break,
            BoundContinueStatement => OperationKind.Continue,
            BoundGotoStatement => OperationKind.Goto,
            BoundLabeledStatement => OperationKind.Labeled,
            BoundLiteralExpression => OperationKind.Literal,
            BoundLocalAccess => OperationKind.LocalReference,
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
            BoundTypeOfExpression => OperationKind.TypeOf,
            BoundLambdaExpression => OperationKind.Lambda,
            BoundMatchExpression => OperationKind.Switch,
            BoundCollectionExpression => OperationKind.Collection,
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
