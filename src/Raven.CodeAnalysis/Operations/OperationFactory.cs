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
            BoundVariableDeclarator => OperationKind.VariableDeclarator,
            BoundReturnStatement => OperationKind.Return,
            BoundLiteralExpression => OperationKind.Literal,
            BoundLocalAccess => OperationKind.LocalReference,
            BoundParameterAccess => OperationKind.ParameterReference,
            BoundFieldAccess => OperationKind.FieldReference,
            BoundPropertyAccess => OperationKind.PropertyReference,
            BoundMethodGroupExpression => OperationKind.MethodReference,
            BoundUnaryExpression => OperationKind.Unary,
            BoundBinaryExpression => OperationKind.Binary,
            BoundParenthesizedExpression => OperationKind.Parenthesized,
            BoundCastExpression => OperationKind.Conversion,
            BoundAsExpression => OperationKind.Conversion,
            BoundConditionalAccessExpression => OperationKind.ConditionalAccess,
            BoundIfStatement => OperationKind.Conditional,
            BoundIfExpression => OperationKind.Conditional,
            BoundWhileStatement => OperationKind.WhileLoop,
            BoundWhileExpression => OperationKind.WhileLoop,
            BoundForStatement => OperationKind.ForLoop,
            BoundForExpression => OperationKind.ForLoop,
            BoundInvocationExpression => OperationKind.Invocation,
            BoundObjectCreationExpression => OperationKind.ObjectCreation,
            BoundAssignmentExpression => OperationKind.Assignment,
            BoundAssignmentStatement => OperationKind.Assignment,
            BoundDelegateCreationExpression => OperationKind.DelegateCreation,
            BoundTupleExpression => OperationKind.Tuple,
            BoundTypeOfExpression => OperationKind.TypeOf,
            BoundLambdaExpression => OperationKind.Lambda,
            BoundMatchExpression => OperationKind.Switch,
            BoundCollectionExpression => OperationKind.Collection,
            BoundTypeExpression => OperationKind.TypeExpression,
            BoundUnitExpression => OperationKind.Unit,
            BoundErrorExpression => OperationKind.Invalid,
            _ => OperationKind.None
        };
    }
}
