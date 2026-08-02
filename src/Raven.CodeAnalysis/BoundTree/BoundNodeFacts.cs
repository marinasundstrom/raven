namespace Raven.CodeAnalysis;

internal static class BoundNodeFacts
{
    public static bool IsAbruptExpression(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundReturnExpression:
            case BoundThrowExpression:
                return true;
            case BoundRequiredResultExpression requiredResult:
                return IsAbruptExpression(requiredResult.Operand);
            case BoundParenthesizedExpression parenthesized:
                return IsAbruptExpression(parenthesized.Expression);
            case BoundConversionExpression conversion:
                return IsAbruptExpression(conversion.Expression);
            case BoundUnaryExpression unary:
                return IsAbruptExpression(unary.Operand);
            case BoundBinaryExpression binary:
                if (IsAbruptExpression(binary.Left))
                    return true;

                var operatorKind = binary.Operator.OperatorKind &
                    ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);
                return operatorKind is not (BinaryOperatorKind.LogicalAnd or BinaryOperatorKind.LogicalOr) &&
                    IsAbruptExpression(binary.Right);
            case BoundMemberAccessExpression { Receiver: { } receiver }:
                return IsAbruptExpression(receiver);
            case BoundPointerMemberAccessExpression pointerMemberAccess:
                return IsAbruptExpression(pointerMemberAccess.PointerReceiver);
            case BoundInvocationExpression invocation:
                return invocation.Receiver is not null && IsAbruptExpression(invocation.Receiver) ||
                    invocation.ExtensionReceiver is not null && IsAbruptExpression(invocation.ExtensionReceiver) ||
                    invocation.Arguments.Any(IsAbruptExpression);
            case BoundObjectCreationExpression objectCreation:
                return objectCreation.Receiver is not null && IsAbruptExpression(objectCreation.Receiver) ||
                    objectCreation.Arguments.Any(IsAbruptExpression) ||
                    ObjectInitializerIsAbrupt(objectCreation.Initializer);
            case BoundArrayAccessExpression arrayAccess:
                return IsAbruptExpression(arrayAccess.Receiver) ||
                    arrayAccess.Indices.Any(IsAbruptExpression);
            case BoundIndexerAccessExpression indexerAccess:
                return IsAbruptExpression(indexerAccess.Receiver) ||
                    indexerAccess.Arguments.Any(IsAbruptExpression);
            case BoundIndexExpression index:
                return IsAbruptExpression(index.Value);
            case BoundAssignmentExpression assignment:
                return IsAbruptExpression(assignment.Left) ||
                    IsAbruptExpression(assignment.Right);
            case BoundConditionalAccessExpression conditionalAccess:
                return IsAbruptExpression(conditionalAccess.Receiver);
            case BoundIfExpression { ElseBranch: not null } ifExpression:
                return IsAbruptExpression(ifExpression.Condition) ||
                    IsAbruptExpression(ifExpression.ThenBranch) &&
                    IsAbruptExpression(ifExpression.ElseBranch);
            case BoundMatchExpression { Arms.IsDefaultOrEmpty: false } matchExpression:
                return IsAbruptExpression(matchExpression.Expression) ||
                    matchExpression.Arms.All(static arm => IsAbruptExpression(arm.Expression));
            case BoundBlockExpression block:
                {
                    var last = block.Statements.LastOrDefault();
                    if (last is BoundReturnStatement or BoundThrowStatement)
                        return true;
                    if (last is BoundExpressionStatement expressionStatement)
                        return IsAbruptExpression(expressionStatement.Expression);
                    return false;
                }
            default:
                return false;
        }
    }

    private static bool ObjectInitializerIsAbrupt(BoundObjectInitializer? initializer)
    {
        if (initializer is null)
            return false;

        return initializer.Entries.Any(static entry => entry switch
        {
            BoundObjectInitializerAssignmentEntry assignment => IsAbruptExpression(assignment.Value),
            BoundObjectInitializerExpressionEntry expression => IsAbruptExpression(expression.Expression),
            _ => false,
        });
    }

    public static bool MatchArmGuardGuaranteesMatch(BoundExpression? guard)
    {
        if (guard is null)
            return true;

        return TryEvaluateBooleanConstant(guard) == true;
    }

    private static bool? TryEvaluateBooleanConstant(BoundExpression expression)
    {
        expression = UnwrapBooleanExpression(expression);

        if (expression is BoundLiteralExpression literal)
        {
            return literal.Kind switch
            {
                BoundLiteralExpressionKind.TrueLiteral => true,
                BoundLiteralExpressionKind.FalseLiteral => false,
                _ when literal.Value is bool value => value,
                _ => null,
            };
        }

        return null;
    }

    private static BoundExpression UnwrapBooleanExpression(BoundExpression expression)
    {
        while (true)
        {
            switch (expression)
            {
                case BoundParenthesizedExpression parenthesized:
                    expression = parenthesized.Expression;
                    continue;
                case BoundConversionExpression cast when cast.Conversion.IsIdentity:
                    expression = cast.Expression;
                    continue;
                case BoundAsExpression asExpression when asExpression.Conversion.IsIdentity:
                    expression = asExpression.Expression;
                    continue;
            }

            return expression;
        }
    }
}
