namespace Raven.CodeAnalysis;

internal static class BoundNodeFacts
{
    public static bool IsAbruptExpression(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundReturnExpression:
            case BoundThrowExpression:
            case BoundRequiredResultExpression { Operand: BoundReturnExpression }:
            case BoundRequiredResultExpression { Operand: BoundThrowExpression }:
                return true;
            case BoundParenthesizedExpression parenthesized:
                return IsAbruptExpression(parenthesized.Expression);
            case BoundConversionExpression conversion:
                return IsAbruptExpression(conversion.Expression);
            case BoundIfExpression { ElseBranch: not null } ifExpression:
                return IsAbruptExpression(ifExpression.ThenBranch) &&
                    IsAbruptExpression(ifExpression.ElseBranch);
            case BoundMatchExpression { Arms.IsDefaultOrEmpty: false } matchExpression:
                return matchExpression.Arms.All(static arm => IsAbruptExpression(arm.Expression));
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
