namespace Raven.CodeAnalysis;

internal static class BoundBooleanFacts
{
    public static bool TryGetConstantValue(BoundExpression expression, out bool value)
    {
        expression = Unwrap(expression);

        if (expression is BoundLiteralExpression
            {
                Kind: BoundLiteralExpressionKind.TrueLiteral or BoundLiteralExpressionKind.FalseLiteral,
                Value: bool booleanValue,
            })
        {
            value = booleanValue;
            return true;
        }

        value = false;
        return false;
    }

    private static BoundExpression Unwrap(BoundExpression expression)
    {
        while (true)
        {
            switch (expression)
            {
                case BoundParenthesizedExpression parenthesized:
                    expression = parenthesized.Expression;
                    continue;
                case BoundConversionExpression { Conversion.IsIdentity: true } conversion:
                    expression = conversion.Expression;
                    continue;
                case BoundAsExpression { Conversion.IsIdentity: true } asExpression:
                    expression = asExpression.Expression;
                    continue;
                default:
                    return expression;
            }
        }
    }
}
