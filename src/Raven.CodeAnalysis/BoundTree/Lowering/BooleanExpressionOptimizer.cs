namespace Raven.CodeAnalysis;

/// <summary>
/// Simplifies built-in Boolean expressions when a literal operand determines
/// the result without changing which non-literal operands are evaluated.
/// </summary>
internal sealed class BooleanExpressionOptimizer : BoundTreeRewriter
{
    private BooleanExpressionOptimizer()
    {
    }

    public static BoundNode Rewrite(BoundNode node)
        => new BooleanExpressionOptimizer().Visit(node)!;

    public override BoundNode? VisitBinaryExpression(BoundBinaryExpression node)
    {
        var rewritten = (BoundBinaryExpression)base.VisitBinaryExpression(node)!;
        if (rewritten.Operator.MethodSymbol is not null)
            return rewritten;

        if (TryGetBooleanValue(rewritten.Left, out var left))
        {
            return rewritten.Operator.OperatorKind switch
            {
                BinaryOperatorKind.LogicalAnd => left ? rewritten.Right : rewritten.Left,
                BinaryOperatorKind.LogicalOr => left ? rewritten.Left : rewritten.Right,
                _ => rewritten,
            };
        }

        if (TryGetBooleanValue(rewritten.Right, out var right))
        {
            return rewritten.Operator.OperatorKind switch
            {
                BinaryOperatorKind.LogicalAnd when right => rewritten.Left,
                BinaryOperatorKind.LogicalOr when !right => rewritten.Left,
                _ => rewritten,
            };
        }

        return rewritten;
    }

    public override BoundNode? VisitUnaryExpression(BoundUnaryExpression node)
    {
        var rewritten = (BoundUnaryExpression)base.VisitUnaryExpression(node)!;
        if (rewritten.Operator.OperatorKind != BoundUnaryOperatorKind.LogicalNot ||
            !TryGetBooleanValue(rewritten.Operand, out var operand))
        {
            return rewritten;
        }

        return new BoundLiteralExpression(
            operand ? BoundLiteralExpressionKind.FalseLiteral : BoundLiteralExpressionKind.TrueLiteral,
            !operand,
            rewritten.Type!);
    }

    private static bool TryGetBooleanValue(BoundExpression expression, out bool value)
    {
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
}
