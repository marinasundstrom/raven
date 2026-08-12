namespace Raven.CodeAnalysis;

/// <summary>
/// Simplifies control flow whose condition is already a Boolean literal.
/// </summary>
internal sealed class ControlFlowOptimizer : BoundTreeRewriter
{
    private ControlFlowOptimizer()
    {
    }

    public static BoundNode Rewrite(BoundNode node)
        => new ControlFlowOptimizer().Visit(node)!;

    public override BoundNode? VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
    {
        var rewritten = (BoundConditionalGotoStatement)base.VisitConditionalGotoStatement(node)!;
        if (!TryGetBooleanValue(rewritten.Condition, out var condition))
            return rewritten;

        return condition == rewritten.JumpIfTrue
            ? new BoundGotoStatement(rewritten.Target)
            : CreateEmptyStatement();
    }

    public override BoundNode? VisitIfExpression(BoundIfExpression node)
    {
        var rewritten = (BoundIfExpression)base.VisitIfExpression(node)!;
        if (!TryGetBooleanValue(rewritten.Condition, out var condition))
            return rewritten;

        if (condition)
            return rewritten.ThenBranch;

        return rewritten.ElseBranch ?? rewritten;
    }

    public override BoundNode? VisitIfStatement(BoundIfStatement node)
    {
        var rewritten = (BoundIfStatement)base.VisitIfStatement(node)!;
        if (!TryGetBooleanValue(rewritten.Condition, out var condition))
            return rewritten;

        if (condition)
            return rewritten.ThenNode;

        return rewritten.ElseNode ?? CreateEmptyStatement();
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

    private static BoundBlockStatement CreateEmptyStatement()
        => new([], introduceILScope: false);
}
