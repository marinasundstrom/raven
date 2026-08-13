namespace Raven.CodeAnalysis;

/// <summary>
/// Removes unconditional branches that target the immediately following
/// label and inverts local conditional-branch-over-goto shapes. This rewriter
/// is not part of the Release pipeline until it can preserve scope-exit
/// disposal semantics.
/// </summary>
internal sealed class BranchOptimizer : BoundTreeRewriter
{
    private BranchOptimizer()
    {
    }

    public static BoundNode Rewrite(BoundNode node)
        => new BranchOptimizer().Visit(node)!;

    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        var rewritten = (BoundBlockStatement)base.VisitBlockStatement(node)!;
        var statements = rewritten.Statements.ToArray();
        if (statements.Length < 2)
            return rewritten;

        var optimized = new List<BoundStatement>(statements.Length);
        var changed = false;

        for (var index = 0; index < statements.Length; index++)
        {
            if (index + 2 < statements.Length &&
                statements[index] is BoundConditionalGotoStatement conditional &&
                statements[index + 1] is BoundGotoStatement @goto &&
                statements[index + 2] is BoundLabeledStatement labeled &&
                ReferenceEquals(conditional.Target, labeled.Label))
            {
                optimized.Add(new BoundConditionalGotoStatement(
                    @goto.Target,
                    conditional.Condition,
                    !conditional.JumpIfTrue));
                index++;
                changed = true;
                continue;
            }

            optimized.Add(statements[index]);
        }

        for (var index = optimized.Count - 2; index >= 0; index--)
        {
            if (optimized[index] is BoundGotoStatement @goto &&
                optimized[index + 1] is BoundLabeledStatement labeled &&
                ReferenceEquals(@goto.Target, labeled.Label))
            {
                optimized.RemoveAt(index);
                changed = true;
            }
        }

        if (!changed)
            return rewritten;

        return rewritten.Update(
            optimized,
            rewritten.LocalsToDispose,
            rewritten.IntroduceILScope);
    }
}
