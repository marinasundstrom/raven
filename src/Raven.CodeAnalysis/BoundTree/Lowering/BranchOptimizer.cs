using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// Removes unconditional branches that target the immediately following
/// label and therefore have the same behavior as fallthrough.
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
        var statements = rewritten.Statements.ToImmutableArray();
        if (statements.Length < 2)
            return rewritten;

        var keep = new bool[statements.Length];
        Array.Fill(keep, true);
        var changed = false;

        for (var index = 0; index < statements.Length - 1; index++)
        {
            if (statements[index] is BoundGotoStatement @goto &&
                statements[index + 1] is BoundLabeledStatement labeled &&
                ReferenceEquals(@goto.Target, labeled.Label))
            {
                keep[index] = false;
                changed = true;
            }
        }

        if (!changed)
            return rewritten;

        return rewritten.Update(
            statements.Where((_, index) => keep[index]),
            rewritten.LocalsToDispose,
            rewritten.IntroduceILScope);
    }
}
