namespace Raven.CodeAnalysis;

/// <summary>
/// Simplifies pattern algebra while preserving pattern evaluation order and
/// designator side effects.
/// </summary>
internal sealed class PatternOptimizer : BoundTreeRewriter
{
    private PatternOptimizer()
    {
    }

    public static BoundNode Rewrite(BoundNode node)
        => new PatternOptimizer().Visit(node)!;

    public override BoundNode? VisitAndPattern(BoundAndPattern node)
    {
        var rewritten = (BoundAndPattern)base.VisitAndPattern(node)!;

        return rewritten switch
        {
            { Left: BoundDiscardPattern, Right: var right } => right,
            { Left: var left, Right: BoundDiscardPattern } => left,
            _ => rewritten,
        };
    }

    public override BoundNode? VisitNotPattern(BoundNotPattern node)
    {
        var rewritten = (BoundNotPattern)base.VisitNotPattern(node)!;
        return rewritten.Pattern is BoundNotPattern nested
            ? nested.Pattern
            : rewritten;
    }

    public override BoundNode? VisitOrPattern(BoundOrPattern node)
    {
        var rewritten = (BoundOrPattern)base.VisitOrPattern(node)!;
        return rewritten.Left is BoundDiscardPattern discard
            ? discard
            : rewritten;
    }
}
