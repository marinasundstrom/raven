namespace Raven.CodeAnalysis;

// Run before control-flow or async rewriting can spill argument/receiver effects.
internal sealed class ConditionalCallRewriter : BoundTreeRewriter
{
    internal static T Rewrite<T>(T node) where T : BoundNode
        => (T)new ConditionalCallRewriter().Visit(node)!;

    public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
        => node.IsConditionallyOmitted
            ? new BoundUnitExpression(node.Type!)
            : base.VisitInvocationExpression(node);

    public override BoundNode? VisitConditionalAccessExpression(BoundConditionalAccessExpression node)
        => node.WhenNotNull is BoundInvocationExpression { IsConditionallyOmitted: true } call
            ? new BoundUnitExpression(call.Type!)
            : base.VisitConditionalAccessExpression(node);
}
