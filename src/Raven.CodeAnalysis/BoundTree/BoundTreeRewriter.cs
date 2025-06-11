using System.Linq.Expressions;

namespace Raven.CodeAnalysis;

abstract partial class BoundTreeRewriter : BoundTreeVisitor<BoundNode?>
{
    public override BoundNode? Visit(BoundNode? node)
    {
        return node?.Accept(this);
    }

    public virtual IEnumerable<T> VisitList<T>(IEnumerable<T> nodes)
        where T : BoundNode
    {
        foreach (var node in nodes)
            yield return (T)node.Accept(this)!;
    }

    public virtual BoundExpression VisitExpression(BoundExpression node)
    {
        switch (node)
        {
            case BoundLiteralExpression lit:
                VisitLiteralExpression(lit);
                break;
            case BoundLocalAccess local:
                VisitLocalAccess(local);
                break;
            case BoundParameterAccess par:
                VisitParameterAccess(par);
                break;
            case BoundBinaryExpression bin:
                VisitBinaryExpression(bin);
                break;
            case BoundInvocationExpression call:
                VisitInvocationExpression(call);
                break;
            case BoundLambdaExpression lambda:
                VisitLambdaExpression(lambda);
                break;
            case BoundBlockExpression block:
                VisitBlockExpression(block);
                break;
        }

        throw new NotImplementedException($"Unhandled expression: {node.GetType().Name}");
    }
}