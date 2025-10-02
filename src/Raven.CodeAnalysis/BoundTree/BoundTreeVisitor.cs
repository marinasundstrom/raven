
namespace Raven.CodeAnalysis;

partial class BoundTreeVisitor
{
    public virtual void DefaultVisit(BoundNode boundNode)
    {

    }

    public virtual void Visit(BoundNode boundNode)
    {
        boundNode.Accept(this);
    }

    public virtual void VisitYieldReturnStatement(BoundYieldReturnStatement node)
    {
        DefaultVisit(node);
    }

    public virtual void VisitYieldBreakStatement(BoundYieldBreakStatement node)
    {
        DefaultVisit(node);
    }
}

partial class BoundTreeVisitor<TResult>
//where TResult : BoundNode
{
    public virtual TResult DefaultVisit(BoundNode boundNode)
    {
        return default!;
    }

    public virtual TResult Visit(BoundNode boundNode)
    {
        return boundNode.Accept(this);
    }

    public virtual TResult VisitYieldReturnStatement(BoundYieldReturnStatement node)
    {
        return DefaultVisit(node);
    }

    public virtual TResult VisitYieldBreakStatement(BoundYieldBreakStatement node)
    {
        return DefaultVisit(node);
    }
}