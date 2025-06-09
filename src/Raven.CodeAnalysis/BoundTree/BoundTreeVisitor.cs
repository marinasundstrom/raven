
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
}