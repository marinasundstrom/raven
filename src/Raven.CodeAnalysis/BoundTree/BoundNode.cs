namespace Raven.CodeAnalysis;

closed class BoundNode
{
    public abstract void Accept(BoundTreeVisitor visitor);

    public abstract TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor);
}
