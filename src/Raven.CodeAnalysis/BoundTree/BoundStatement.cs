namespace Raven.CodeAnalysis;

abstract class BoundStatement : BoundNode
{
    public virtual ISymbol Symbol { get; }
}