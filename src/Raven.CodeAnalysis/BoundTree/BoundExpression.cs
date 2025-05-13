namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public abstract ITypeSymbol Type { get; }
}
