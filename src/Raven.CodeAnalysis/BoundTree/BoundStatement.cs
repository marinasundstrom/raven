namespace Raven.CodeAnalysis;

abstract class BoundStatement : BoundNode
{
    public virtual Symbol? Symbol => null;
}
