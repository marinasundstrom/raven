namespace Raven.CodeAnalysis;

abstract class BoundNode
{

}

abstract class BoundExpression : BoundNode
{
    public abstract ITypeSymbol Type { get; }
}

internal class TypeSymbol
{
    public static ITypeSymbol Int { get; internal set; }
    public static ITypeSymbol String { get; internal set; }
}
