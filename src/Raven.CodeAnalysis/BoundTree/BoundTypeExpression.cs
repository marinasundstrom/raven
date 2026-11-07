namespace Raven.CodeAnalysis;

internal sealed partial class BoundTypeExpression : BoundExpression
{
    public TypeSymbol TypeSymbol { get; }

    public BoundTypeExpression(TypeSymbol typeSymbol)
        : base(typeSymbol, typeSymbol)
    {
        TypeSymbol = typeSymbol;
    }
}
