namespace Raven.CodeAnalysis;

internal partial class BoundTypeExpression : BoundExpression
{
    public TypeSymbol TypeSymbol { get; }

    public BoundTypeExpression(TypeSymbol typeSymbol)
        : base(typeSymbol, typeSymbol)
    {
        TypeSymbol = typeSymbol;
    }
}
