namespace Raven.CodeAnalysis;

internal partial class BoundTypeExpression : BoundExpression
{
    public ITypeSymbol TypeSymbol { get; }

    public BoundTypeExpression(ITypeSymbol typeSymbol)
        : base(typeSymbol, typeSymbol)
    {
        TypeSymbol = typeSymbol;
    }
}