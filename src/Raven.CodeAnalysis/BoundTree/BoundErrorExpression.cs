namespace Raven.CodeAnalysis;

internal class BoundErrorExpression : BoundExpression
{
    private INamedTypeSymbol _namedTypeSymbol;

    public BoundErrorExpression(INamedTypeSymbol namedTypeSymbol)
    {
        _namedTypeSymbol = namedTypeSymbol;
    }

    public override ITypeSymbol Type => throw new NotImplementedException();
}
