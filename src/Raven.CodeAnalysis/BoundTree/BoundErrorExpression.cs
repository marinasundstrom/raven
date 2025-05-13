namespace Raven.CodeAnalysis;

internal class BoundErrorExpression : BoundExpression
{
    private readonly INamedTypeSymbol _namedTypeSymbol;

    public BoundErrorExpression(INamedTypeSymbol namedTypeSymbol)
    {
        _namedTypeSymbol = namedTypeSymbol;
    }

    public override ITypeSymbol Type => _namedTypeSymbol;
}
