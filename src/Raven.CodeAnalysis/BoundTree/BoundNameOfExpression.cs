namespace Raven.CodeAnalysis;

internal partial class BoundNameOfExpression : BoundExpression
{
    public BoundNameOfExpression(ISymbol symbol, ITypeSymbol stringType)
        : base(stringType, symbol)
    {
        Name = symbol.Name;
        StringType = stringType;
    }

    public string Name { get; }
    public ITypeSymbol StringType { get; }

    public override string ToString()
    {
        return Name;
    }
}
