namespace Raven.CodeAnalysis;

internal partial class BoundNameOfExpression : BoundExpression
{
    public BoundNameOfExpression(Symbol symbol, TypeSymbol stringType)
        : base(stringType, symbol)
    {
        Name = symbol.Name;
        StringType = stringType;
    }

    public string Name { get; }
    public TypeSymbol StringType { get; }

    public override string ToString()
    {
        return Name;
    }
}
