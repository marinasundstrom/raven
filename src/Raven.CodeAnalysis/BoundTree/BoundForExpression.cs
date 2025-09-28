namespace Raven.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;

internal partial class BoundForExpression : BoundExpression
{
    public ILocalSymbol Local { get; }
    public BoundExpression Collection { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol UnitType { get; }

    public BoundForExpression(ILocalSymbol local, BoundExpression collection, BoundExpression body, ITypeSymbol unitType)
        : base(unitType, null, BoundExpressionReason.None)
    {
        Local = local;
        Collection = collection;
        Body = body;
        UnitType = unitType;
    }
}
