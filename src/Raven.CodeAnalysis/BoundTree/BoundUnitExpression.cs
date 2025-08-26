namespace Raven.CodeAnalysis;

internal partial class BoundUnitExpression : BoundExpression
{
    public BoundUnitExpression(ITypeSymbol unitType, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(unitType, null, reason)
    {

    }

    public ITypeSymbol UnitType => Type;
}