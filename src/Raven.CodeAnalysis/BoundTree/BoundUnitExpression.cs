namespace Raven.CodeAnalysis;

internal partial class BoundUnitExpression : BoundExpression
{
    public BoundUnitExpression(TypeSymbol unitType, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(unitType, null, reason)
    {

    }

    public TypeSymbol UnitType => Type;
}
