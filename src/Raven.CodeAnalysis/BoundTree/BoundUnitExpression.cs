namespace Raven.CodeAnalysis;

internal sealed partial class BoundUnitExpression : BoundExpression
{
    public BoundUnitExpression(TypeSymbol unitType, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(unitType, null, reason)
    {

    }

    public TypeSymbol UnitType => Type ?? throw new System.InvalidOperationException("A bound unit expression must have a type.");
}
