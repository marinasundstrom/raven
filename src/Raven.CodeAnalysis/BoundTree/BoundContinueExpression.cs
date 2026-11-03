namespace Raven.CodeAnalysis;

internal partial class BoundContinueExpression : BoundExpression
{
    public BoundContinueExpression(LabelSymbol? targetLabel, TypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        TargetLabel = targetLabel;
    }

    public LabelSymbol? TargetLabel { get; }
}
