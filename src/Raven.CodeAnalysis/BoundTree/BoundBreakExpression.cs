namespace Raven.CodeAnalysis;

internal sealed partial class BoundBreakExpression : BoundExpression
{
    public BoundBreakExpression(LabelSymbol? targetLabel, TypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        TargetLabel = targetLabel;
    }

    public LabelSymbol? TargetLabel { get; }
}
