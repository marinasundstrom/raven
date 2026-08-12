namespace Raven.CodeAnalysis;

internal partial class BoundBreakExpression : BoundExpression
{
    public BoundBreakExpression(ILabelSymbol? targetLabel, ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        TargetLabel = targetLabel;
    }

    public ILabelSymbol? TargetLabel { get; }
}
