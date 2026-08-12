namespace Raven.CodeAnalysis;

internal partial class BoundContinueExpression : BoundExpression
{
    public BoundContinueExpression(ILabelSymbol? targetLabel, ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        TargetLabel = targetLabel;
    }

    public ILabelSymbol? TargetLabel { get; }
}
