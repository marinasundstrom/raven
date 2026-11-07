namespace Raven.CodeAnalysis;

internal sealed partial class BoundBreakStatement : BoundStatement
{
    public BoundBreakStatement(LabelSymbol? targetLabel = null)
    {
        TargetLabel = targetLabel;
    }

    public LabelSymbol? TargetLabel { get; }
}
