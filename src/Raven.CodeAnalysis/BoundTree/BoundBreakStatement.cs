namespace Raven.CodeAnalysis;

internal partial class BoundBreakStatement : BoundStatement
{
    public BoundBreakStatement(LabelSymbol? targetLabel = null)
    {
        TargetLabel = targetLabel;
    }

    public LabelSymbol? TargetLabel { get; }
}
