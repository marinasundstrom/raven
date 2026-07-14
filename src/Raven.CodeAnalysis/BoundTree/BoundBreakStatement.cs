namespace Raven.CodeAnalysis;

internal partial class BoundBreakStatement : BoundStatement
{
    public BoundBreakStatement(ILabelSymbol? targetLabel = null)
    {
        TargetLabel = targetLabel;
    }

    public ILabelSymbol? TargetLabel { get; }
}
